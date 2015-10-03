/*
 * ForwardChainer.cc
 *
 * Copyright (C) 2014,2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <boost/range/algorithm/find.hpp>

#include <opencog/atoms/bind/BindLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/query/BindLinkAPI.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/rule-engine/Rule.h>
#include <opencog/util/Logger.h>

#include "ForwardChainer.h"
#include "FocusSetPMCB.h"
#include "VarGroundingPMCB.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& as, Handle rbs, Handle hsource,
                               HandleSeq focus_set) :
        _as(as), _rec(as), _rbs(rbs), _configReader(as, rbs)
{
    init(hsource,focus_set);
}

ForwardChainer::~ForwardChainer()
{

}

void ForwardChainer::init(Handle hsource, HandleSeq focus_set)
{
     validate(hsource, focus_set);

    _search_in_af = _configReader.get_attention_allocation();
    _search_focus_Set = not focus_set.empty();
    _ts_mode = TV_FITNESS_BASED;

    //Set potential source.
    HandleSeq init_sources = { };
    //Accept set of initial sources wrapped in a SET_LINK
    if (LinkCast(hsource) and hsource->getType() == SET_LINK) {
        init_sources = _as.get_outgoing(hsource);
    } else {
        init_sources.push_back(hsource);
    }
    update_potential_sources(init_sources);

     //Set rules.
     for(Rule& r :_configReader.get_rules())
     {
         _rules.push_back(&r);
     }
    _cur_rule = nullptr;
    
    // Provide a logger
    _log = NULL;
    setLogger(new opencog::Logger("forward_chainer.log", Logger::FINE, true));
}

void ForwardChainer::setLogger(Logger* log)
{
    if (_log)
        delete _log;
    _log = log;
}

Logger* ForwardChainer::getLogger()
{
    return _log;
}

/**
 * Does one step forward chaining and stores result.
 *
 */
void ForwardChainer::do_step(void)
{
    _cur_source=choose_next_source();
    _log->debug("[ForwardChainer] Next source %s", _cur_source->toString().c_str());

    HandleSeq derived_rhandles;

    //choose a rule that source unifies with one of its premises.
    //if not found try to find by matching suba-toms of the rules
    //premises.
    Rule *rule = choose_rule(_cur_source, false);
    if (rule) {
        _cur_rule = rule;
        derived_rhandles = derive_rules(_cur_source, rule);

    } else {
        rule = choose_rule(_cur_source, true);
        if (rule) {
            _cur_rule = rule;
            derived_rhandles = derive_rules(_cur_source, rule,
            true);
        }
    }

    _log->debug( "Derived rule size = %d", derived_rhandles.size());

    HandleSeq products;
    //Applying all partial/full groundings.
    for (Handle rhandle : derived_rhandles) {
        HandleSeq hs;
        //Check for fully grounded outputs returned by derive_rules.
        if (not contains_atomtype(rhandle, VARIABLE_NODE)) {
            Instantiator inst(&_as);
            hs.push_back(inst.instantiate(rhandle, { }));

        } else {
            hs = apply_rule(rhandle, _search_focus_Set);
        }

        products.insert(products.end(), hs.begin(), hs.end());
    }

    //Finally store source partial groundings and inference results.
    if (not derived_rhandles.empty()) {
        _fcstat.add_partial_grounding(_cur_source, rule->get_handle(),
                              derived_rhandles);
        _fcstat.add_inference_record(
                _cur_source, HandleSeq(products.begin(), products.end()));
    }

}

void ForwardChainer::do_chain(void)
{
    //Relex2Logic uses this.TODO make a separate class
    //to handle this robustly.
    if(_potential_sources.empty())
    {
        apply_all_rules(_search_focus_Set);
        return;
    }

    auto max_iter = _configReader.get_maximum_iterations();

    while (_iteration < max_iter /*OR other termination criteria*/) {
        _log->debug("Iteration %d", _iteration);
         do_step();
        _iteration++;
    }

    _log->debug("[ForwardChainer] finished forwarch chaining.");
}

/**
 * Does pattern matching for a variable containing query.
 *
 * @param source    A variable containing handle passed as an input to the pattern matcher
 * @param var_nodes The VariableNodes in @param hsource
 *
 */
void ForwardChainer::do_pm(const Handle& hsource,
                           const UnorderedHandleSet& var_nodes)
{
    DefaultImplicator impl(&_as);
    impl.implicand = hsource;
    HandleSeq vars;
    for (auto h : var_nodes)
        vars.push_back(h);
    _cur_source = hsource;
    Handle hvar_list = _as.add_link(VARIABLE_LIST, vars);
    Handle hclause = _as.add_link(AND_LINK, hsource);

    // Run the pattern matcher, find all patterns that satisfy the
    // the clause, with the given variables in it.
    PatternLinkPtr sl(createPatternLink(hvar_list, hclause));
    sl->satisfy(impl);

    // Update result
    _fcstat.add_inference_record(Handle::UNDEFINED, impl.get_result_list());

    // Delete the AND_LINK and LIST_LINK
    _as.remove_atom(hvar_list);
    _as.remove_atom(hclause);

    //!Additionally, find applicable rules and apply.
    vector<Rule*> rules = {choose_rule(hsource,true)};
    for (Rule* rule : rules) {
        BindLinkPtr bl(BindLinkCast(rule->get_handle()));
        DefaultImplicator impl(&_as);
        impl.implicand = bl->get_implicand();
        bl->imply(impl);
        _cur_rule = rule;
        _fcstat.add_inference_record(Handle::UNDEFINED, impl.get_result_list());
    }
}

/**
 * Applies all rules in the rule base.
 *
 * @param search_focus_set flag for searching focus set.
 */
void ForwardChainer::apply_all_rules(bool search_focus_set /*= false*/)
{
    for (Rule* rule : _rules) {
        _cur_rule = rule;
        HandleSeq hs = apply_rule(rule->get_handle(), search_focus_set);

        //Update
       _fcstat.add_inference_record(Handle::UNDEFINED,hs);
        update_potential_sources(hs);
    }

}

HandleSeq ForwardChainer::get_chaining_result()
{
    return _fcstat.get_all_inferences();
}

Rule* ForwardChainer::choose_rule(Handle hsource, bool subatom_match)
{
    //TODO move this somewhere else
    std::map<Rule*, float> rule_weight;
    for (Rule* r : _rules)
        rule_weight[r] = r->get_weight();

    _log->debug("[ForwardChainer] %d rules to be searched",rule_weight.size());

    //Select a rule among the admissible rules in the rule-base via stochastic
    //selection,based on the weights of the rules in the current context.
    Rule* rule = nullptr;
    bool unifiable = false;

    if (subatom_match) {
        _log->debug("[ForwardChainer] Subatom-unifying. %s",(hsource->toShortString()).c_str());

        while (!unifiable and !rule_weight.empty()) {
            Rule* temp = _rec.tournament_select(rule_weight);

            if (subatom_unify(hsource, temp)) {
                unifiable = true;
                rule = temp;
                break;
            }
            rule_weight.erase(temp);
        }

    } else {
        _log->debug("[ForwardChainer] Unifying. %s",(hsource->toShortString()).c_str());

        while (!unifiable and !rule_weight.empty()) {
            Rule *temp = _rec.tournament_select(rule_weight);
            HandleSeq hs = temp->get_implicant_seq();

            for (Handle target : hs) {
                if (unify(hsource, target, temp)) {
                    unifiable = true;
                    rule = temp;
                    break;
                }
            }
            rule_weight.erase(temp);
        }
    }

    if(nullptr != rule)
        _log->debug("[ForwardChainer] Selected rule is %s",
                            (rule->get_handle())->toShortString().c_str());
    else
       _log->debug("[ForwardChainer] No match found.");

    return rule;
};

Handle ForwardChainer::choose_next_source()
{

    URECommons urec(_as);
    map<Handle, float> tournament_elem;

    switch (_ts_mode) {
    case TV_FITNESS_BASED:
        for (Handle s : _potential_sources)
            tournament_elem[s] = urec.tv_fitness(s);
        break;

    case STI_BASED:
        for (Handle s : _potential_sources)
            tournament_elem[s] = s->getSTI();
        break;

    default:
        throw RuntimeException(TRACE_INFO, "Unknown source selection mode.");
        break;
    }

    Handle hchosen = Handle::UNDEFINED;

    //!Prioritize new source selection.
    for (size_t i = 0; i < tournament_elem.size(); i++) {
        Handle hselected = urec.tournament_select(tournament_elem);
        bool selected_before = (boost::find(_selected_sources, hselected)
                != _selected_sources.end());

        if (selected_before) {
            continue;
        } else {
            hchosen = hselected;
            _selected_sources.push_back(hchosen);
            break;
        }
    }

    // Incase of when all sources are selected
    if (hchosen == Handle::UNDEFINED)
        return urec.tournament_select(tournament_elem);

    return hchosen;
}

HandleSeq ForwardChainer::apply_rule(Handle rhandle,bool search_in_focus_set /*=false*/)
{
    HandleSeq result;

    if (search_in_focus_set) {
        //This restricts PM to look only in the focus set
        AtomSpace focus_set_as;

        //Add focus set atoms to focus_set atomspace
        for (Handle h : _focus_set)
            focus_set_as.add_atom(h);

        //Add source atoms to focus_set atomspace
        for (Handle h : _potential_sources)
            focus_set_as.add_atom(h);

        //rhandle may introduce a new atoms that satisfies condition for the output
        //In order to prevent this undesirable effect, lets store rhandle in a child
        //atomspace of parent focus_set_as so that PM will never be able to find this
        //new undesired atom created from partial grounding.
        AtomSpace derived_rule_as(&focus_set_as);
        Handle rhcpy = derived_rule_as.add_atom(rhandle);

        BindLinkPtr bl = BindLinkCast(rhcpy);

        FocusSetPMCB fs_pmcb(&derived_rule_as, &_as);
        fs_pmcb.implicand = bl->get_implicand();

        _log->debug("Applying rule in focus set %s ",(rhcpy->toShortString()).c_str());

        bl->imply(fs_pmcb);

        result = fs_pmcb.get_result_list();

        _log->debug(
                "Result is %s ",
                ((_as.add_link(SET_LINK, result))->toShortString()).c_str());

    }
    //Search the whole atomspace
    else {
        AtomSpace derived_rule_as(&_as);

        Handle rhcpy = derived_rule_as.add_atom(rhandle);

        _log->debug("Applying rule on atomspace %s ",(rhcpy->toShortString()).c_str());

        Handle h = bindlink(&derived_rule_as,rhcpy);

        _log->debug("Result is %s ",(h->toShortString()).c_str());

        result = derived_rule_as.get_outgoing(h);
    }

    //add the results back to main atomspace
    for(Handle h:result) _as.add_atom(h);

    return result;
}

/**
 * Derives new rules by replacing variables that are unfiable in @param target
 * with source.The rule handles are not added to any atomspace.
 *
 * @param  source    A source atom that will be matched with the target.
 * @param  target    An implicant containing variable to be grounded form source.
 * @param  rule      A rule object that contains @param target in its implicant. *
 *
 * @return  A HandleSeq of derived rule handles.
 */
HandleSeq ForwardChainer::derive_rules(Handle source, Handle target,
                                               Rule* rule)
{
    //exceptions
    if (not is_valid_implicant(target))
        return {};

    HandleSeq derived_rules = { };

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(target);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(target, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    Handle h = temp_pm_as.add_link(BIND_LINK,HandleSeq { implicant_vardecl, hcpy, hcpy });
    BindLinkPtr bl = BindLinkCast(h);

    VarGroundingPMCB gcb(&temp_pm_as);
    gcb.implicand = bl->get_implicand();

    bl->imply(gcb);

    auto del_by_value =
            [] (std::vector<std::map<Handle,Handle>>& vec_map,const Handle& h) {
                for (auto& map: vec_map)
                for(auto& it:map) {if (it.second == h) map.erase(it.first);}
            };

    //We don't want VariableList atoms to ground free-vars.
    del_by_value(gcb.term_groundings, implicant_vardecl);
    del_by_value(gcb.var_groundings, implicant_vardecl);

    FindAtoms fv(VARIABLE_NODE);
    for (const auto& termg_map : gcb.term_groundings) {
        for (const auto& it : termg_map) {
            if (it.second == sourcecpy) {

                fv.search_set(it.first);

                Handle rhandle = rule->get_handle();
                HandleSeq new_candidate_rules = substitute_rule_part(
                        temp_pm_as, temp_pm_as.add_atom(rhandle), fv.varset,
                        gcb.var_groundings);

                for (Handle nr : new_candidate_rules) {
                    if (find(derived_rules.begin(), derived_rules.end(), nr) == derived_rules.end()
                            and nr != rhandle) {
                        derived_rules.push_back(nr);
                    }
                }
            }
        }
    }

    return derived_rules;
}

/**
 * Derives new rules by replacing variables in @param rule that are
 * unifiable with source.
 *
 * @param  source    A source atom that will be matched with the rule.
 * @param  rule      A rule object
 * @param  subatomic A flag that sets subatom unification.
 *
 * @return  A HandleSeq of derived rule handles.
 */
HandleSeq ForwardChainer::derive_rules(Handle source, Rule* rule,
                                               bool subatomic/*=false*/)
{
    HandleSeq derived_rules = { };

    auto add_result = [&derived_rules] (HandleSeq result) {
        derived_rules.insert(derived_rules.end(), result.begin(),
                result.end());
    };

    if (subatomic) {
        for (Handle target : get_subatoms(rule))
            add_result(derive_rules(source, target, rule));

    } else {
        for (Handle target : rule->get_implicant_seq())
            add_result(derive_rules(source, target, rule));

    }

    return derived_rules;
}

/**
 * Checks whether an atom can be used to generate a bindLink or not.
 *
 * @param h  The atom handle to be validated.
 *
 * @return   A boolean result of the check.
 */
bool ForwardChainer::is_valid_implicant(const Handle& h)
{
    FindAtoms fv(VARIABLE_NODE);
            fv.search_set(h);

    bool is_valid = (h->getType() != NOT_LINK)   and
                    (not classserver().isA(h->getType(),
                                          VIRTUAL_LINK))
                    and
                    (not fv.varset.empty());

    return is_valid;
}

void ForwardChainer::validate(Handle hsource, HandleSeq hfocus_set)
{
    if (hsource == Handle::UNDEFINED)
        throw RuntimeException(TRACE_INFO, "ForwardChainer - Invalid source.");
   //Any other validation here
}

/**
 * Gets all unique atoms of in the implicant list of @param r.
 *
 * @param r  A rule object
 *
 * @return   An unoderedHandleSet of of all unique atoms in the implicant.
 */
UnorderedHandleSet ForwardChainer::get_subatoms(Rule *rule)
{
    UnorderedHandleSet output_expanded;

    HandleSeq impl_members = rule->get_implicant_seq();
    for (Handle h : impl_members) {
        UnorderedHandleSet hs = get_all_unique_atoms(h);
        hs.erase(h); //Already tried to unify this.
        output_expanded.insert(hs.begin(), hs.end());
    }

    return output_expanded;
}

/**
 * Derives new rules from @param hrule by replacing variables
 * with their groundings.In case of fully grounded rules,only
 * the output atoms will be added to the list returned.
 *
 * @param as             An atomspace where the handles dwell.
 * @param hrule          A handle to BindLink instance
 * @param vars           The grounded var list in @param hrule
 * @param var_groundings The set of groundings to each var in @param vars
 *
 * @return A HandleSeq of all possible derived rules
 */
HandleSeq ForwardChainer::substitute_rule_part(
        AtomSpace& as, Handle hrule, const std::set<Handle>& vars,
        const std::vector<std::map<Handle, Handle>>& var_groundings)
{
    std::vector<std::map<Handle, Handle>> filtered_vgmap_list;

    //Filter out variables not listed in vars from var-groundings
    for (const auto& varg_map : var_groundings) {
        std::map<Handle, Handle> filtered;

        for (const auto& iv : varg_map) {
            if (find(vars.begin(), vars.end(), iv.first) != vars.end()) {
                filtered[iv.first] = iv.second;
            }
        }

        filtered_vgmap_list.push_back(filtered);
    }

    HandleSeq derived_rules;
    BindLinkPtr blptr = BindLinkCast(hrule);

    //Create the BindLink/Rule by substituting vars with groundings
    for (auto& vgmap : filtered_vgmap_list) {
        Handle himplicand = Substitutor::substitute(blptr->get_implicand(),
                                                    vgmap);
        Handle himplicant = Substitutor::substitute(blptr->get_body(), vgmap);
        Handle hvarlist;

        if (contains_atomtype(himplicand, VARIABLE_NODE)) {
            //Assuming himplicant's set of variables are superset for himplicand's,
            //generate varlist from himplicant.
            hvarlist = as.add_atom(
                    gen_sub_varlist(himplicant,
                                    LinkCast(hrule)->getOutgoingSet()[0]));
            Handle hderived_rule = as.add_atom(Handle(createBindLink(HandleSeq {
                hvarlist, himplicant, himplicand})));
            derived_rules.push_back(hderived_rule);

        } else {
            //We can't create BindLink with no variable.Just add the output.
            derived_rules.push_back(himplicand);
        }

    }

    return derived_rules;
}

/**
 * Tries to unify the @param source with @parama target and derives
 * new rules using @param rule as a template.
 *
 * @param source  An atom that might bind to variables in @param rule.
 * @param target  An atom to be unified with @param source
 * @rule  rule    The rule object whose implicants are to be unified.
 *
 * @return        true on successful unification and false otherwise.
 */
bool ForwardChainer::unify(Handle source, Handle target, Rule* rule)
{
    //exceptions
    if (not is_valid_implicant(target))
        return false;

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(target);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(target, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    BindLinkPtr bl =
    createBindLink(HandleSeq { implicant_vardecl, hcpy, hcpy });
    Handle blhandle = temp_pm_as.add_atom(bl);
    Handle  result = bindlink(&temp_pm_as, blhandle);
    HandleSeq results = temp_pm_as.get_outgoing(result);

    if (std::find(results.begin(), results.end(), sourcecpy) != results.end())
        return true;
    else
        return false;
}

/**
 *  Checks if sub atoms of implicant lists in @param rule are unifiable with
 *  @param source.
 *
 *  @param source  An atom that might bind to variables in @param rule.
 *  @param rule    The rule object whose implicants are to be sub atom unified.
 *
 *  @return        true if source is subatom unifiable and false otherwise.
 */
bool ForwardChainer::subatom_unify(Handle source, Rule* rule)
{
    UnorderedHandleSet output_expanded = get_subatoms(rule);

    for (Handle h : output_expanded) {
        if (unify(source, h, rule))
            return true;
    }

    return false;
}

Handle ForwardChainer::gen_sub_varlist(const Handle& parent,
                                       const Handle& parent_varlist)
{
    FindAtoms fv(VARIABLE_NODE);
    fv.search_set(parent);

    HandleSeq oset;
    if (LinkCast(parent_varlist))
        oset = LinkCast(parent_varlist)->getOutgoingSet();
    else
        oset.push_back(parent_varlist);

    HandleSeq final_oset;

    // for each var in varlist, check if it is used in parent
    for (const Handle& h : oset) {
        Type t = h->getType();

        if (VARIABLE_NODE == t && fv.varset.count(h) == 1)
            final_oset.push_back(h);
        else if (TYPED_VARIABLE_LINK == t
                and fv.varset.count(LinkCast(h)->getOutgoingSet()[0]) == 1)
            final_oset.push_back(h);
    }

    return Handle(createVariableList(final_oset));
}

void ForwardChainer::update_potential_sources(HandleSeq input)
{
    for (Handle i : input) {
        if (boost::find(_potential_sources, i) == _potential_sources.end())
            _potential_sources.push_back(i);
    }
}
