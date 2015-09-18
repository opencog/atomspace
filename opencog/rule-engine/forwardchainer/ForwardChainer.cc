/*
 * ForwardChainer.cc
 *
 * Copyright (C) 2014,2015
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

#include <opencog/util/Logger.h>

#include <opencog/atoms/bind/BindLink.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/query/BindLinkAPI.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/rule-engine/Rule.h>

#include "ForwardChainer.h"
#include "FocusSetPMCB.h"
#include "VarGroundingPMCB.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& as, Handle rbs) :
	_as(as), _rec(as), _rbs(rbs), _configReader(as, rbs), _fcmem(&as)
{
   init();
}

void ForwardChainer::init()
{
    _fcmem.set_search_in_af(_configReader.get_attention_allocation());
    _fcmem.set_rules(_configReader.get_rules());
    _fcmem.set_cur_rule(nullptr);
    
    _ts_mode = TV_FITNESS_BASED;

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
 * Does one step forward chaining
 *
 * @return An unordered sets of result of applying a particular selected rule.
 */
UnorderedHandleSet ForwardChainer::do_step(bool search_focus_set/* = false*/)
{

    Handle hsource = choose_next_source(_fcmem);

    _log->debug("[ForwardChainer] Next source %s", hsource->toString().c_str());

    _fcmem.set_source(hsource);

    HandleSeq derived_rhandles;

    //choose a rule that source unifies with one of its premises.
    Rule *rule = choose_rule(hsource, false);
    if (rule) {
        _fcmem.set_cur_rule(rule);
        derived_rhandles = derive_rules(hsource, rule);

    } else {
        //choose rule that unifies that source unifies with sub-atoms of its premises.
        rule = choose_rule(hsource, true);

        if (rule) {
            _fcmem.set_cur_rule(rule);
            derived_rhandles = derive_rules(hsource, rule,
            true);
        }
    }

    _log->debug( "Derived rule size = %d", derived_rhandles.size());

    UnorderedHandleSet products;

    for (Handle rhandle : derived_rhandles) {
        HandleSeq temp_result = apply_rule(rhandle,search_focus_set);

        std::copy(temp_result.begin(), temp_result.end(),
                  std::inserter(products, products.end()));
    }

    return products;
}

void ForwardChainer::do_chain(Handle hsource, HandleSeq focus_set)
{

    validate(hsource,focus_set);

    _fcmem.set_focus_set(focus_set);

    HandleSeq init_sources;
    //Accept set of initial sources wrapped in a SET_LINK
    if(LinkCast(hsource) and hsource->getType() == SET_LINK)
    {
     init_sources = _as.get_outgoing(hsource);

     //Relex2Logic uses this.TODO make a separate class
     //to handle this robustly.
     if(init_sources.empty())
     {
         bool search_in_af = not focus_set.empty();
         apply_all_rules(search_in_af);
         return;
     }

    }
    else
    {
        init_sources.push_back(hsource);
    }

    // Variable fulfillment query.
    UnorderedHandleSet var_nodes = get_outgoing_nodes(hsource,
                                                      { VARIABLE_NODE });
    if (not var_nodes.empty())
        return do_pm(hsource, var_nodes);

    // Default forward chaining
    _fcmem.update_potential_sources(init_sources);

    auto max_iter = _configReader.get_maximum_iterations();

    while (_iteration < max_iter /*OR other termination criteria*/) {

        _log->debug("Iteration %d", _iteration);

        UnorderedHandleSet products;

        if (focus_set.empty())
            products = do_step(false);
        else
            products = do_step(true);

        _fcmem.add_rules_product(_iteration,
                                 HandleSeq(products.begin(), products.end()));
        _fcmem.update_potential_sources(
                HandleSeq(products.begin(), products.end()));

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
    _fcmem.set_source(hsource);
    Handle hvar_list = _as.add_link(VARIABLE_LIST, vars);
    Handle hclause = _as.add_link(AND_LINK, hsource);

    // Run the pattern matcher, find all patterns that satisfy the
    // the clause, with the given variables in it.
    PatternLinkPtr sl(createPatternLink(hvar_list, hclause));
    sl->satisfy(impl);

    // Update result
    _fcmem.add_rules_product(0, impl.get_result_list());

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
        _fcmem.set_cur_rule(rule);
        _fcmem.add_rules_product(0, impl.get_result_list());
    }
}

/**
 * Applies all rules in the rule base.
 *
 * @param search_focus_set flag for searching focus set.
 */
void ForwardChainer::apply_all_rules(bool search_focus_set /*= false*/)
{
    vector<Rule*> rules = _fcmem.get_rules();

    for (Rule* rule : rules) {
        _fcmem.set_cur_rule(rule);
        HandleSeq hs = apply_rule(rule->get_handle(), search_focus_set);

        //Update
        _fcmem.add_rules_product(0, hs);
        _fcmem.update_potential_sources(hs);
    }

}

HandleSeq ForwardChainer::get_chaining_result()
{
    return _fcmem.get_result();
}

Rule* ForwardChainer::choose_rule(Handle hsource, bool subatom_match)
{
    //TODO move this somewhere else
    std::map<Rule*, float> rule_weight;
    for (Rule* r : _fcmem.get_rules())
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

HandleSeq ForwardChainer::choose_premises(FCMemory& fcmem)
{
    HandleSeq inputs;
    URECommons urec(_as);
    Handle hsource = fcmem.get_cur_source();

    // Get everything associated with the source handle.
    UnorderedHandleSet neighbors = get_distant_neighbors(hsource, 2);

    // Add all root links of atoms in @param neighbors.
    for (auto hn : neighbors) {
        if (hn->getType() != VARIABLE_NODE) {
            HandleSeq roots;
            urec.get_root_links(hn, roots);
            for (auto r : roots) {
                if (find(inputs.begin(), inputs.end(), r) == inputs.end() and r->getType()
                        != BIND_LINK)
                    inputs.push_back(r);
            }
        }
    }

    return inputs;
}

Handle ForwardChainer::choose_next_source(FCMemory& fcmem)
{

    URECommons urec(_as);
    HandleSeq tlist = fcmem.get_potential_sources();
    map<Handle, float> tournament_elem;

    switch (_ts_mode) {
    case TV_FITNESS_BASED:
        for (Handle t : tlist)
            tournament_elem[t] = urec.tv_fitness(t);
        break;

    case STI_BASED:
        for (Handle t : tlist)
            tournament_elem[t] = t->getSTI();
        break;

    default:
        throw RuntimeException(TRACE_INFO, "Unknown source selection mode.");
        break;
    }

    Handle hchosen = Handle::UNDEFINED;

    //!Choose a new source that has never been chosen before.
    //!xxx FIXME since same handle might be chosen multiple times the following
    //!code doesn't guarantee all sources have been exhaustively looked.
    for (size_t i = 0; i < tournament_elem.size(); i++) {
        Handle hselected = urec.tournament_select(tournament_elem);
        if (fcmem.isin_selected_sources(hselected)) {
            continue;
        } else {
            hchosen = hselected;
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
        HandleSeq focus_set_atoms = _fcmem.get_focus_set();
        for (Handle h : focus_set_atoms)
            focus_set_as.add_atom(h);

        //Add source atoms to focus_set atomspace
        HandleSeq sources = _fcmem.get_potential_sources();
        for (Handle h : sources)
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

        std::cout << "ATOMSPACE:" << derived_rule_as << std::endl;
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
 * with their groundings.
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
    //Substitutor st(&as);

    for (auto& vgmap : filtered_vgmap_list) {
        Handle himplicand = Substitutor::substitute(blptr->get_implicand(), vgmap);
        //Create the BindLink/Rule by substituting vars with groundings
        if (contains_atomtype(himplicand, VARIABLE_NODE)) {
            Handle himplicant = Substitutor::substitute(blptr->get_body(), vgmap);

            //Assuming himplicant's set of variables are superset for himplicand's,
            //generate varlist from himplicant.
            Handle hvarlist = as.add_atom(gen_sub_varlist(
                    himplicant, LinkCast(hrule)->getOutgoingSet()[0]));
            Handle hderived_rule = as.add_atom(Handle(createBindLink(HandleSeq {
                    hvarlist, himplicant, himplicand })));
            derived_rules.push_back(hderived_rule);
        } else {
            //TODO Execute if executable and push to FC results
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
