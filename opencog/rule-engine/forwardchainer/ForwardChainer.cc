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
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/unique_copy.hpp>

#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/query/BindLinkAPI.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/rule-engine/Rule.h>

#include "ForwardChainer.h"
#include "FocusSetPMCB.h"
#include "VarGroundingPMCB.h"
#include "FCLogger.h"

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

    // Set potential source.
    HandleSeq init_sources;

    // Accept set of initial sources wrapped in a SET_LINK.
    if (hsource->getType() == SET_LINK) {
        init_sources = LinkCast(hsource)->getOutgoingSet();
    } else {
        init_sources.push_back(hsource);
    }
    update_potential_sources(init_sources);

    //Add focus set atoms and sources to focus_set atomspace
    if (_search_focus_Set) {
        _focus_set = focus_set;

        for (const Handle& h : _focus_set)
            _focus_set_as.add_atom(h);

        for (const Handle& h : _potential_sources)
            _focus_set_as.add_atom(h);
    }

     //Set rules.
     for(Rule& r :_configReader.get_rules())
     {
         _rules.push_back(&r);
     }
}

/**
 * Do one step forward chaining and store result.
 *
 */
void ForwardChainer::do_step(void)
{
    _cur_source = choose_next_source();
    LAZY_FC_LOG_DEBUG << "Next source:" << std::endl
                      << _cur_source->toString();

    const Rule* rule = nullptr;
    UnorderedHandleSet derived_rhandles;

    // Return previously derived handles for a given choosen
    // rule. Empty if the rule has not been selected previously for
    // this particular source atom.
    auto get_derived = [&](const Handle& hsource) {
        auto pgmap = _fcstat.get_rule_pg_map(hsource);
        auto it = pgmap.find(rule->get_handle());
        if (it != pgmap.end()) {
            for (auto hwm : it->second) {
                derived_rhandles.insert(hwm.first);
            }
        }
    };

    bool subatom = false;
    rule = choose_rule(_cur_source, subatom);

    // Temporarily disable till I understand why it is needed
    // // If a fully matching rule is not found, look for
    // // subatomically matching rule.
    // if (not rule) {
    //     subatom = true;
    //     rule = choose_rule(_cur_source, subatom);
    // }

    if (rule) {
        // Use previously derived rules if they exist.
        if (_fcstat.has_partial_grounding(_cur_source))
            get_derived(_cur_source);
        else
            derived_rhandles = derive_rules(_cur_source, rule, subatom);
    }

    fc_logger().debug("Derived rule size = %d", derived_rhandles.size());

    UnorderedHandleSet products;
    // Applying all partial/full groundings.
    for (Handle rhandle : derived_rhandles) {
        HandleSeq hs = apply_rule(rhandle, _search_focus_Set);
        products.insert(hs.begin(), hs.end());
    }

    // Finally store source partial groundings and inference results.
    if (not derived_rhandles.empty()) {
        _potential_sources.insert(products.begin(), products.end());

        HandleWeightMap hwm;
        float weight = rule->get_weight();
        for (const Handle& h : derived_rhandles)
            hwm[h] = weight;
        _fcstat.add_partial_grounding(_cur_source, rule->get_handle(), hwm);

        _fcstat.add_inference_record(_cur_source, rule,
                                     HandleSeq(products.begin(), products.end()));
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
	    fc_logger().debug("Iteration %d", _iteration);
        do_step();
        _iteration++;
    }

    fc_logger().debug("finished forwarch chaining.");
}

/**
 * Applies all rules in the rule base.
 *
 * @param search_focus_set flag for searching focus set.
 */
void ForwardChainer::apply_all_rules(bool search_focus_set /*= false*/)
{
    for (Rule* rule : _rules) {
        HandleSeq hs = apply_rule(rule->get_handle(), search_focus_set);

        // Update
        _fcstat.add_inference_record(Handle::UNDEFINED, rule, hs);
        update_potential_sources(hs);
    }

}

HandleSeq ForwardChainer::get_chaining_result()
{
    return _fcstat.get_all_inferences();
}

Rule* ForwardChainer::choose_rule(Handle hsource, bool subatom_match)
{
    std::map<Rule*, float> rule_weight;
    for (Rule* r : _rules)
        rule_weight[r] = r->get_weight();

    fc_logger().debug("%d rules to be searched as matched against the source",
                      rule_weight.size());

    // Select a rule among the admissible rules in the rule-base via stochastic
    // selection, based on the weights of the rules in the current context.
    Rule* rule = nullptr;

    auto is_matched = [&](const Rule* rule) {
        if (_fcstat.has_partial_grounding(_cur_source)) {
            auto pgmap = _fcstat.get_rule_pg_map(hsource);
            auto it = pgmap.find(rule->get_handle());
            if (it != pgmap.end())
                return true;
        }
        return false;
    };

    std::string match_type = subatom_match ? "sub-atom-unifying" : "unifying";

    while (not rule_weight.empty()) {
        Rule *temp = _rec.tournament_select(rule_weight);
        fc_logger().fine("Selected rule %s to match against source by %s ",
                         temp->get_name().c_str(), match_type.c_str());

        if (is_matched(temp)) {
            fc_logger().fine("Found previous matching by %s",
                             match_type.c_str());

            rule = temp;
            break;
        }

        bool unified = false;
        if (subatom_match) {
            if (subatom_unify(hsource, temp)) {
                rule = temp;
                unified = true;
            }
        }

        else {
            HandleSeq hs = temp->get_implicant_seq();
            for (Handle term : hs) {
                if (unify(hsource, term, temp)) {
                    rule = temp;
                    unified = true;
                    break;
                }
            }
        }

        if (unified) {
            fc_logger().debug("Rule %s matched the source",
                              temp->get_name().c_str());
            break;
        } else {
            fc_logger().debug("Rule %s is not a match. Looking for another rule",
                              temp->get_name().c_str());
        }

        rule_weight.erase(temp);
    }

    if (nullptr == rule)
        fc_logger().debug("No matching rules were found for the given source");

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
        bool selected_before = (_selected_sources.find(hselected)
                                != _selected_sources.end());

        if (selected_before) {
            continue;
        } else {
            hchosen = hselected;
            _selected_sources.insert(hchosen);
            break;
        }
    }

    // In case all sources are selected
    if (hchosen == Handle::UNDEFINED)
        return urec.tournament_select(tournament_elem);

    return hchosen;
}

HandleSeq ForwardChainer::apply_rule(Handle rhandle,
                                     bool search_in_focus_set /*=false*/)
{
    HandleSeq result;

    // Check for fully grounded outputs returned by derive_rules.
    if (not contains_atomtype(rhandle, VARIABLE_NODE)) {

        // Subatomic matching may have created a non existing implicant
        // atom and if the implicant doesn't exist, nor should the implicand.
        Handle implicant = BindLinkCast(rhandle)->get_body();
        HandleSeq hs;
        if (implicant->getType() == AND_LINK or implicant->getType() == OR_LINK)
            hs = LinkCast(implicant)->getOutgoingSet();
        else
            hs.push_back(implicant);
        // Actual checking here.
        for (const Handle& h : hs) {
            if (_as.get_atom(h) == Handle::UNDEFINED or (search_in_focus_set
                    and _focus_set_as.get_atom(h) == Handle::UNDEFINED ) ) {
                return {};
            }
        }

        Instantiator inst(&_as);
        Handle houtput = LinkCast(rhandle)->getOutgoingSet().back();
        LAZY_FC_LOG_DEBUG << "Instantiating " << houtput->toShortString();

        result.push_back(inst.instantiate(houtput, { }));
    }

    else {
        if (search_in_focus_set) {

            // rhandle may introduce a new atom that satisfies
            // condition for the output. In order to prevent this
            // undesirable effect, lets store rhandle in a child
            // atomspace of parent focus_set_as so that PM will never
            // be able to find this new undesired atom created from
            // partial grounding.
            AtomSpace derived_rule_as(&_focus_set_as);
            Handle rhcpy = derived_rule_as.add_atom(rhandle);

            BindLinkPtr bl = BindLinkCast(rhcpy);

            FocusSetPMCB fs_pmcb(&derived_rule_as, &_as);
            fs_pmcb.implicand = bl->get_implicand();

            LAZY_FC_LOG_DEBUG << "In focus set, apply rule:" << std::endl
                              << rhcpy->toShortString();

            bl->imply(fs_pmcb, false);

            result = fs_pmcb.get_result_list();

            LAZY_FC_LOG_DEBUG << "Result is:" << std::endl
                              << _as.add_link(SET_LINK, result)->toShortString();

        }
        // Search the whole atomspace.
        else {
            AtomSpace derived_rule_as(&_as);

            Handle rhcpy = derived_rule_as.add_atom(rhandle);

            LAZY_FC_LOG_DEBUG << "On atomspace, apply rule:" << std::endl
                              << rhcpy->toShortString();

            Handle h = bindlink(&derived_rule_as, rhcpy);

            LAZY_FC_LOG_DEBUG << "Result is:" << std::endl
                              << h->toShortString();

            LinkPtr lp(LinkCast(h));
            if (lp) result = lp->getOutgoingSet();
        }
    }

    // Add result back to atomspace.
    if (search_in_focus_set) {
        for (const Handle& h : result)
            _focus_set_as.add_atom(h);

    } else {
        for (const Handle& h : result)
            _as.add_atom(h);
    }

    return result;
}

/**
 * Derives new rules by replacing variables that are unfiable in @param term
 * with source. The rule handles are not added to any atomspace.
 *
 * @param  source    A source atom that will be matched with the term.
 * @param  term      An implicant term containing variables to be grounded form source.
 * @param  rule      A rule object that contains @param term in its implicant. *
 *
 * @return  A UnorderedHandleSet of derived rule handles.
 */
UnorderedHandleSet ForwardChainer::derive_rules(Handle source, Handle term,
                                                const Rule* rule)
{
    //exceptions
    if (not is_valid_implicant(term))
        return {};

    UnorderedHandleSet derived_rules;

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(term);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(term, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    Handle h = temp_pm_as.add_link(BIND_LINK, implicant_vardecl, hcpy, hcpy);
    BindLinkPtr bl = BindLinkCast(h);

    VarGroundingPMCB gcb(&temp_pm_as);
    gcb.implicand = bl->get_implicand();

    bl->imply(gcb, false);

    auto del_by_value =
        [] (std::vector<std::map<Handle,Handle>>& vec_map,const Handle& h) {
        for (auto& map: vec_map)
            for (auto& it:map) { if (it.second == h) map.erase(it.first); }
    };

    // We don't want VariableList atoms to ground free-vars.
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
                    if (nr != rhandle) {
                        derived_rules.insert(nr);
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
UnorderedHandleSet ForwardChainer::derive_rules(Handle source, const Rule* rule,
                                                bool subatomic/*=false*/)
{
    UnorderedHandleSet derived_rules;

    auto add_result = [&derived_rules] (UnorderedHandleSet result) {
        derived_rules.insert(result.begin(), result.end());
    };

    if (subatomic) {
        for (Handle subterm : get_subatoms(rule))
            add_result(derive_rules(source, subterm, rule));

    } else {
        for (Handle term : rule->get_implicant_seq())
            add_result(derive_rules(source, term, rule));

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
 * Get all unique atoms within a link and its sublinks.
 *
 * Similar to getAllAtoms except there will be no repetition.
 *
 * @param h     the top level link
 * @return      a UnorderedHandleSet of atoms
 */
static void get_all_unique_atoms(const Handle& h, UnorderedHandleSet& atom_set)
{
    atom_set.insert(h);

    LinkPtr lll(LinkCast(h));
    if (lll)
    {
        for (const Handle& o : lll->getOutgoingSet())
            get_all_unique_atoms(o, atom_set);
    }
}


/**
 * Gets all unique atoms of in the implicant list of @param r.
 *
 * @param r  A rule object
 *
 * @return   An UnoderedHandleSet of all unique atoms in the implicant.
 */
UnorderedHandleSet ForwardChainer::get_subatoms(const Rule *rule)
{
    UnorderedHandleSet output_expanded;

    for (const Handle& h : rule->get_implicant_seq())
    {
        get_all_unique_atoms(h, output_expanded);
        output_expanded.erase(h); // Already tried to unify this.
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

        //Assuming himplicant's set of variables are superset for himplicand's,
        //generate varlist from himplicant.
        Handle hvarlist = as.add_atom(
                gen_sub_varlist(himplicant,
                                LinkCast(hrule)->getOutgoingSet()[0]));
        Handle hderived_rule = as.add_atom(createBindLink(HandleSeq {
            hvarlist, himplicant, himplicand}));
        derived_rules.push_back(hderived_rule);
    }

    return derived_rules;
}

/**
 * Tries to unify the @param source with @parama term and derives
 * new rules using @param rule as a template.
 *
 * @param source  An atom that might bind to variables in @param rule.
 * @param term  An atom to be unified with @param source
 * @rule  rule    The rule object whose implicants are to be unified.
 *
 * @return        true on successful unification and false otherwise.
 */
bool ForwardChainer::unify(Handle source, Handle term, const Rule* rule)
{
    // exceptions
    if (not is_valid_implicant(term))
        return false;

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(term);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(term, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    Handle blhandle =
        temp_pm_as.add_link(BIND_LINK, implicant_vardecl, hcpy, hcpy);
    Handle result = bindlink(&temp_pm_as, blhandle);
    HandleSeq results = LinkCast(result)->getOutgoingSet();

    return std::find(results.begin(), results.end(), sourcecpy) != results.end();
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
bool ForwardChainer::subatom_unify(Handle source, const Rule* rule)
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

        if ((VARIABLE_NODE == t and fv.varset.count(h) == 1)
            or (TYPED_VARIABLE_LINK == t
                and fv.varset.count(LinkCast(h)->getOutgoingSet()[0]) == 1))
            final_oset.push_back(h);
    }

    return Handle(createVariableList(final_oset));
}

void ForwardChainer::update_potential_sources(HandleSeq input)
{
	_potential_sources.insert(input.begin(), input.end());
}
