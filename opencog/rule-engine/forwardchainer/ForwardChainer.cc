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

ForwardChainer::ForwardChainer(AtomSpace& as, const Handle& rbs,
                               const Handle& hsource,
                               const HandleSeq& focus_set /* = HandleSeq()*/,
                               source_selection_mode sm /*= source_selection_mode::UNIFORM */) :
    _as(as), _rec(as), _rbs(rbs), _configReader(as, rbs), _fcstat(as)
{
    _ts_mode = sm;
    init(hsource, focus_set);
}

ForwardChainer::~ForwardChainer()
{
}

void ForwardChainer::init(const Handle& hsource, const HandleSeq& focus_set)
{
    validate(hsource, focus_set);

    _search_in_af = _configReader.get_attention_allocation();
    _search_focus_set = not focus_set.empty();

    // Set potential source.
    HandleSeq init_sources;

    // Accept set of initial sources wrapped in a SET_LINK.
    if (hsource->getType() == SET_LINK) {
        init_sources = hsource->getOutgoingSet();
    } else {
        init_sources.push_back(hsource);
    }
    update_potential_sources(init_sources);

    // Add focus set atoms and sources to focus_set atomspace
    if (_search_focus_set) {
        _focus_set = focus_set;

        for (const Handle& h : _focus_set)
            _focus_set_as.add_atom(h);

        for (const Handle& h : _potential_sources)
            _focus_set_as.add_atom(h);
    }

    // Set rules.
    for(Rule& r :_configReader.get_rules())
    {
        _rules.push_back(&r);
    }

    // Reset the iteration count and max count
    _iteration = 0;
    _max_iteration = _configReader.get_maximum_iterations();
}

/**
 * Do one step forward chaining and store result.
 *
 */
void ForwardChainer::do_step()
{
    fc_logger().debug("Iteration %d", _iteration);
    _iteration++;

    // Select source
    _cur_source = select_source();
    LAZY_FC_LOG_DEBUG << "Source:" << std::endl << _cur_source->toString();

    // Select rule
    const Rule* rule = select_rule(_cur_source);
    if (not rule) {
        fc_logger().debug("No selected rule, abort step");
        return;
    }

    // Apply rule on _cur_source
    UnorderedHandleSet products = apply_rule(rule);

    // Store results
    update_potential_sources(products);
    _fcstat.add_inference_record(_iteration - 1, // _iteration has
                                                 // already been
                                                 // incremented
                                 _cur_source, rule, products);
}

void ForwardChainer::do_chain()
{
    // Relex2Logic uses this. TODO make a separate class to handle
    // this robustly.
    if(_potential_sources.empty())
    {
        apply_all_rules();
        return;
    }

    while (not termination())
    {
        do_step();
    }

    fc_logger().debug("Finished forwarch chaining");
}

bool ForwardChainer::termination()
{
    return _max_iteration <= _iteration;
}

/**
 * Applies all rules in the rule base.
 *
 * @param search_focus_set flag for searching focus set.
 */
void ForwardChainer::apply_all_rules()
{
    for (Rule* rule : _rules) {
        fc_logger().debug("Apply rule %s", rule->get_name().c_str());
        HandleSeq hs = apply_rule(rule->get_forward_rule());

        // Update
        _fcstat.add_inference_record(_iteration,
                                     _as.add_node(CONCEPT_NODE, "dummy-source"),
                                     rule,
                                     UnorderedHandleSet(hs.begin(), hs.end()));
        update_potential_sources(hs);
    }
}

UnorderedHandleSet ForwardChainer::get_chaining_result()
{
    return _fcstat.get_all_products();
}

Handle ForwardChainer::select_source()
{
	size_t selsrc_size = _selected_sources.size();
	// If all sources have been selected then insert the sources'
	// children in the set of potential sources
	if (_unselected_sources.empty()) {
		fc_logger().debug() << "All " << selsrc_size
		                    << " sources have already been selected";

		// Hack to help to exhaust sources with
		// multiple matching rules. This would be
		// better used with a memory of which
		// source x rule pairs have been
		// tried. But choose_source would still
		// remain a hack anyway.
		if (biased_randbool(0.01)) {
			for (const Handle& h : _selected_sources) {
				if (h->isLink()) {
					const HandleSeq& outgoings = h->getOutgoingSet();
					HandleSeq no_free_vars_outgoings;
					// Only add children with no free variables in them
					for (const Handle& h : outgoings)
						if (is_closed(h))
							no_free_vars_outgoings.push_back(h);
					update_potential_sources(no_free_vars_outgoings);
				}
			}
			fc_logger().debug() << (_potential_sources.size() - selsrc_size)
			                    << " sources' children have been added as "
			                    << "potential sources";
		} else {
			fc_logger().debug() << "No added sources, "
			                    << "retry existing sources instead";
		}
	}

	fc_logger().debug() << "Selected sources so far "
	                    << selsrc_size << "/" << _potential_sources.size();

	URECommons urec(_as);
	map<Handle, float> tournament_elem;

	const UnorderedHandleSet& to_select_sources =
		_unselected_sources.empty() ? _potential_sources : _unselected_sources;

	Handle hchosen;
	switch (_ts_mode) {
	case source_selection_mode::TV_FITNESS:
	    for (const Handle& s : to_select_sources)
		    tournament_elem[s] = urec.tv_fitness(s);
	    hchosen = urec.tournament_select(tournament_elem);
	    break;

	case source_selection_mode::STI:
	    for (const Handle& s : to_select_sources)
		    tournament_elem[s] = s->getSTI();
	    hchosen = urec.tournament_select(tournament_elem);
	    break;

	case source_selection_mode::UNIFORM:
		hchosen = rand_element(to_select_sources);
	    break;

	default:
	    throw RuntimeException(TRACE_INFO, "Unknown source selection mode.");
	    break;
	}

	OC_ASSERT(hchosen != Handle::UNDEFINED);
	
	_selected_sources.insert(hchosen);
	_unselected_sources.erase(hchosen);

	return hchosen;
}

Rule* ForwardChainer::select_rule(const Handle& hsource)
{
    std::map<Rule*, float> rule_weight;
    for (Rule* r : _rules)
        rule_weight[r] = r->get_weight();

    fc_logger().debug("%d rules to be searched as matched against the source",
                      rule_weight.size());

    // Select a rule among the admissible rules in the rule-base via stochastic
    // selection, based on the weights of the rules in the current context.
    Rule* rule = nullptr;

    while (not rule_weight.empty()) {
        Rule *temp = _rec.tournament_select(rule_weight);
        fc_logger().fine("Selected rule %s to match against the source",
                         temp->get_name().c_str());

        bool unified = false;
        for (Handle premise_pat : temp->get_premises()) {
            if (unify(hsource, premise_pat, temp)) {
                rule = temp;
                unified = true;
                break;
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

UnorderedHandleSet ForwardChainer::apply_rule(const Rule* rule)
{
    // Derive rules partially applied with the source
    UnorderedHandleSet derived_rhandles = derive_rules(_cur_source, rule);
    if (derived_rhandles.empty()) {
        fc_logger().debug("No derived rule, abort step");
        return UnorderedHandleSet();
    } else {
        fc_logger().debug("Derived rule size = %d", derived_rhandles.size());
    }

    // Applying all partial/full groundings
    UnorderedHandleSet products;
    for (Handle rhandle : derived_rhandles) {
        HandleSeq hs = apply_rule(rhandle);
        products.insert(hs.begin(), hs.end());
    }
    return products;
}

HandleSeq ForwardChainer::apply_rule(const Handle& rhandle)
{
    HandleSeq result;

    // Check for fully grounded outputs returned by derive_rules.
    if (not contains_atomtype(rhandle, VARIABLE_NODE)) {

        // Subatomic matching may have created a non existing implicant
        // atom and if the implicant doesn't exist, nor should the implicand.
        Handle implicant = BindLinkCast(rhandle)->get_body();
        HandleSeq hs;
        if (implicant->getType() == AND_LINK or implicant->getType() == OR_LINK)
            hs = implicant->getOutgoingSet();
        else
            hs.push_back(implicant);
        // Actual checking here.
        for (const Handle& h : hs) {
            if (_as.get_atom(h) == Handle::UNDEFINED
                or (_search_focus_set
                    and _focus_set_as.get_atom(h) == Handle::UNDEFINED)) {
                return {};
            }
        }

        Instantiator inst(&_as);
        Handle houtput = rhandle->getOutgoingSet().back();
        LAZY_FC_LOG_DEBUG << "Instantiating " << houtput->toShortString();

        result.push_back(inst.instantiate(houtput, {}));
    }

    else {
        if (_search_focus_set) {

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
        }
        // Search the whole atomspace.
        else {
            AtomSpace derived_rule_as(&_as);

            Handle rhcpy = derived_rule_as.add_atom(rhandle);

            LAZY_FC_LOG_DEBUG << "On atomspace, apply rule:" << std::endl
                              << rhcpy->toShortString();

            Handle h = bindlink(&derived_rule_as, rhcpy);

            result = h->getOutgoingSet();
        }
    }

    // Take the results from applying the rule and add them in the
    // given AtomSpace
    auto add_results = [](AtomSpace& as, HandleSeq& result) {
	    for (Handle& h : result)
	    {
		    Type t = h->getType();
		    // If it's a List then add all the results. That kinda
		    // means you can't infer List itself, maybe something to
		    // look after.
		    if (t == LIST_LINK)
			    for (const Handle& hc : h->getOutgoingSet())
				    as.add_atom(hc);
		    else
			    h = as.add_atom(h);
	    }
    };

    // Add result back to atomspace.
    if (_search_focus_set) {
	    add_results(_focus_set_as, result);
    } else {
	    add_results(_as, result);
    }

    LAZY_FC_LOG_DEBUG << "Result is:" << std::endl
                      << _as.add_link(SET_LINK, result)->toShortString();

    return result;
}

/**
 * Derives new rules by replacing variables that are unifiable in
 * @param pattern with source. The rule handles are not added to any
 * atomspace.
 *
 * @param  source    A source atom that will be matched with the pattern.
 * @param  pattern   An implicant term containing variables to be grounded form source.
 * @param  rule      A rule object that contains @param pattern in its implicant. *
 *
 * @return  A UnorderedHandleSet of derived rule handles.
 */
UnorderedHandleSet ForwardChainer::derive_rules(const Handle& source,
                                                const Handle& pattern,
                                                const Rule* rule)
{
    // Exceptions
    if (not is_valid_implicant(pattern))
	    return UnorderedHandleSet();

    // Create a temporary atomspace with the rule pattern and the
    // source inside
    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(pattern);
    Handle implicant_vardecl = temp_pm_as.add_atom(
        gen_sub_varlist(pattern, rule->get_forward_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    Handle h = temp_pm_as.add_link(BIND_LINK, implicant_vardecl, hcpy, hcpy);
    BindLinkPtr bl = BindLinkCast(h);

    VarGroundingPMCB gcb(&temp_pm_as);
    gcb.implicand = bl->get_implicand();

    bl->imply(gcb, false);

    // Remove all groundings that don't match the source
    auto tgit = gcb.term_groundings.begin();
    auto vgit = gcb.var_groundings.begin();
    for (; tgit != gcb.term_groundings.end();) {
        auto it = tgit->find(hcpy);
        if (it == tgit->end() or it->second != sourcecpy) {
            tgit = gcb.term_groundings.erase(tgit);
            vgit = gcb.var_groundings.erase(vgit);
        } else {
            tgit++;
            vgit++;
        }
    }

    if (gcb.term_groundings.empty())
	    return UnorderedHandleSet();

    // OC_ASSERT(gcb.term_groundings.size() == 1,
    //           "There should be only one way to have a "
    //           "premise clause ground a source");

    UnorderedHandleSet derived_rules;

    // Generate the derived rules
    FindAtoms fv(VARIABLE_NODE);
    const auto& termg_map = gcb.term_groundings.back();
    for (const auto& it : termg_map) {
        if (it.second == sourcecpy) {

            fv.search_set(it.first);

            Handle rhandle = rule->get_forward_rule();
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

    return derived_rules;
}

/**
 * Derives new rules by replacing variables in @param rule that are
 * unifiable with source.
 *
 * @param  source    A source atom that will be matched with the rule.
 * @param  rule      A rule object
 *
 * @return  A HandleSeq of derived rule handles.
 */
UnorderedHandleSet ForwardChainer::derive_rules(const Handle& source,
                                                const Rule* rule)
{
    UnorderedHandleSet derived_rules;

    auto add_result = [&derived_rules] (const UnorderedHandleSet& result) {
        derived_rules.insert(result.begin(), result.end());
    };

    for (const Handle& premise_pat : rule->get_premises())
        add_result(derive_rules(source, premise_pat, rule));

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

    bool is_valid = h->getType() != NOT_LINK
        and not classserver().isA(h->getType(), VIRTUAL_LINK)
        and not fv.varset.empty();

    return is_valid;
}

void ForwardChainer::validate(const Handle& hsource, const HandleSeq& hfocus_set)
{
    if (hsource == Handle::UNDEFINED)
        throw RuntimeException(TRACE_INFO, "ForwardChainer - Invalid source.");
    // Any other validation here
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
    if (h->isLink())
    {
        for (const Handle& o : h->getOutgoingSet())
            get_all_unique_atoms(o, atom_set);
    }
}

bool ForwardChainer::is_constant_clause(const Handle& hvardecls,
                                        const Handle& hclause) const
{
    VariableList vl(hvardecls);
    return not any_unquoted_unscoped_in_tree(hclause, vl.get_variables().varset);
}

/**
 * Remove clauses that are do not contain free variables in hvardecl.
 */
Handle ForwardChainer::remove_constant_clauses(const Handle& hvarlist,
                                               const Handle& himplicant)
{
    Type t = himplicant->getType();
    if (t == AND_LINK) {
        HandleSeq outgoings;
        for (const Handle& hclause : himplicant->getOutgoingSet())
            if (not is_constant_clause(hvarlist, hclause))
                outgoings.push_back(hclause);
        // The pattern matcher crashes if given an empty AndLink, so
        // we add whatever comes first to avoid that
        if (outgoings.empty()) {
            OC_ASSERT(not himplicant->getOutgoingSet().empty());
            outgoings.push_back(himplicant->getOutgoingSet()[0]);
        }
        return Handle(createLink(t, outgoings));
    } else
        return himplicant;
}

/**
 * Derives new rules from @param hrule by replacing variables
 * with their groundings. In case of fully grounded rules, only
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
        AtomSpace& as, const Handle& hrule, const OrderedHandleSet& vars,
        const HandleMapSeq& var_groundings)
{
    HandleMapSeq filtered_vgmap_list;

    // Filter out variables not listed in vars from var_groundings
    for (const auto& varg_map : var_groundings) {
        HandleMap filtered;

        for (const auto& iv : varg_map) {
            if (vars.find(iv.first) != vars.end()) {
                filtered[iv.first] = iv.second;
            }
        }

        filtered_vgmap_list.push_back(filtered);
    }

    HandleSeq derived_rules;
    BindLinkPtr blptr = BindLinkCast(hrule);

    // Create the BindLink/Rule by substituting vars with groundings
    for (auto& vgmap : filtered_vgmap_list) {
        Handle himplicand = Substitutor::substitute(blptr->get_implicand(),
                                                    vgmap);
        Handle himplicant = Substitutor::substitute(blptr->get_body(), vgmap);

        // Assuming himplicant's set of variables are superset for himplicand's,
        // generate varlist from himplicant.
        Handle hvarlist = as.add_atom(
            gen_sub_varlist(himplicant,
                            hrule->getOutgoingAtom(0)));

        // Create a simplified implicand without constant clauses
        Handle hsimplicant = remove_constant_clauses(hvarlist, himplicant);
        Handle hderived_rule = as.add_atom(createBindLink(HandleSeq {
                    hvarlist, hsimplicant, himplicand}));
        derived_rules.push_back(hderived_rule);
    }

    return derived_rules;
}

/**
 * Tries to unify the @param source with @parama term and derives
 * new rules using @param rule as a template.
 *
 * @param source  An atom that might bind to variables in @param rule.
 * @param pattern An atom to be unified with @param source
 * @rule  rule    The rule object whose implicants are to be unified.
 *
 * @return        true on successful unification and false otherwise.
 */
bool ForwardChainer::unify(const Handle& source, const Handle& pattern,
                           const Rule* rule)
{
    // Exceptions
    if (not is_valid_implicant(pattern))
        return false;

    AtomSpace tmp_as;
    Handle pattern_cpy = tmp_as.add_atom(pattern);
    Handle pattern_vardecl =
	    tmp_as.add_atom(gen_sub_varlist(pattern, rule->get_forward_vardecl()));
    Handle source_cpy = tmp_as.add_atom(source);

    Handle bl =
        tmp_as.add_link(BIND_LINK, pattern_vardecl, pattern_cpy, pattern_cpy);
    Handle result = bindlink(&tmp_as, bl);
    HandleSeq results = result->getOutgoingSet();

    return std::find(results.begin(), results.end(), source_cpy) != results.end();
}

Handle ForwardChainer::gen_sub_varlist(const Handle& parent,
                                       const Handle& parent_varlist)
{
    FindAtoms fv(VARIABLE_NODE);
    fv.search_set(parent);

    HandleSeq oset;
    if (parent_varlist->isLink())
        oset = parent_varlist->getOutgoingSet();
    else
        oset.push_back(parent_varlist);

    HandleSeq final_oset;

    // For each var in varlist, check if it is used in parent
    for (const Handle& h : oset) {
        Type t = h->getType();

        if ((VARIABLE_NODE == t and fv.varset.count(h) == 1)
            or (TYPED_VARIABLE_LINK == t
                and fv.varset.count(h->getOutgoingAtom(0)) == 1))
            final_oset.push_back(h);
    }

    return Handle(createVariableList(final_oset));
}
