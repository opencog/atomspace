/*
 * BackwardChainer.cc
 *
 * Copyright (C) 2014 Misgana Bayetta
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
 *         William Ma <https://github.com/williampma>
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

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "UnifyPMCB.h"

#include <opencog/util/random.h>

#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atoms/bind/PatternLink.h>

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, Handle rbs)
	: _as(as), _configReader(as, rbs),
	  // create a garbage superspace with _as as parent, so codes acting on
	  // _garbage will see stuff in _as, but codes acting on _as will not
	  // see stuff in _garbage
	  _garbage_superspace(&_as) {}

/**
 * Set the initial target for backward chaining.
 *
 * @param init_target   Handle of the target
 */
void BackwardChainer::set_target(Handle init_target)
{
	_init_target = init_target;

	_targets_set.clear();
	_targets_set.emplace(_init_target,
	                     _garbage_superspace.add_atom(createVariableList(get_free_vars_in_tree(init_target))));
}

UREConfigReader& BackwardChainer::get_config()
{
	return _configReader;
}

const UREConfigReader& BackwardChainer::get_config() const
{
	return _configReader;
}

/**
 * The public entry point for full backward chaining.
 *
 * @param max_steps   The maximum number of backward chain steps
 *
 * XXX TODO add more stopping param like fitness criterion, etc
 */
void BackwardChainer::do_chain()
{
	int i = 0;

	while (i != _configReader.get_maximum_iterations())
	{
		do_step();

		// XXX TODO check the TV of the initial target to see if we want more steps

		i++;
	}
}

/**
 * Do a single step of backward chaining.
 */
void BackwardChainer::do_step()
{	
	logger().debug("[BackwardChainer] ==========================================");
	logger().debug("[BackwardChainer] Start of a single BC step");
	logger().debug("[BackwardChainer] %d potential targets", _targets_set.size());

	// do target selection using some criteria
	// XXX for example, choose target with low TV 50% of the time
	Target& selected_target = _targets_set.select();

	logger().debug("[BackwardChainer] Selected target " + selected_target.get_handle()->toShortString());
	logger().debug("[BackwardChainer] with var_decl " + selected_target.get_vardecl()->toShortString());

	if (selected_target.get_varseq().empty())
		logger().debug("[BackwardChainer] Target is 'Truth Value Query'");
	else
		logger().debug("[BackwardChainer] Target is 'Variable Fullfillment Query'");

	process_target(selected_target);

	// XXX Cannot clear since some target are reversed grounded rule's input
	// and contain temp variables. Need a better clean that keep stuff used
	// in the target set
	//_garbage_superspace.clear();

	logger().debug("[BackwardChainer] End of a single BC step");
}

/**
 * Get the current result on the initial target, if any.
 *
 * @return a VarMultimap mapping each variable to all possible solutions
 */
const VarMultimap& BackwardChainer::get_chaining_result()
{
	return _targets_set.get(_init_target).get_varmap();
}

/**
 * The main recursive backward chaining method.
 *
 * @param target   the Target object containing the target atom
 */
void BackwardChainer::process_target(Target& target)
{
	Handle htarget = target.get_handle();

	// Check whether this target is a virtual link and is useless to explore
	if (classserver().isA(htarget->getType(), VIRTUAL_LINK))
	{
		logger().debug("[BackwardChainer] Boring virtual link goal, "
		               "skipping " + htarget->toShortString());
		return;
	}

	// before doing any real backward chaining, see if the free variables
	// can already be grounded (no point grounding bound variables)
	if (not target.get_varseq().empty())
	{
		std::vector<VarMap> kb_vmap;

		HandleSeq kb_match = match_knowledge_base(htarget, target.get_vardecl(),
		                                          kb_vmap);

		// Matched something in the knowledge base? Then need to store
		// any grounding as a possible solution for this target
		if (not kb_match.empty())
		{
			logger().debug("[BackwardChainer] Matched something in knowledge base, "
			               "storing the grounding");

			for (size_t i = 0; i < kb_match.size(); ++i)
			{
				Handle& soln = kb_match[i];
				VarMap& vgm = kb_vmap[i];

				logger().debug("[BackwardChainer] Looking at grounding "
				               + soln->toShortString());

				// add whatever it matched as Target (so new variables can be
				// filled, and TV updated)
				_targets_set.emplace(soln,
					                 _garbage_superspace.add_atom(createVariableList(get_free_vars_in_tree(soln))));

				target.store_varmap(vgm);
			}
		}
	}

	// If logical link, break it up, add each to new targets and return
	if (_logical_link_types.count(htarget->getType()) == 1)
	{
		logger().debug("[BackwardChainer] Breaking into sub-targets");

		HandleSeq sub_premises = LinkCast(htarget)->getOutgoingSet();

		for (Handle& h : sub_premises)
			_targets_set.emplace(h, _garbage_superspace.add_atom(gen_sub_varlist(h, target.get_vardecl(), std::set<Handle>())));

		return;
	}

	/*************************************************/
	/**** This is where the actual BC step starts ****/
	/*************************************************/

	// Find all rules whose implicand can be unified to htarget
	std::vector<Rule> acceptable_rules = filter_rules(target);

	logger().debug("[BackwardChainer] %d rules unifiable", acceptable_rules.size());

	// If no rules to backward chain on, no way to solve this target
	if (acceptable_rules.empty())
		return;

	Rule selected_rule = select_rule(target, acceptable_rules);
	Rule standardized_rule =
		selected_rule.gen_standardize_apart(&_garbage_superspace);

	logger().debug("[BackwardChainer] Selected rule "
	               + standardized_rule.get_handle()->toShortString());

	Handle hrule_implicant = standardized_rule.get_implicant();
	Handle hrule_vardecl = standardized_rule.get_vardecl();
	HandleSeq qrule_outputs = standardized_rule.get_implicand_seq();

	std::vector<VarMap> all_implicand_to_target_mappings;

	// A rule can have multiple outputs, and more than one output will unify
	// to our target, so get all outputs that works
	for (Handle h : qrule_outputs)
	{
		VarMap temp_mapping;

		if (not unify(h,
		              htarget,
		              _garbage_superspace.add_atom(gen_sub_varlist(h, hrule_vardecl, std::set<Handle>())),
		              target.get_vardecl(),
		              temp_mapping))
			continue;

		all_implicand_to_target_mappings.push_back(temp_mapping);
	}

	// try sub-atom unification only if no whole output unification is possible
	if (all_implicand_to_target_mappings.empty())
	{
		logger().debug("[BackwardChainer] Trying sub-atom unification");

		UnorderedHandleSet output_expanded;
		for (Handle h : qrule_outputs)
		{
			UnorderedHandleSet hs = get_all_unique_atoms(h);
			output_expanded.insert(hs.begin(), hs.end());
			output_expanded.erase(h);
		}

		for (Handle h : output_expanded)
		{
			VarMap temp_mapping;

			if (not unify(h,
			              htarget,
			              _garbage_superspace.add_atom(gen_sub_varlist(h, hrule_vardecl, std::set<Handle>())),
			              target.get_vardecl(),
			              temp_mapping))
				continue;

			all_implicand_to_target_mappings.push_back(temp_mapping);
		}
	}

	logger().debug("[BackwardChainer] Found %d implicand's output unifiable",
	               all_implicand_to_target_mappings.size());

	// Randomly select one of the mapping (so that each time the
	// same target is visited, and the same rule is selected, it is
	// possible to select a different output to map to)
	//
	// XXX TODO use all possible output mapping instead; ie visit them
	// all, and add all resulting new targets to targets list; this will
	// avoid having to visit the target multiple times to get all
	// possible output mappings
	VarMap implicand_normal_mapping = rand_element(all_implicand_to_target_mappings);
	for (auto& p : implicand_normal_mapping)
		logger().debug("[BackwardChainer] Chosen mapping is "
		               + p.first->toShortString()
		               + " to " + p.second->toShortString());

	// Reverse ground the implicant with the grounding we found from
	// unifying the implicand
	Substitutor subt(&_garbage_superspace);
	std::vector<VarMap> premises_vmap_list, premises_vmap_list_alt;
	Handle hrule_implicant_normal_grounded = subt.substitute(hrule_implicant, implicand_normal_mapping);

	logger().debug("[BackwardChainer] Reverse grounded as "
	               + hrule_implicant_normal_grounded->toShortString());

	// Find all matching premises matching the implicant, where premises_vmap_list
	// will be the mapping from free variables in himplicant to stuff in a premise
	HandleSeq possible_premises =
		match_knowledge_base(hrule_implicant_normal_grounded,
	                         _garbage_superspace.add_atom(gen_sub_varlist(hrule_implicant_normal_grounded, hrule_vardecl, target.get_varset())),
	                         premises_vmap_list);

	// only need to generate QuoteLink version when there are free variables
	if (not target.get_varseq().empty())
	{
		// Generate another version where each variables (free or bound) are inside
		// QuoteLink; mostly to handle where PM cannot map a variable to itself
		VarMap implicand_quoted_mapping;
		for (auto& p : implicand_normal_mapping)
		{
			// find all variables
			FindAtoms fv(VARIABLE_NODE);
			fv.search_set(p.second);

			// wrap a QuoteLink on each variable
			VarMap quote_mapping;
			for (auto& h: fv.varset)
				quote_mapping[h] = _garbage_superspace.add_atom(createLink(QUOTE_LINK, h));

			Substitutor subt(&_garbage_superspace);
			implicand_quoted_mapping[p.first] = subt.substitute(p.second, quote_mapping);
		}

		// Reverse ground 2nd version, try it with QuoteLink around variables
		Handle hrule_implicant_quoted_grounded = subt.substitute(hrule_implicant, implicand_quoted_mapping);

		logger().debug("[BackwardChainer] Alternative reverse grounded as "
		               + hrule_implicant_quoted_grounded->toShortString());

		HandleSeq possible_premises_alt =
			match_knowledge_base(hrule_implicant_quoted_grounded,
								 _garbage_superspace.add_atom(gen_sub_varlist(hrule_implicant_quoted_grounded, hrule_vardecl, target.get_varset())),
								 premises_vmap_list_alt);

		// collect the possible premises from the two verions of mapping
		possible_premises.insert(possible_premises.end(),
		                         possible_premises_alt.begin(),
		                         possible_premises_alt.end());
		premises_vmap_list.insert(premises_vmap_list.end(), premises_vmap_list_alt.begin(),
		                          premises_vmap_list_alt.end());
	}

	logger().debug("[BackwardChainer] %d possible permises", possible_premises.size());

	// If no possible premises, then the reverse grounded rule's implicant
	// could be added as potential target.  Note that however, such target are
	// not yet in the main atomspace, since whatever the grounded implicant
	// is, it is possible that it is not valid (like the reverse of if-then
	// is not always true).  It will require some future steps to see if
	// another rule will generate the target and add it to the main atomspace.
	// Also note that these targets could contain variables from standardized
	// apart version of the rule, and should not be added to the main space.
	if (possible_premises.size() == 0)
	{
		logger().debug("[BackwardChainer] Adding rule's grounded input as Target");

		target.store_step(selected_rule, { hrule_implicant_normal_grounded });
		_targets_set.emplace(hrule_implicant_normal_grounded,
		                     _garbage_superspace.add_atom(gen_sub_varlist(hrule_implicant_normal_grounded, hrule_vardecl, target.get_varset())));
		return;
	}

	// For each set of possible premises, check if they already satisfy the
	// target, so that we can apply the rule while we are looking at it in the
	// same step
	for (size_t i = 0; i < possible_premises.size(); i++)
	{
		Handle hp = possible_premises[i];
		VarMap vm = premises_vmap_list[i];

		logger().debug("[BackwardChainer] Checking permises " + hp->toShortString());

		// reverse ground the rule's outputs with the mapping to the premise
		// so that when we ground the premise, we know how to generate
		// the final output
		Handle output_grounded = subt.substitute(standardized_rule.get_implicand(), implicand_normal_mapping);

		logger().debug("[BackwardChainer] Output reverse grounded step 1 as " + output_grounded->toShortString());
		output_grounded = subt.substitute(output_grounded, vm);

		logger().debug("[BackwardChainer] Output reverse grounded step 2 as " + output_grounded->toShortString());

		std::vector<VarMap> vm_list;

		// include the implicand mapping into vm so we can do variable chasing
		vm.insert(implicand_normal_mapping.begin(), implicand_normal_mapping.end());

		// use pattern matcher to try to ground the variables (if any) in the
		// selected premises, so we can use this grounding to "apply" the rule
		// to generate the rule's final output
		HandleSeq grounded_premises = ground_premises(hp, vm, vm_list);

		// Check each grounding to see if any has no variable
		for (size_t i = 0; i < grounded_premises.size(); ++i)
		{
			VarMultimap results;
			Handle& g = grounded_premises[i];
			VarMap& m = vm_list.at(i);

			logger().debug("[BackwardChainer] Checking possible permises grounding "
			               + g->toShortString());

			FindAtoms fv(VARIABLE_NODE);
			fv.search_set(g);

			// If some grounding cannot solve the goal, will need to BC
			if (not fv.varset.empty())
				continue;

			// This is a premise grounding that can solve the target, so
			// apply it by using the mapping to ground the target, and add
			// it to _as since this is not garbage; this should generate
			// all the outputs of the rule, and execute any evaluatable
			//
			// XXX TODO the TV of the original "Variable Fullfillment" target
			// need to be changed here... right?
			Instantiator inst(&_as);
			Handle added = inst.instantiate(output_grounded, m);

			logger().debug("[BackwardChainer] Added " + added->toShortString() + " to _as");

			// Add the grounding to the return results
			for (Handle& h : target.get_varseq())
				results[h].emplace(m.at(h));

			target.store_varmap(results);
		}

		// XXX TODO premise selection would be done here to
		// determine whether to BC on a premise



		// non-logical link can be added straight to targets list
		if (_logical_link_types.count(hp->getType()) == 0)
		{			
			target.store_step(selected_rule, { hp });
			_targets_set.emplace(hp, _garbage_superspace.add_atom(createVariableList(get_free_vars_in_tree(hp))));
			continue;
		}

		logger().debug("[BackwardChainer] Before breaking apart into sub-premises");

		// Else break out any logical link and add to targets
		HandleSeq sub_premises = LinkCast(hp)->getOutgoingSet();
		target.store_step(selected_rule, sub_premises);

		for (Handle& s : sub_premises)
			_targets_set.emplace(s, _garbage_superspace.add_atom(createVariableList(get_free_vars_in_tree(s))));
	}

	return;
}

/**
 * Find all rules in which the output could generate target.
 *
 * This method will try sub-atom unification if no whole output
 * unification is possible.
 *
 * @param htarget   the target to be generated by the rule's output
 * @return          a vector of rules
 */
std::vector<Rule> BackwardChainer::filter_rules(const Target& target)
{
	std::vector<Rule> rules;

	Handle htarget = target.get_handle();
	Handle htarget_vardecl = target.get_vardecl();

	for (Rule& r : _configReader.get_rules())
	{
		// unify against the standardized version, so the result will match
		// with what we will be applying against at later step
		Rule standardized_rule = r.gen_standardize_apart(&_garbage_superspace);

		Handle hrule_vardecl = standardized_rule.get_vardecl();
		HandleSeq output = standardized_rule.get_implicand_seq();
		bool unifiable = false;

		// check if any of the implicand's output can be unified to target
		for (Handle h : output)
		{
			VarMap mapping;

			if (not unify(h,
			              htarget,
			              _garbage_superspace.add_atom(gen_sub_varlist(h, hrule_vardecl, std::set<Handle>())),
			              htarget_vardecl,
			              mapping))
				continue;

			unifiable = true;
			break;
		}

		// if not unifiable, try sub-atom unification
		if (not unifiable)
		{
			UnorderedHandleSet output_expanded;
			for (Handle h : output)
			{
				UnorderedHandleSet hs = get_all_unique_atoms(h);
				output_expanded.insert(hs.begin(), hs.end());
				output_expanded.erase(h);
			}

			for (Handle h : output_expanded)
			{
				VarMap mapping;

				if (not unify(h,
				              htarget,
				              _garbage_superspace.add_atom(gen_sub_varlist(h, hrule_vardecl, std::set<Handle>())),
				              htarget_vardecl,
				              mapping))
					continue;

				unifiable = true;
				break;
			}
		}

		// move on to next rule if htarget cannot map to the output
		if (not unifiable)
			continue;

		rules.push_back(r);
	}

	return rules;
}

/**
 * Find all atoms in the AtomSpace matching the pattern.
 *
 * @param hpattern         the atom to pattern match against
 * @param hpattern_vardecl the typed VariableList of the variables in hpattern
 * @param vmap             an output list of mapping for variables in hpattern
 * @return                 a vector of matched atoms
 */
HandleSeq BackwardChainer::match_knowledge_base(const Handle& hpattern,
                                                Handle hpattern_vardecl,
                                                vector<VarMap>& vmap)
{
	// Get all VariableNodes (unquoted)
	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(hpattern);

	if (hpattern_vardecl == Handle::UNDEFINED)
	{
		HandleSeq vars;
		for (auto& h : fv.varset)
			vars.push_back(h);

		hpattern_vardecl = _garbage_superspace.add_atom(createVariableList(vars));
	}

	logger().debug("[BackwardChainer] Matching knowledge base with "
	               " %s and variables %s",
	               hpattern->toShortString().c_str(),
	               hpattern_vardecl->toShortString().c_str());

	if (VariableListCast(hpattern_vardecl)->get_variables().varseq.empty())
		return HandleSeq();

	// Pattern Match on _garbage_superspace since some atoms in hpattern could
	// be in the _garbage space
	PatternLinkPtr sl(createPatternLink(hpattern_vardecl, hpattern));
	BackwardChainerPMCB pmcb(&_garbage_superspace,
	                         VariableListCast(hpattern_vardecl));

	sl->satisfy(pmcb);

	vector<map<Handle, Handle>> var_solns = pmcb.get_var_list();
	vector<map<Handle, Handle>> pred_solns = pmcb.get_pred_list();

	HandleSeq results;

	logger().debug("[BackwardChainer] Pattern matcher found %d matches",
	               var_solns.size());

	for (size_t i = 0; i < var_solns.size(); i++)
	{
		HandleSeq i_pred_soln;

		// check for bad mapping
		for (auto& p : pred_solns[i])
		{
			// don't want matched clause that is part of a rule
			auto& rules = _configReader.get_rules();
			if (std::any_of(rules.begin(), rules.end(), [&](Rule& r) {
						return is_atom_in_tree(r.get_handle(), p.second); }))
			{
				logger().debug("[BackwardChainer] matched clause in rule");
				break;
			}

			// don't want matched stuff with some part of a rule inside
			if (std::any_of(rules.begin(), rules.end(), [&](Rule& r) {
						return is_atom_in_tree(p.second, r.get_handle()); }))
			{
				logger().debug("[BackwardChainer] matched clause wrapping rule");
				break;
			}

			// don't want matched clause that is not in the parent _as
			if (_as.get_atom(p.second) == Handle::UNDEFINED)
			{
				logger().debug("[BackwardChainer] matched clause not in _as");
				break;
			}

			i_pred_soln.push_back(p.second);
		}

		if (i_pred_soln.size() != pred_solns[i].size())
			continue;

		// if the original htarget is multi-clause, wrap the solution with the
		// same logical link
		// XXX TODO preserve htarget's order (but logical link are unordered...)
		Handle this_result;
		if (_logical_link_types.count(hpattern->getType()) == 1)
			this_result = _garbage_superspace.add_link(hpattern->getType(),
			                                           i_pred_soln);
		else
			this_result = i_pred_soln[0];

		results.push_back(this_result);
		vmap.push_back(var_solns[i]);
	}

	return results;
}

/**
 * Try to ground any free variables in the input target.
 *
 * @param hpremise      the input atom to be grounded
 * @param premise_vmap  the original mapping to the variables in hpremise
 * @param vmap_list     the final output mapping of the variables
 * @return              the mapping of the hpremise
 */
HandleSeq BackwardChainer::ground_premises(const Handle& hpremise,
                                           const VarMap& premise_vmap,
                                           std::vector<VarMap>& vmap_list)
{
	HandleSeq results;

	// XXX are all variables in premises free?
	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(hpremise);

	// if the target is already fully grounded
	if (fv.varset.empty())
	{
		vmap_list.push_back(premise_vmap);
		results.push_back(hpremise);

		return results;
	}

	Handle premises = hpremise;

	if (_logical_link_types.count(premises->getType()) == 1)
	{
		HandleSeq sub_premises;
		HandleSeq oset = LinkCast(hpremise)->getOutgoingSet();

		for (const Handle& h : oset)
		{
			// ignore premises with no free var
			if (get_free_vars_in_tree(h).empty())
				continue;

			sub_premises.push_back(h);
		}

		if (sub_premises.size() == 1)
			premises = sub_premises[0];
		else
			premises = _garbage_superspace.add_link(hpremise->getType(),
			                                        sub_premises);
	}

	logger().debug("[BackwardChainer] Grounding " + premises->toShortString());

	std::vector<VarMap> temp_vmap_list;

	HandleSeq temp_results = match_knowledge_base(premises, Handle::UNDEFINED, temp_vmap_list);

	// chase the variables so that if a variable A were mapped to another
	// variable B in premise_vmap, and after pattern matching, B now map
	// to some solution, change A to map to the same solution
	for (unsigned int i = 0; i < temp_results.size(); ++i)
	{
		VarMap& tvm = temp_vmap_list[i];
		VarMap this_map;

		for (const auto& p : premise_vmap)
		{
			if (tvm.count(p.second) == 1)
			{
				this_map[p.first] = tvm[p.second];
				this_map[p.second] = tvm[p.second];
				tvm.erase(p.second);
			}
			else
				this_map[p.first] = p.second;
		}

		// add any leftover mapping into final ouput
		this_map.insert(tvm.begin(), tvm.end());

		vmap_list.push_back(this_map);
		results.push_back(temp_results[i]);
	}

	return results;
}

/**
 * Unify two atoms, finding a mapping that makes them equal.
 *
 * Use the Pattern Matcher to do the heavy lifting of unification from one
 * specific atom to another, let it handles UnorderedLink, VariableNode in
 * QuoteLink, etc.
 *
 * This will in general unify htarget to hmatch in one direction.  However, it
 * allows a typed variable A in htarget to map to another variable B in hmatch,
 * in which case the mapping will be returned reverse (as B->A).
 *
 * @param hsource          the atom from which to unify
 * @param hmatch           the atom to which hsource will be unified to
 * @param hsource_vardecl  the typed VariableList of the variables in hsource
 * @param hmatch_vardecl   the VariableList of the free variables in hmatch
 * @param result           an output VarMap mapping varibles from hsource to hmatch
 * @return                 true if the two atoms can be unified
 */
bool BackwardChainer::unify(const Handle& hsource,
                            const Handle& hmatch,
                            Handle hsource_vardecl,
                            Handle hmatch_vardecl,
                            VarMap& result)
{
	// lazy way of restricting PM to be between two atoms
	AtomSpace temp_space;

	Handle temp_hsource = temp_space.add_atom(hsource);
	Handle temp_hmatch = temp_space.add_atom(hmatch);
	Handle temp_hsource_vardecl = temp_space.add_atom(hsource_vardecl);
	Handle temp_hmatch_vardecl = temp_space.add_atom(hmatch_vardecl);

	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(hsource);

	if (temp_hsource_vardecl == Handle::UNDEFINED)
	{
		HandleSeq vars;
		for (const Handle& h : fv.varset)
			vars.push_back(h);

		temp_hsource_vardecl = temp_space.add_atom(createVariableList(vars));
	}

	PatternLinkPtr sl(createPatternLink(temp_hsource_vardecl, temp_hsource));
	UnifyPMCB pmcb(&temp_space, VariableListCast(temp_hsource_vardecl), VariableListCast(temp_hmatch_vardecl));

	sl->satisfy(pmcb);

	// if no grounding
	if (pmcb.get_var_list().size() == 0)
		return false;

	std::vector<std::map<Handle, Handle>> pred_list = pmcb.get_pred_list();
	std::vector<std::map<Handle, Handle>> var_list = pmcb.get_var_list();

	VarMap good_map;

	// go thru each solution, and get the first one that map the whole temp_hmatch
	//
	// XXX TODO branch on the various groundings?  how to properly handle
	// multiple possible unify option????
	for (size_t i = 0; i < pred_list.size(); ++i)
	{
		for (const auto& p : pred_list[i])
		{
			if (is_atom_in_tree(p.second, temp_hmatch))
			{
				good_map = var_list[i];
				i = pred_list.size();
				break;
			}
		}
	}

	// if none of the mapping map the whole temp_hmatch (possible in the case
	// of sub-atom unification that map a typed variable to another variable)
	if (good_map.size() == 0)
		return false;

	// change the mapping from temp_atomspace to current atomspace
	for (auto& p : good_map)
	{
		Handle var = p.first;
		Handle grn = p.second;

		result[_garbage_superspace.get_atom(var)] =
			_garbage_superspace.get_atom(grn);
	}

	return true;
}

/**
 * Select a candidate rule from the set of filtered rules.
 *
 * XXX TODO use the rule weight
 * XXX should these selection functions be in callbacks like the ForwardChainer?
 *
 * @param target  the original target the set of filtered rules are unifiable to
 * @param rules   a vector of filtered rules to select from
 * @return        one of the rule
 */
Rule BackwardChainer::select_rule(Target& target, const std::vector<Rule>& rules)
{
	// store how many times each rule has been used for the target
	std::vector<unsigned int> weights;
	std::for_each(rules.begin(), rules.end(),
	              [&](const Rule& r)
	              { weights.push_back(target.get_selection_count() - target.rule_count(r) + 1); });

	// Select the rule that has been applied least
	// XXX use cogutil MT19937RandGen's internal randomGen member possible?
	std::mt19937 generator(std::chrono::system_clock::now().time_since_epoch().count());
	std::discrete_distribution<int> distribution(weights.begin(), weights.end());

	return rules[distribution(generator)];
}

/**
 * Given a VariableList, generate a new VariableList of only the specific vars.
 *
 * Mostly to keep the typed definition from the original VariableList.  Also
 * put any "free" variables not inside the original VariableList in the new
 * list.  The "free" variables are passed in as a parameter.
 *
 * VariableNodes not in the original VariableList nor in the free_varset will
 * be considered bound already.
 *
 * @param parent                  the atom the VariableList was for
 * @param parent_varlist          the original VariableList
 * @param additional_free_varset  a set of free VariableNodes to be included
 * @return                        the new sublist
 */
Handle BackwardChainer::gen_sub_varlist(const Handle& parent,
                                        const Handle& parent_varlist,
                                        std::set<Handle> additional_free_varset)
{
	HandleSeq varseq = get_free_vars_in_tree(parent);
	std::set<Handle> varset(varseq.begin(), varseq.end());

	HandleSeq oset = LinkCast(parent_varlist)->getOutgoingSet();
	HandleSeq final_oset;

	// for each var in varlist, check if it is used in parent
	for (const Handle& h : oset)
	{
		Type t = h->getType();
		if (VARIABLE_NODE == t && varset.count(h) == 1)
		{
			final_oset.push_back(h);
			additional_free_varset.erase(h);
		}
		else if (TYPED_VARIABLE_LINK == t
			     and varset.count(LinkCast(h)->getOutgoingSet()[0]) == 1)
		{
			final_oset.push_back(h);
			additional_free_varset.erase(LinkCast(h)->getOutgoingSet()[0]);
		}
	}

	// for each var left in the additional_free_varset, check
	// if it is used in parent
	for (const Handle& h : additional_free_varset)
	{
		if (varset.count(h) == 1)
			final_oset.push_back(h);
	}

	return Handle(createVariableList(final_oset));
}

