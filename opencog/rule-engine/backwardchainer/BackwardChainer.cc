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
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atoms/bind/SatisfactionLink.h>

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace* as, std::vector<Rule> rs)
    : _as(as)
{
	_rules_set = rs;

	// create a garbage superspace with _as as parent, so codes acting on
	// _garbage will see stuff in _as, but codes acting on _as will not
	// see stuff in _garbage
	_garbage_superspace = new AtomSpace(_as);
}

BackwardChainer::~BackwardChainer()
{
	// this will presumably remove all temp atoms
	delete _garbage_superspace;
}

/**
 * Set the initial target for backward chaining.
 *
 * @param init_target   Handle of the target
 */
void BackwardChainer::set_target(Handle init_target)
{
	_init_target = init_target;

	_inference_history.clear();

	_targets_set = UnorderedHandleSet();
	_targets_set.insert(_init_target);
}

/**
 * The public entry point for full backward chaining.
 *
 * @param max_steps   The maximum number of backward chain steps
 *
 * XXX TODO add more stopping param like fitness criterion, etc
 */
void BackwardChainer::do_until(uint max_steps)
{
	uint i = 0;

	while (i < max_steps)
	{
		do_step();
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

	// XXX TODO do proper target selection here using some fitness function
	Handle selected_target = rand_element(_targets_set);

	VarMultimap subt = process_target(selected_target);
	VarMultimap& old_subt = _inference_history[selected_target];

	logger().debug("[BackwardChainer] End of a single BC step");

	// add the substitution to inference history
	for (auto& p : subt)
		old_subt[p.first].insert(p.second.begin(), p.second.end());
}

/**
 * Get the current result on the initial target, if any.
 *
 * @return a VarMultimap mapping each variable to all possible solutions
 */
VarMultimap& BackwardChainer::get_chaining_result()
{
	return _inference_history[_init_target];
}

/**
 * The main recursive backward chaining method.
 *
 * @param htarget  the atom to do backward chaining on
 * @return         the solution found for this target, if any
 */
VarMultimap BackwardChainer::process_target(Handle& htarget)
{
	VarMultimap results;

	HandleSeq free_vars = get_free_vars_in_tree(htarget);

	// Check whether this goal has free variables and worth exploring
	if (free_vars.empty())
	{
		logger().debug("[BackwardChainer] Boring goal with no free var, "
		               "skipping " + htarget->toShortString());
		return results;
	}

	// Check whether this goal is a virtual link and is useless to explore
	if (classserver().isA(htarget->getType(), VIRTUAL_LINK))
	{
		logger().debug("[BackwardChainer] Boring virtual link goal, "
		               "skipping " + htarget->toShortString());
		return results;
	}

	std::vector<VarMap> kb_vmap;

	// Else, try to ground, and backward chain
	HandleSeq kb_match = match_knowledge_base(htarget, Handle::UNDEFINED,
	                                          true, kb_vmap);

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

			// Check if there is any free variables in soln
			HandleSeq free_vars = get_free_vars_in_tree(soln);

			// If there are free variables, add this soln to the target stack
			if (not free_vars.empty())
				_targets_set.insert(soln);

			// Construct the htarget to all mappings here to be returned
			for (auto it = vgm.begin(); it != vgm.end(); ++it)
				results[it->first].emplace(it->second);
		}
	}

	// If logical link, break it up, add each to the targets
	// stack, and return
	if (_logical_link_types.count(htarget->getType()) == 1)
	{
		HandleSeq sub_premises = LinkCast(htarget)->getOutgoingSet();

		for (Handle& h : sub_premises)
			_targets_set.insert(h);

		return results;
	}

	/*************************************************/
	/**** This is where the actual BC step starts ****/
	/*************************************************/

	// Find all rules whose implicand can be unified to htarget
	std::vector<Rule> acceptable_rules = filter_rules(htarget);

	// If no rules to backward chain on, no way to solve this goal
	if (acceptable_rules.empty())
		return results;

	Rule standardized_rule = select_rule(acceptable_rules).gen_standardize_apart(_garbage_superspace);

	logger().debug("[BackwardChainer] Selected rule " + standardized_rule.get_handle()->toShortString());

	Handle himplicant = standardized_rule.get_implicant();
	Handle hvardecl = standardized_rule.get_vardecl();
	HandleSeq outputs = standardized_rule.get_implicand();

	std::vector<VarMap> all_mappings;

	// A rule can have multiple outputs, and more than one output will unify
	// to our goal, so get all outputs that works
	for (Handle h : outputs)
	{
		VarMap temp_mapping;

		if (not unify(h, htarget, hvardecl, temp_mapping))
			continue;

		all_mappings.push_back(temp_mapping);
	}

	logger().debug("[BackwardChainer] Found %d implicand's output unifiable",
	               all_mappings.size());

	// Randomly select one of the mapping (so that each time the
	// same target is visited, and the same rule is selected, it is
	// possible to select a different output to map to)
	//
	// XXX TODO use all possible output mapping instead; ie visit them
	// all, and add all resulting new targets to targets list; this will
	// avoid having to visit the target multiple times to get all
	// possible output mappings
	VarMap implicand_normal_mapping = rand_element(all_mappings);
	for (auto& p : implicand_normal_mapping)
		logger().debug("[BackwardChainer] Chosen mapping is "
					   + p.first->toShortString()
					   + " to " + p.second->toShortString());

	// Generate another version where each variables are inside QuoteLink;
	// this is mostly to handle where PM cannot map a variable to itself
	VarMap implicand_quoted_mapping;
	for (auto& p : implicand_normal_mapping)
	{
		// find all variables
		FindAtoms fv(VARIABLE_NODE);
		fv.search_set(p.second);

		// wrap a QuoteLink on each variable
		VarMap quote_mapping;
		for (auto& h: fv.varset)
			quote_mapping[h] = _garbage_superspace->addAtom(createLink(QUOTE_LINK, h));

		Instantiator inst(_garbage_superspace);
		implicand_quoted_mapping[p.first] = inst.instantiate(p.second, quote_mapping);

		logger().debug("[BackwardChainer] Added "
					   + implicand_quoted_mapping[p.first]->toShortString()
					   + " to garbage space");
	}

	// Reverse ground the implicant with the grounding we found from
	// unifying the implicand
	Instantiator inst(_garbage_superspace);
	std::vector<VarMap> premises_vmap_list, premises_vmap_list_alt;
	Handle himplicant_normal = inst.instantiate(himplicant, implicand_normal_mapping);

	logger().debug("[BackwardChainer] Reverse grounded as "
				   + himplicant_normal->toShortString());

	// Find all matching premises matching the implicant
	HandleSeq possible_premises =
		match_knowledge_base(himplicant_normal, hvardecl, false, premises_vmap_list);

	// Reverse ground 2nd version, try it with QuoteLink around variables
	Handle himplicant_quoted = inst.instantiate(himplicant, implicand_quoted_mapping);

	logger().debug("[BackwardChainer] Alternative everse grounded as "
				   + himplicant_quoted->toShortString());

	HandleSeq possible_premises_alt =
	    match_knowledge_base(himplicant_quoted, hvardecl, false, premises_vmap_list_alt);

	// collect the possible premises from the two verions of mapping
	possible_premises.insert(possible_premises.end(),
	                         possible_premises_alt.begin(),
	                         possible_premises_alt.end());
	premises_vmap_list.insert(premises_vmap_list.end(), premises_vmap_list_alt.begin(),
	                 premises_vmap_list_alt.end());

	logger().debug("%d possible permises", possible_premises.size());

	// For each set of possible premises, check if they already
	// satisfy the goal, so that we can apply the rule while
	// we are looking at it in the same step
	for (size_t i = 0; i < possible_premises.size(); i++)
	{
		Handle hp = possible_premises[i];
		VarMap vm = premises_vmap_list[i];

		// include the implicand_normal_mapping because the variables are not quoted
		// and we will be matching the variables in there as well
		vm.insert(implicand_normal_mapping.begin(), implicand_normal_mapping.end());

		HandleSeq outputs_grounded;

		// reverse ground the rule's outputs the mapping to the premise
		// so that when we ground the premise, we know how to generate
		// the final output
		for (const Handle& ho : outputs)
			outputs_grounded.push_back(inst.instantiate(ho, vm));

		logger().debug("Checking permises " + hp->toShortString());

		bool need_bc = false;
		std::vector<VarMap> vm_list;

		// use pattern matcher to try to ground the variables in the
		// selected premises, so we can use this grounding to "apply"
		// the rule to generate the rule's final output
		HandleSeq grounded_premises = ground_premises(hp, vm, vm_list);

		// matched nothing? need to backward chain on this premise
		if (grounded_premises.size() == 0)
			need_bc = true;

		// Check each grounding to see if any has no variable
		for (size_t i = 0; i < grounded_premises.size(); ++i)
		{
			Handle& g = grounded_premises[i];
			VarMap& m = vm_list[i];

			logger().debug("Checking possible permises grounding "
						   + g->toShortString());

			FindAtoms fv(VARIABLE_NODE);
			fv.search_set(g);

			// If some grounding cannot solve the goal, will need to BC
			if (not fv.varset.empty())
			{
				need_bc = true;
				continue;
			}

			// This is a premise grounding that can solve the target, so
			// apply it by using the mapping to ground the target, and add
			// it to _as since this is not garbage; this should generate
			// all the outputs of the rule
			for (const Handle& ho : outputs_grounded)
			{
				Instantiator inst(_as);
				inst.instantiate(ho, m);
			}

			// Add the grounding to the return results
			for (Handle& h : free_vars)
				results[h].emplace(m[h]);
		}

		if (not need_bc)
			continue;



		// XXX TODO premise selection would be done here to
		// determine whether to BC on a premise



		// non-logical link can be added straight to targets list
		if (_logical_link_types.count(hp->getType()) == 0)
		{
			_targets_set.insert(hp);
			continue;
		}

		// Else break out any logical link and add to targets
		HandleSeq sub_premises = LinkCast(hp)->getOutgoingSet();

		for (Handle& s : sub_premises)
			_targets_set.insert(s);
	}

	return results;
}

/**
 * Find all rules in which the output could generate target.
 *
 * @param htarget   the target to be generated by the rule's output
 * @return          a vector of rules
 */
std::vector<Rule> BackwardChainer::filter_rules(Handle htarget)
{
	std::vector<Rule> rules;

	for (Rule& r : _rules_set)
	{
		Handle vardecl = r.get_vardecl();
		HandleSeq output = r.get_implicand();
		bool unifiable = false;

		// check if any of the implicand's output can be unified to target
		for (Handle h : output)
		{
			VarMap mapping;

			if (not unify(h, htarget, vardecl, mapping))
				continue;

			unifiable = true;
			break;
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
 * @param htarget          the atom to pattern match against
 * @param htarget_vardecl  the typed VariableList of the variables in htarget
 * @param check_history    flag to indicate whether to match stuff in history
 * @param vmap             an output list of mapping for variables in htarget
 * @return                 a vector of matched atoms
 *
 * XXX TODO double check the check_history usage is a good idea or not
 */
HandleSeq BackwardChainer::match_knowledge_base(const Handle& htarget,
                                                Handle htarget_vardecl,
                                                bool check_history,
                                                vector<VarMap>& vmap)
{
	// Get all VariableNodes (unquoted)
	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(htarget);

	if (htarget_vardecl == Handle::UNDEFINED)
	{
		HandleSeq vars;
		for (auto& h : fv.varset)
			vars.push_back(h);

		htarget_vardecl = _garbage_superspace->addAtom(createVariableList(vars));
	}
	else
		htarget_vardecl = _garbage_superspace->addAtom(gen_sub_varlist(htarget_vardecl, fv.varset));

	logger().debug("[BackwardChainer] Matching knowledge base with "
	               " %s and variables %s",
	               htarget->toShortString().c_str(),
	               htarget_vardecl->toShortString().c_str());

	// Pattern Match on _garbage_superspace since some atoms in htarget could
	// be in the _garbage space
	SatisfactionLinkPtr sl(createSatisfactionLink(htarget_vardecl, htarget));
	BackwardChainerPMCB pmcb(_garbage_superspace);

	logger().debug("[BackwardChainer] Before patterm matcher");

	sl->satisfy(pmcb);

	logger().debug("[BackwardChainer] After running pattern matcher");

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
			if (std::any_of(_rules_set.begin(), _rules_set.end(), [&](Rule& r) {
						return is_atom_in_tree(r.get_handle(), p.second); }))
			{
				logger().debug("[BackwardChainer] matched clause in rule");
				break;
			}

			// don't want matched clause that is not in the parent _as
			if (_as->getAtom(p.second) == Handle::UNDEFINED)
			{
				logger().debug("[BackwardChainer] matched clause not in _as");
				break;
			}

			// don't want matched clause already in inference history
			if (check_history && _inference_history.count(p.second) == 1)
			{
				logger().debug("[BackwardChainer] matched clause in history");
				break;
			}

			// XXX don't want clause that are already in _targets_set?
			// no need? since things on targets set are in inference history
			// but only if the target is been inferenced upon...

			i_pred_soln.push_back(p.second);
		}

		if (i_pred_soln.size() != pred_solns[i].size())
			continue;

		// if the original htarget is multi-clause, wrap the solution with the
		// same logical link
		// XXX TODO preserve htarget's order
		Handle this_result;
		if (_logical_link_types.count(htarget->getType()) == 1)
			this_result = _garbage_superspace->addLink(htarget->getType(),
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
			premises = _garbage_superspace->addLink(hpremise->getType(),
			                                        sub_premises);
	}

	logger().debug("[BackwardChainer] Grounding " + premises->toShortString());

	std::vector<VarMap> temp_vmap_list;

	results = match_knowledge_base(premises, Handle::UNDEFINED, false, temp_vmap_list);

	// chase the variables so that if a variable A were mapped to another
	// variable B in premise_vmap, and after pattern matching, B now map
	// to some solution, change A to map to the same solution
	for (VarMap& tvm : temp_vmap_list)
	{
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
 * XXX Should (Link (Node A)) be unifiable to (Node A))?  BC literature never
 * unify this way, but in AtomSpace context, (Link (Node A)) does contain (Node A)
 *
 * @param hsource          the atom from which to unify
 * @param hmatch           the atom to which hsource will be unified to
 * @param htarget_vardecl  the typed VariableList of the variables in hsource
 * @param result           an output VarMap mapping varibles from hsource to hmatch
 * @return                 true if the two atoms can be unified
 */
bool BackwardChainer::unify(const Handle& hsource,
                            const Handle& hmatch,
                            Handle hsource_vardecl,
                            VarMap& result)
{
	logger().debug("[BackwardChainer] starting unify " + hsource->toShortString()
	               + " to " + hmatch->toShortString());

	// lazy way of restricting PM to be between two atoms
	AtomSpace temp_space;

	Handle temp_hsource = temp_space.addAtom(hsource);
	Handle temp_hmatch = temp_space.addAtom(hmatch);
	Handle temp_vardecl;

	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(hsource);

	if (hsource_vardecl == Handle::UNDEFINED)
	{
		HandleSeq vars;
		for (const Handle& h : fv.varset)
			vars.push_back(h);

		temp_vardecl = temp_space.addAtom(createVariableList(vars));
	}
	else
		temp_vardecl = temp_space.addAtom(gen_sub_varlist(hsource_vardecl,
		                                                  fv.varset));

	SatisfactionLinkPtr sl(createSatisfactionLink(temp_vardecl, temp_hsource));
	UnifyPMCB pmcb(&temp_space);

	sl->satisfy(pmcb);

	logger().debug("[BackwardChainer] unify found %d mapping",
	               pmcb.get_var_list().size());

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

	// change the mapping from temp_atomspace to current atomspace
	for (auto& p : good_map)
	{
		Handle var = p.first;
		Handle grn = p.second;

		result[_garbage_superspace->getAtom(var)] =
			_garbage_superspace->getAtom(grn);
	}

	return true;
}

/**
 * Given a target, select a candidate rule.
 *
 * XXX TODO apply selection criteria to select one amongst the matching rules
 * XXX better implement target selection first before trying to implement this!
 *
 * @param rules   a vector of rules to select from
 * @return        one of the rule
 */
Rule BackwardChainer::select_rule(const std::vector<Rule>& rules)
{
	//xxx return random for the purpose of integration testing before going
	//for a complex implementation of this function
	return rand_element(rules);
}

/**
 * Given a VariableList, generate a new VariableList of only the specific vars.
 *
 * Also put any variables not inside the original VariableList in the new list.
 *
 * @param parent_varlist  the original VariableList
 * @param varset          a set of VariableNodes to be included
 * @return                the new sublist
 */
Handle BackwardChainer::gen_sub_varlist(const Handle& parent_varlist,
                                        std::set<Handle> varset)
{
	HandleSeq oset = LinkCast(parent_varlist)->getOutgoingSet();
	HandleSeq final_oset;

	// for each var, check if it is in varset
	for (const Handle& h : oset)
	{
		Type t = h->getType();
		if (VARIABLE_NODE == t && varset.count(h) == 1)
		{
			final_oset.push_back(h);
			varset.erase(h);
		}
		else if (TYPED_VARIABLE_LINK == t
			     and varset.count(LinkCast(h)->getOutgoingSet()[0]) == 1)
		{
			final_oset.push_back(h);
			varset.erase(LinkCast(h)->getOutgoingSet()[0]);
		}
	}

	// add any leftover variables into the list
	for (const Handle& h : varset)
		final_oset.push_back(h);

	return Handle(createVariableList(final_oset));
}

