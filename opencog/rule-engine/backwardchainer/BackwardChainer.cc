/*
 * BackwardChainer.cc
 *
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

#include <opencog/util/random.h>

#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/atoms/pattern/PatternLink.h>

#include <opencog/query/BindLinkAPI.h>

#include "BackwardChainer.h"
#include "BackwardChainerPMCB.h"
#include "UnifyPMCB.h"
#include "BCLogger.h"

using namespace opencog;

BackwardChainer::BackwardChainer(AtomSpace& as, const Handle& rbs)
	: _as(as), _configReader(as, rbs),
	  // create a garbage superspace with _as as parent, so codes
	  // acting on _garbage_superspace will see stuff in _as, but
	  // codes acting on _as will not see stuff in _garbage_superspace
	  _garbage_superspace(&_as),
	  _iteration(0),
	  _rules(_configReader.get_rules()) {}

/**
 * Set the initial target for backward chaining.
 *
 * @param init_target   Handle of the target
 * @param focus_link    The SetLink containing the optional focus set.
 */
void BackwardChainer::set_target(const Handle& init_target,
                                 const Handle& focus_link)
{
	OC_ASSERT(false, "TODO");

	// _init_target = init_target;

	// _targets_set.clear();
	// _focus_space.clear();

	// _targets_set.emplace(_init_target, gen_varlist(_init_target));

	// // get the stuff under the SetLink
	// if (focus_link and focus_link->isLink())
	// {
	// 	HandleSeq focus_set = focus_link->getOutgoingSet();
	// 	for (const auto& h : focus_set)
	// 		_focus_space.add_atom(h);

	// 	// the target itself should be part of the focus set
	// 	if (focus_set.size() > 0)
	// 		_focus_space.add_atom(init_target);
	// }
}

UREConfigReader& BackwardChainer::get_config()
{
	return _configReader;
}

const UREConfigReader& BackwardChainer::get_config() const
{
	return _configReader;
}

void BackwardChainer::do_chain()
{
	while (not termination())
	{
		do_step();
	}
}

bool BackwardChainer::termination()
{
	return _configReader.get_maximum_iterations() <= _iteration;
}

/**
 * Do a single step of backward chaining.
 */
void BackwardChainer::do_step()
{	
	bc_logger().debug("Iteration %d", _iteration);
	_iteration++;

	// Select target
	Target& target = select_target();
	// LAZY_BC_LOG_DEBUG << "Target:" << std::endl << target.to_string();

	// Select a valid rule
	const Rule& rule = select_rule(target);

	// Expand the back-inference tree of this target
	expand_bit(target, rule);

	// Fulfill target
	fulfill_target(target);
}

void BackwardChainer::expand_bit(Target& target, const Rule& rule)
{
	// TODO
}

Target& BackwardChainer::select_target()
{
	// For now selection is uniformly random
	return rand_element(_target_set).second;
}

const Rule& BackwardChainer::select_rule(const Target& target)
{
	// For now the rule is uniformly randomly selected amongst the
	// valid ones
	return *rand_element(get_valid_rules(target));
}

vector<const Rule*> BackwardChainer::get_valid_rules(const Target& target)
{
	vector<const Rule*> valid_rules;
	for (const Rule& rule : _rules)
		if (match_conclusion(target, rule))
			valid_rules.push_back(&rule);
	return valid_rules;
}

bool BackwardChainer::match_conclusion(const Target& target, const Rule& rule)
{
	for (const HandlePair& hp : rule.get_conclusions())
		if (unify(target.handle, Handle::UNDEFINED, hp.first, hp.second))
		    return true;
	return false;
}

void BackwardChainer::fulfill_target(Target& target)
{
	// TODO
}

bool BackwardChainer::unify(const Handle& target, const Handle& pattern,
                            const Handle& pattern_vardecl)
{
	AtomSpace tmp_as;
	Handle tmp_target = tmp_as.add_atom(target),
		tmp_bl = tmp_as.add_link(BIND_LINK, pattern_vardecl, pattern, pattern),
		result = bindlink(&tmp_as, tmp_bl);
	HandleSeq results = result->getOutgoingSet();
	return std::find(results.begin(), results.end(), tmp_target) != results.end();
}

/**
 * Unify two atoms, finding a mapping that makes them equal.
 *
 * Use the Pattern Matcher to do the heavy lifting of unification from one
 * specific atom to another, let it handles UnorderedLink, VariableNode in
 * QuoteLink, etc.
 *
 * This will in general unify pattern to target in one direction.  However, it
 * allows a typed variable A in htarget to map to another variable B in hmatch,
 * in which case the mapping will be returned reverse (as B->A).
 *
 * @param target           the atom from which to unify
 * @param pattern          the atom to which hsource will be unified to
 * @param target_vardecl   the typed VariableList of the variables in hsource
 * @param pattern_vardecl  the VariableList of the free variables in hmatch
 * @param result           an output HandleMap mapping varibles from hsource to hmatch
 * @return                 true if the two atoms can be unified
 */
bool BackwardChainer::unify(const Handle& target,
                            const Handle& pattern,
                            const Handle& target_vardecl,
                            const Handle& pattern_vardecl,
                            HandleMap& result)
{
	// Lazy way of restricting PM to be between two atoms
	AtomSpace tmp_space;

	Handle tmp_target = tmp_space.add_atom(target);
	Handle tmp_pattern = tmp_space.add_atom(pattern);
	Handle tmp_target_vardecl = tmp_space.add_atom(target_vardecl);
	Handle tmp_pattern_vardecl = tmp_space.add_atom(pattern_vardecl);

	VariableListPtr tmp_target_vardecl_vlp = gen_varlist(tmp_target, tmp_target_vardecl);
	VariableListPtr tmp_pattern_vardecl_vlp = gen_varlist(tmp_pattern, tmp_pattern_vardecl);

	tmp_target_vardecl = tmp_space.add_atom(tmp_target_vardecl_vlp);
	tmp_pattern_vardecl = tmp_space.add_atom(tmp_pattern_vardecl_vlp);

	PatternLinkPtr sl(createPatternLink(tmp_pattern_vardecl, tmp_pattern));
	UnifyPMCB pmcb(&tmp_space, tmp_pattern_vardecl_vlp, tmp_target_vardecl_vlp);

	sl->satisfy(pmcb);

	// If no grounding
	if (pmcb.get_var_list().size() == 0)
		return false;

	HandleMapSeq pred_list = pmcb.get_pred_list();
	HandleMapSeq var_list = pmcb.get_var_list();

	HandleMap good_map;

	// Go thru each solution, and get the first one that map the whole
	// temp_pattern
	//
	// XXX TODO branch on the various groundings?  how to properly handle
	// multiple possible unify option????
	for (size_t i = 0; i < pred_list.size(); ++i)
	{
		for (const auto& p : pred_list[i])
		{
			if (is_atom_in_tree(p.second, tmp_target))
			{
				good_map = var_list[i];
				i = pred_list.size();
				break;
			}
		}
	}

	// If none of the mapping map the whole temp_pattern (possible in the case
	// of sub-atom unification that map a typed variable to another variable)
	if (good_map.empty())
		return false;

	// Change the mapping from temp_atomspace to current atomspace
	for (const auto& p : good_map)
	{
		Handle var = p.first;
		Handle grn = p.second;

		result[_garbage_superspace.get_atom(var)] =
			_garbage_superspace.get_atom(grn);
	}

	return true;
}

bool BackwardChainer::unify(const Handle& target,
                            const Handle& pattern,
                            const Handle& target_vardecl,
                            const Handle& pattern_vardecl)
{
	HandleMap tmp;
	return unify(target, pattern, target_vardecl, pattern_vardecl, tmp);
}

bool BackwardChainer::sym_unify(const Handle& lhs,
                                const Handle& rhs,
                                const Handle& lhs_vardecl,
                                const Handle& rhs_vardecl)
{
	return unify(lhs, rhs, lhs_vardecl, rhs_vardecl)
		or unify(rhs, lhs, rhs_vardecl, lhs_vardecl);
}

/**
 * Get the current result on the initial target, if any.
 *
 * @return a HandleMultimap mapping each variable to all possible solutions
 */
HandleMultimap BackwardChainer::get_chaining_result()
{
	OC_ASSERT(false, "TODO");
	HandleMultimap temp_result;// = _target_set.get(_init_target).get_varmap();
	HandleMultimap result;
	for (auto& p : temp_result)
	{
		UnorderedHandleSet s;
		for (auto& h : p.second)
			s.insert(_as.get_atom(h));
		result[_as.get_atom(p.first)] = s;
	}

	return result;
}

/**
 * Generate a VariableList of the free variables of a given atom h.
 */
VariableListPtr BackwardChainer::gen_varlist(const Handle& h)
{
	OrderedHandleSet vars = get_free_variables(h);
	return createVariableList(HandleSeq(vars.begin(), vars.end()));
}

/**
 * Given an atom h and its variable declaration vardecl, turn the
 * vardecl into a VariableList if not already, and if undefined,
 * generate a VariableList of the free variables of h.
 */
VariableListPtr BackwardChainer::gen_varlist(const Handle& h, const Handle& vardecl)
{
	if (vardecl == Handle::UNDEFINED)
		return gen_varlist(h);
	else {
		Type vardecl_t = vardecl->getType();
		if (vardecl_t == VARIABLE_LIST)
			return VariableListCast(vardecl);
		else {
			OC_ASSERT(vardecl_t == VARIABLE_NODE
			          or vardecl_t == TYPED_VARIABLE_LINK);
			return createVariableList(HandleSeq(1, vardecl));
		}
	}
}

#if 0
void BackwardChainer::do_step_old()
{
	bc_logger().debug("Start single BC step.");
	bc_logger().debug("Iteration %d", _iteration);
	_iteration++;

	bc_logger().debug("%d potential targets", _targets_set.size());
	bc_logger().debug("%d in focus set", _focus_space.get_size());

	// do target selection using some criteria
	// XXX for example, choose target with low TV 50% of the time
	Target& selected_target = _targets_set.select();

	LAZY_BC_LOG_DEBUG << "Selected target:" << std::endl
	                  << selected_target.get_handle()->toShortString()
	                  << "With var_decl:" << std::endl
	                  << selected_target.get_vardecl()->toShortString();

	if (selected_target.get_varseq().empty())
		bc_logger().debug("Target is 'Truth Value Query'");
	else
		bc_logger().debug("Target is 'Variable Fullfillment Query'");

	process_target(selected_target);

	// Clear the garbage space to avoid lingering copy of atoms that exist
	// in both the garbage and main atomspace
	_garbage_superspace.clear();

	bc_logger().debug("End single BC step");
}

/**
 * The main recursive backward chaining method.
 *
 * @param target   the Target object containing the target atom
 */
void BackwardChainer::process_target(Target& target)
{
	Handle htarget = _garbage_superspace.add_atom(target.get_handle());
	Handle htarget_vardecl = _garbage_superspace.add_atom(target.get_vardecl());

	// Check whether this target is a virtual link and is useless to explore
	if (classserver().isA(htarget->getType(), VIRTUAL_LINK))
	{
		LAZY_BC_LOG_DEBUG << "Boring virtual link goal, skipping:" << std::endl
		                  << htarget->toShortString();
		return;
	}

	// Check whether this target is a logical link and everything inside are
	// virtual, and therefore useless to explore (PM cannot match it)
	if (_logical_link_types.count(htarget->getType()) == 1)
	{
		bool all_virtual = true;
		for (const Handle& h : htarget->getOutgoingSet())
		{
			if (classserver().isA(h->getType(), VIRTUAL_LINK))
				continue;

			all_virtual = false;
			break;
		}

		if (all_virtual)
		{
			LAZY_BC_LOG_DEBUG << "Boring logical link all virtual, skipping "
			                  << std::endl << htarget->toShortString();
			return;
		}
	}

	// before doing any real backward chaining, see if any variables in
	// vardecl can already be grounded
	if (not target.get_varseq().empty())
	{
		HandleMapSeq kb_vmap;

		HandleSeq kb_match = match_knowledge_base(htarget, htarget_vardecl,
		                                          kb_vmap);

		// Matched something in the knowledge base? Then need to store
		// any grounding as a possible solution for this target
		if (not kb_match.empty())
		{
			bc_logger().debug("Matched something in knowledge base, "
			                  "storing the grounding");

			for (size_t i = 0; i < kb_match.size(); ++i)
			{
				Handle& soln = kb_match[i];
				HandleMap& vgm = kb_vmap[i];

				LAZY_BC_LOG_DEBUG << "Looking at grounding:" << std::endl
				                  << soln->toShortString();

				// add whatever it matched as Target (so new variables can be
				// filled, and TV updated)
				_targets_set.emplace(soln, gen_varlist(soln));

				target.store_varmap(vgm);
			}
		}
	}

	// If logical link, break it up, add each to new targets and return
	if (_logical_link_types.count(htarget->getType()) == 1)
	{
		bc_logger().debug("Breaking into sub-targets");

		HandleSeq sub_premises = htarget->getOutgoingSet();

		for (Handle& h : sub_premises)
			_targets_set.emplace(h, gen_sub_varlist(h, htarget_vardecl,
			                                        OrderedHandleSet()));

		return;
	}

	/*************************************************/
	/**** This is where the actual BC step starts ****/
	/*************************************************/

	Rule selected_rule;
	Rule standardized_rule;
	HandleMapSeq all_implicand_to_target_mappings;

	// If no rules to backward chain on, no way to solve this target
	if (not select_rule(target, selected_rule, standardized_rule,
	                    all_implicand_to_target_mappings))
		return;

	bc_logger().debug("Selected rule %s", selected_rule.get_name().c_str());
	LAZY_BC_LOG_DEBUG << "Standardized rule:" << std::endl
	                  << standardized_rule.get_forward_rule()->toShortString();
	bc_logger().debug("Found %d implicand's output unifiable",
	                  all_implicand_to_target_mappings.size());

	// Randomly select one of the mapping (so that each time the
	// same target is visited, and the same rule is selected, it is
	// possible to select a different output to map to)
	//
	// XXX TODO use all possible output mapping instead; ie visit them
	// all, and add all resulting new targets to targets list; this will
	// avoid having to visit the target multiple times to get all
	// possible output mappings
	const HandleMap& implicand_mapping =
		rand_element(all_implicand_to_target_mappings);
	for (auto& p : implicand_mapping)
		LAZY_BC_LOG_DEBUG << "Chosen mapping is:" << std::endl
		                  << p.first->toShortString()
		                  << "to:" << std::endl
		                  << p.second->toShortString();

	Handle hrule_implicant_reverse_grounded;
	HandleMapSeq premises_vmap_list;
	OrderedHandleSet additional_free_var;
	for (auto& h : target.get_varset())
		additional_free_var.insert(_garbage_superspace.get_atom(h));
	HandleSeq possible_premises = find_premises(standardized_rule,
	                                            implicand_mapping,
	                                            additional_free_var,
	                                            hrule_implicant_reverse_grounded,
	                                            premises_vmap_list);

	bc_logger().debug("%d possible permises", possible_premises.size());

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
		bc_logger().debug("Adding rule's grounded input as Target");

		target.store_step(selected_rule, { hrule_implicant_reverse_grounded });
		_targets_set.emplace(hrule_implicant_reverse_grounded,
		                     gen_sub_varlist(hrule_implicant_reverse_grounded,
		                                     standardized_rule.get_forward_vardecl(),
		                                     additional_free_var));
		return;
	}

	// For each set of possible premises, check if they already satisfy the
	// target, so that we can apply the rule while we are looking at it in the
	// same step
	for (size_t i = 0; i < possible_premises.size(); i++)
	{
		Handle hp = possible_premises[i];
		HandleMap vm = premises_vmap_list[i];

		LAZY_BC_LOG_DEBUG << "Checking premises:" << std::endl
		                  << hp->toShortString();

		// Reverse ground the rule's outputs with the mapping to the premise
		// so that when we ground the premise, we know how to generate
		// the final output; one version containing the ExecutionOutputLink (if
		// any), and the other contains the actual output vector sequence.
		// Adding to _garbage_superspace because the mapping are from within
		// the garbage space.
		Handle output_grounded =
			garbage_substitute(standardized_rule.get_forward_conclusion(),
			                   implicand_mapping);
		LAZY_BC_LOG_DEBUG << "Output reverse grounded step 1 as:" << std::endl
		                  << output_grounded->toShortString();
		output_grounded = garbage_substitute(output_grounded, vm);
		LAZY_BC_LOG_DEBUG << "Output reverse grounded step 2 as:" << std::endl
		                  << output_grounded->toShortString();

		HandleSeq output_grounded_seq;
		for (const auto& h : standardized_rule.get_conclusion_seq())
			output_grounded_seq.push_back(
				garbage_substitute(garbage_substitute(h, implicand_mapping), vm));

		HandleMapSeq vm_list;

		// include the implicand mapping into vm so we can do variable chasing
		vm.insert(implicand_mapping.begin(), implicand_mapping.end());

		// use pattern matcher to try to ground the variables (if any) in the
		// selected premises, so we can use this grounding to "apply" the rule
		// to generate the rule's final output
		HandleSeq grounded_premises = ground_premises(hp, vm, vm_list);

		// Check each grounding to see if any has no variable
		for (size_t i = 0; i < grounded_premises.size(); ++i)
		{
			HandleMultimap results;
			Handle& g = grounded_premises[i];
			HandleMap& m = vm_list.at(i);

			LAZY_BC_LOG_DEBUG << "Checking possible permises grounding:"
			                  << std::endl << g->toShortString();

			// XXX should this only search for free var?
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
			// In other words apply forward chaining for that
			// grounded conclusion.
			//
			// XXX TODO the TV of the original "Variable Fullfillment" target
			// need to be changed here... right?
			Instantiator inst(&_as);
			Handle added = inst.instantiate(output_grounded, m);

			LAZY_BC_LOG_DEBUG << "Added:" << std::endl
			                  << added->toShortString() << "to _as";

			for (const auto& h : output_grounded_seq)
			{
				// add each sub-output to _as since the ExecutionOutputLink might
				// not add them all
				added = _as.add_atom(Substitutor::substitute(h, m));
				if (_focus_space.get_size() > 0 )
					_focus_space.add_atom(added);
				LAZY_BC_LOG_DEBUG << "Added:" << std::endl
				                  << added->toShortString() << "to _as";
			}

			// Add the grounding to the return results
			for (Handle& h : target.get_varseq())
				results[h].emplace(m.at(_garbage_superspace.get_atom(h)));

			target.store_varmap(results);
		}

		// XXX TODO premise selection would be done here to
		// determine whether to BC on a premise

		// non-logical link can be added straight to targets list
		if (_logical_link_types.count(hp->getType()) == 0)
		{			
			target.store_step(selected_rule, { hp });
			_targets_set.emplace(hp, gen_varlist(hp));
			continue;
		}

		bc_logger().debug("Before breaking apart into sub-premises");

		// Else break out any logical link and add to targets
		HandleSeq sub_premises = hp->getOutgoingSet();
		target.store_step(selected_rule, sub_premises);

		for (Handle& s : sub_premises)
			_targets_set.emplace(s, gen_varlist(s));
	}

	return;
}

/**
 * Find all atoms in the AtomSpace matching the pattern.
 *
 * @param hpattern         the atom to pattern match against
 * @param hpattern_vardecl the typed VariableList of the variables in hpattern
 * @param vmap             an output list of mapping for variables in hpattern
 * @return                 a vector of matched atoms
 */
HandleSeq BackwardChainer::match_knowledge_base(Handle hpattern,
                                                Handle hpattern_vardecl,
                                                HandleMapSeq& vmap,
                                                bool enable_var_name_check)
{
	AtomSpace focus_garbage_superspace(&_focus_space);
	AtomSpace* working_space;
	AtomSpace* working_garbage_superspace;

	// decide whether to look at a small focus set or whole AtomSpace
	if (_focus_space.get_size() > 0)
	{
		working_space = &_focus_space;
		working_garbage_superspace = &focus_garbage_superspace;
	}
	else
	{
		working_space = &_as;
		working_garbage_superspace = &_garbage_superspace;
	}

	hpattern = working_garbage_superspace->add_atom(hpattern);

	if (hpattern_vardecl == Handle::UNDEFINED)
	{
		OrderedHandleSet vars = get_free_variables(hpattern);
		hpattern_vardecl = working_garbage_superspace->add_atom(createVariableList(HandleSeq(vars.begin(), vars.end())));
	}
	else
		hpattern_vardecl = working_garbage_superspace->add_atom(hpattern_vardecl);

	LAZY_BC_LOG_DEBUG << "Matching knowledge base with:" << std::endl
	                  << hpattern->toShortString()
	                  << "and variables:" << std::endl
	                  << hpattern_vardecl->toShortString();

	// if no variables at all
	if (VariableListCast(hpattern_vardecl)->get_variables().varseq.empty())
	{
		// If the pattern already in the main atomspace, then itself is a match
		Handle hself = working_space->get_atom(hpattern);
		if (hself != Handle::UNDEFINED)
		{
			vmap.push_back(HandleMap());
			return { _as.get_atom(hself) };
		}

		return HandleSeq();
	}

	// Pattern Match on working_space, assuming PM will work even if some atoms
	// in hpattern are in the garbage space
	PatternLinkPtr sl(createPatternLink(hpattern_vardecl, hpattern));
	BackwardChainerPMCB pmcb(working_space,
	                         VariableListCast(hpattern_vardecl),
	                         enable_var_name_check);

	sl->satisfy(pmcb);

	HandleMapSeq var_solns = pmcb.get_var_list();
	HandleMapSeq pred_solns = pmcb.get_pred_list();

	HandleSeq results;

	bc_logger().debug("Pattern matcher found %d matches", var_solns.size());

	for (size_t i = 0; i < var_solns.size(); i++)
	{
		HandleSeq i_pred_soln;

		// check for bad mapping
		for (auto& p : pred_solns[i])
		{
			// don't want matched clause that is part of a rule
			auto& rules = _configReader.get_rules();
			if (std::any_of(rules.begin(), rules.end(), [&](Rule& r) {
						return is_atom_in_tree(r.get_forward_rule(), p.second);
					}))
			{
				bc_logger().debug("matched clause in rule");
				break;
			}

			// don't want matched stuff with some part of a rule inside
			if (std::any_of(rules.begin(), rules.end(), [&](Rule& r) {
						return is_atom_in_tree(p.second, r.get_forward_rule());
					}))
			{
				bc_logger().debug("matched clause wrapping rule");
				break;
			}

			// don't want matched clause that is not in the focus set or parent _as
			if (working_space->get_atom(p.second) == Handle::UNDEFINED)
			{
				LAZY_BC_LOG_DEBUG << "matched clause:" << std::endl
				                  << p.second->toShortString()
				                  << "not in target search space";
				break;
			}

			// store the main _as version of the matched atom
			i_pred_soln.push_back(_as.get_atom(p.second));
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

		// convert the working_space mapping to _as
		HandleMap converted_vmap;
		for (auto& p : var_solns[i])
			converted_vmap[_garbage_superspace.get_atom(p.first)]
			        = _as.get_atom(p.second);

		vmap.push_back(converted_vmap);
	}

	return results;
}

/**
 * Find all possible premises for a specific rule's implicant (input).
 *
 * @param standardized_rule                 the Rule object with the implicant
 * @param implicand_mapping                 the output (implicand) var mapping
 * @param additional_free_varset            additional free variables from the target
 * @param hrule_implicant_reverse_grounded  output grounding of the implicant
 * @param premises_vmap_list                the var mapping of each premise
 * @return                                  a vector of premises
 */
HandleSeq BackwardChainer::find_premises(const Rule& standardized_rule,
                                         const HandleMap& implicand_mapping,
                                         const OrderedHandleSet& additional_free_varset,
                                         Handle& hrule_implicant_reverse_grounded,
                                         HandleMapSeq& premises_vmap_list)
{
	Handle hrule_implicant = standardized_rule.get_forward_implicant();
	Handle hrule_vardecl = standardized_rule.get_forward_vardecl();

	// Reverse ground the implicant with the grounding we found from
	// unifying the implicand
	hrule_implicant_reverse_grounded = garbage_substitute(hrule_implicant,
	                                                      implicand_mapping);

	LAZY_BC_LOG_DEBUG << "Reverse grounded as:" << std::endl
	                  << hrule_implicant_reverse_grounded->toShortString();

	// Find all matching premises matching the implicant, where
	// premises_vmap_list will be the mapping from free variables in
	// himplicant to stuff in a premise
	HandleSeq possible_premises =
		match_knowledge_base(hrule_implicant_reverse_grounded,
		                     gen_sub_varlist(hrule_implicant_reverse_grounded,
		                                     hrule_vardecl,
		                                     additional_free_varset),
		                     premises_vmap_list);

	// Do another match but without the target's free var as variable, so they
	// are constant; mostly to handle where PM cannot map a variable to itself
	if (not additional_free_varset.empty())
	{
		HandleMapSeq premises_vmap_list_alt;

		HandleSeq possible_premises_alt =
		        match_knowledge_base(hrule_implicant_reverse_grounded,
		                             gen_sub_varlist(hrule_implicant_reverse_grounded,
		                                             hrule_vardecl,
		                                             OrderedHandleSet()),
		                             premises_vmap_list_alt,
		                             true);

		// collect the possible premises from the two verions of mapping
		possible_premises.insert(possible_premises.end(),
		                         possible_premises_alt.begin(),
		                         possible_premises_alt.end());
		premises_vmap_list.insert(premises_vmap_list.end(),
		                          premises_vmap_list_alt.begin(),
		                          premises_vmap_list_alt.end());
	}


	return possible_premises;
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
                                           const HandleMap& premise_vmap,
                                           HandleMapSeq& vmap_list)
{
	HandleSeq results;

	// if the target is already fully grounded
	if (is_closed(hpremise))
	{
		HandleMap old_map = premise_vmap;
		HandleMap new_map;

		// do variable chasing
		for (const auto& p : premise_vmap)
		{
			if (old_map.count(p.second) == 1)
			{
				new_map[p.first] = old_map[p.second];
				new_map[p.second] = old_map[p.second];
				old_map.erase(p.second);
			}
			else
				new_map[p.first] = p.second;
		}

		// add any leftover mapping into final ouput
		new_map.insert(old_map.begin(), old_map.end());

		vmap_list.push_back(new_map);
		results.push_back(hpremise);

		return results;
	}

	Handle premises = hpremise;

	if (_logical_link_types.count(premises->getType()) == 1)
	{
		HandleSeq sub_premises;
		HandleSeq oset = hpremise->getOutgoingSet();

		for (const Handle& h : oset)
		{
			// ignore premises with no free var
			if (is_closed(h))
				continue;

			sub_premises.push_back(h);
		}

		if (sub_premises.size() == 1)
			premises = sub_premises[0];
		else
			premises = _garbage_superspace.add_link(hpremise->getType(),
			                                        sub_premises);
	}

	LAZY_BC_LOG_DEBUG << "Grounding:" << std::endl
	                  << premises->toShortString();

	HandleMapSeq temp_vmap_list;

	// XXX TODO when all VariableNode are unique, we will be able to tell what
	// type a random VariableNode is in the AtomSpace by looking at its
	// antecedent; so the type should be included in the future
	HandleSeq temp_results = match_knowledge_base(premises, Handle::UNDEFINED,
	                                              temp_vmap_list);

	// Chase the variables so that if a variable A were mapped to another
	// variable B in premise_vmap, and after pattern matching, B now map
	// to some solution, change A to map to the same solution
	for (unsigned int i = 0; i < temp_results.size(); ++i)
	{
		HandleMap& tvm = temp_vmap_list[i];
		HandleMap this_map;

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
        for (const Handle& o : h->getOutgoingSet())
            get_all_unique_atoms(o, atom_set);
}

/**
 * Select a candidate rule from all rules.
 *
 * This method will try sub-atom unification if no whole output
 * unification is possible.
 *
 * XXX TODO use the rule weight
 * XXX should these selection functions be in callbacks like the ForwardChainer?
 *
 * @param target             the original target that the rules are going to unify to
 * @param selected_rule      output the selected rule
 * @param standardized_rule  output the standardized-apart version of the selected rule
 * @param all_implicand_to_target_mappings  the output implicand to target mapping
 * @return                   true if a rule is selected
 */
bool BackwardChainer::select_rule_old(const Target& target,
                                      Rule& selected_rule,
                                      Rule& standardized_rule,
                                      HandleMapSeq& all_implicand_to_target_mappings)
{
	Handle htarget = _garbage_superspace.add_atom(target.get_handle());
	Handle htarget_vardecl = _garbage_superspace.add_atom(target.get_vardecl());
	std::vector<Rule> rules = _configReader.get_rules();

	// store how many times each rule has been used for the target
	std::vector<double> weights;
	std::for_each(rules.begin(), rules.end(),
	              [&](const Rule& r)
	              { weights.push_back(target.get_selection_count() - target.rule_count(r) + 1); });

	while (not rules.empty())
	{
		// Select the rule that has been applied least
		int index = randGen().rand_discrete(weights);

		// unify against the standardized version, so the result will match
		// with what we will be applying against at later step
		selected_rule = rules[index];
		standardized_rule = selected_rule.gen_standardize_apart(&_garbage_superspace);

		Handle hrule_vardecl = standardized_rule.get_forward_vardecl();
		HandleSeq output = standardized_rule.get_conclusion_seq();

		all_implicand_to_target_mappings.clear();

		// check if any of the implicand's output can be unified to target
		for (const Handle& h : output)
		{
			HandleMap mapping;

			if (not unify(h,
			              htarget,
			              gen_sub_varlist(h, hrule_vardecl, OrderedHandleSet()),
			              htarget_vardecl,
			              mapping))
				continue;

			all_implicand_to_target_mappings.push_back(mapping);
		}

		// // if not unifiable, try sub-atom unification
		// if (all_implicand_to_target_mappings.empty())
		// {
		// 	UnorderedHandleSet output_expanded;
		// 	for (const Handle& h : output)
		// 	{
		// 		get_all_unique_atoms(h, output_expanded);
		// 		output_expanded.erase(h);
		// 	}

		// 	for (const Handle& h : output_expanded)
		// 	{
		// 		HandleMap mapping;

		// 		if (not unify(h,
		// 		              htarget,
		// 		              gen_sub_varlist(h, hrule_vardecl,
		// 		                              OrderedHandleSet()),
		// 		              htarget_vardecl,
		// 		              mapping))
		// 			continue;

		// 		all_implicand_to_target_mappings.push_back(mapping);
		// 	}
		// }

		if (not all_implicand_to_target_mappings.empty())
			return true;

		// move on to next rule if htarget cannot map to the output
		rules.erase(rules.begin() + index);
		weights.erase(weights.begin() + index);
	}

	return false;
}

Handle BackwardChainer::garbage_substitute(const Handle& term,
                                           const HandleMap& vm)
{
	return _garbage_superspace.add_atom(Substitutor::substitute(term, vm));
}

/**
 * Generate a VariableList of the free variables of a given target,
 * and add it to _garbage_superspace.
 */
Handle BackwardChainer::gen_varlist(const Handle& target)
{
	OrderedHandleSet target_vars = get_free_variables(target);
	return _garbage_superspace.add_atom(createVariableList(HandleSeq(target_vars.begin(), target_vars.end())));
}

/**
 * Given a VariableList, generate a new VariableList of only the
 * specific vars and add it to the _garbage_superspace.
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
                                        OrderedHandleSet additional_free_varset)
{
	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(parent);

	HandleSeq oset;
	if (parent_varlist->isLink())
		oset = parent_varlist->getOutgoingSet();
	else
		oset.push_back(parent_varlist);

	HandleSeq final_oset;

	// for each var in varlist, check if it is used in parent
	for (const Handle& h : oset)
	{
		Type t = h->getType();
		if (VARIABLE_NODE == t && fv.varset.count(h) == 1)
		{
			final_oset.push_back(h);
			additional_free_varset.erase(h);
		}
		else if (TYPED_VARIABLE_LINK == t
			     and fv.varset.count(h->getOutgoingSet()[0]) == 1)
		{
			final_oset.push_back(h);
			additional_free_varset.erase(h->getOutgoingSet()[0]);
		}
	}

	// for each var left in the additional_free_varset, check
	// if it is used in parent
	for (const Handle& h : additional_free_varset)
	{
		if (fv.varset.count(h) == 1)
			final_oset.push_back(h);
	}

	return _garbage_superspace.add_atom(createVariableList(final_oset));
}
#endif
