/*
 * BackwardChainer.h
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
#ifndef BACKWARDCHAINER_H_
#define BACKWARDCHAINER_H_

#include <opencog/rule-engine/Rule.h>
#include <opencog/rule-engine/UREConfigReader.h>

#include "Target.h"

class BackwardChainerUTest;

namespace opencog
{

/**
 * TODO: update that comment
 *
 * Backward chaining falls into two cases
 *
 * 1. Truth value query - Given a target atom whose truth value is not
 *    known and a pool of atoms, find a way to estimate the truth
 *    value of the target Atom, via combining the atoms in the pool
 *    using the inference rule. Eg. The target is "Do people breath"
 *
 *    (InheritanceLink people breath)
 *
 *    the truth value of the target is estimated via doing the
 *    inference "People are animals, animals breathe, therefore people
 *    breathe."
 *
 * 2. Variable fulfillment query - Given a target Link (Atoms may be
 *    Nodes or Links) with one or more VariableAtoms among its
 *    targets, figure what atoms may be put in place of these
 *    VariableAtoms, so as to give the grounded targets a high
 *
 *    strength * confidence
 *
 *    Eg. What breathes (InheritanceLink $X breath) can be fulfilled
 *    by pattern matching, whenever there are are multiple values to
 *    fill $X we will use fitness value measure to choose the best
 *    other compound example is what breathes and adds ANDLink
 *    InheritanceLink $X Breath InheritanceLink $X adds
 *
 * Anatomy of current implementation
 * =================================
 *
 * 1. First check if the target matches to something in the knowledge base
 *    already
 *
 * 2. If not, choose an inference Rule R (using some Rule selection criteria)
 *    whose output can unify to the target
 *
 * 3. Reverse ground R's input to restrict the permises search
 *
 * 4. Find all premises that matches the restricted R's input by Pattern
 *    Matching
 *
 * 5. For each set of premises, Forward Chain (a.k.a apply the rule, or
 *    Pattern Matching) on the R to see if it can solve the target.
 *
 * 6. If not, add the permises to the targets list (in addition to some
 *    permise selection criteria)
 *
 * 7. Do target selection and repeat.
 */

class BackwardChainer
{
    friend class ::BackwardChainerUTest;

public:
	BackwardChainer(AtomSpace& as, const Handle& rbs);

	void set_target(const Handle& init_target,
	                const Handle& focus_link = Handle::UNDEFINED);
	UREConfigReader& get_config();
	const UREConfigReader& get_config() const;

	/**
	 * Perform a single backward chaining inference step.
	 */
	void do_step();
	void do_step_old();

	/**
	 * Perform backward chaining inference till the termination
	 * criteria have been met.
	 */
	void do_chain();

	/**
	 * @return true if the termination criteria have been met.
	 */
	bool termination();

	HandleMultimap get_chaining_result();

private:
	// Expand the back-inference tree of a target
	void expand_bit(Target& target, const Rule& rule);

	Target& select_target();

	// Select a valid rule given a target.
	const Rule& select_rule(const Target& target);

	// Return all valid rules, in the sense that these rules may
	// possibly be used to infer the target.
	vector<const Rule*> get_valid_rules(const Target& target);

	// Return true if a given target matches a given rule's
	// conclusion. In other words whether the rule is a potential
	// candidate for back-inference tree expansion.
	bool match_conclusion(const Target& target, const Rule& rule);

	// Fulfill, apply possible inferences in a forward way and pattern
	// matchings in order to fulfill the given target
	void fulfill_target(Target& target);

	// Check that h is matched by a given pattern body with a given
	// variable declaration vardecl.
	bool unify(const Handle& target, const Handle& pattern,
	           const Handle& pattern_vardecl);

	// Like above but handles variables on the source as well and
	// record the matching in the HandleMap result
	bool unify(const Handle& target, const Handle& pattern,
	           const Handle& target_vardecl, const Handle& pattern_vardecl,
	           HandleMap& result);

	// Like above but discard the result
	bool unify(const Handle& target, const Handle& pattern,
	           const Handle& target_vardecl, const Handle& pattern_vardecl);

	bool sym_unify(const Handle& lhs, const Handle& rhs,
	               const Handle& lhs_vardecl, const Handle& rhs_vardecl);

	VariableListPtr gen_varlist(const Handle& h);
	VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl);

#if 0
	bool select_rule_old(const Target& target,
	                     Rule& selected_rule,
	                     Rule& standardized_rule,
	                     HandleMapSeq& all_implicand_to_target_mappings);

	void process_target(Target& target);

	HandleSeq match_knowledge_base(Handle htarget,
	                               Handle htarget_vardecl,
	                               HandleMapSeq& vmap,
	                               bool enable_var_name_check = false);
	HandleSeq find_premises(const Rule& standardized_rule,
	                        const HandleMap& implicand_mapping,
	                        const OrderedHandleSet& additional_free_varset,
	                        Handle& hrule_implicant_reverse_grounded,
	                        HandleMapSeq& premises_vmap_list);
	HandleSeq ground_premises(const Handle& htarget, const HandleMap& vmap,
	                          HandleMapSeq& vmap_list);
	Handle garbage_substitute(const Handle& term, const HandleMap& vm);
	
	Handle gen_varlist(const Handle& target);
	Handle gen_sub_varlist(const Handle& parent, const Handle& parent_varlist,
	                       OrderedHandleSet additional_free_varset);
#endif
	AtomSpace& _as;
	UREConfigReader _configReader;
	AtomSpace _garbage_superspace;
	Handle _init_target;
	AtomSpace _focus_space;
	int _iteration;

	TargetSet _target_set;

	const std::vector<Rule>& _rules;

	// XXX any additional link should be reflected
	unordered_set<Type> _logical_link_types = { AND_LINK, OR_LINK, NOT_LINK };
};


} // namespace opencog

#endif /* BACKWARDCHAINER_H_ */
