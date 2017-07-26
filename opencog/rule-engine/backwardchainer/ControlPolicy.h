/*
 * ControlPolicy.h
 *
 * Copyright (C) 2017 OpenCog Foundation
 *
 * Authors: Nil Geisweiller
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
#ifndef OPENCOG_CONTROLPOLICY_H_
#define OPENCOG_CONTROLPOLICY_H_

#include <opencog/atomspace/AtomSpace.h>

#include "BIT.h"
#include "../Rule.h"

namespace opencog
{

class ControlPolicy
{
public:
	ControlPolicy(const RuleSet& rules, const BIT& bit,
	              AtomSpace* control_as=nullptr);

	const std::string preproof_predicate_name = "URE:BC:preproof";

	RuleSet rules;

	/**
	 * Select a valid rule given a target. The selected is a new
	 * object because a new rule is created, its variables are
	 * uniquely renamed, possibly some partial substitutions are
	 * applied.
	 *
	 * Unless a control_as is provided at construction time, the
	 * Selection is random amongst the valid rules and weighted
	 * according to their weights.
	 *
	 * TODO: add comments about inference control policy, see
	 * README.md of
	 * <OPENCOG_ROOT>/examples/pln/inference-control-learning
	 *
	 * The andbit and bitleaf are not const because if the rules are
	 * exhausted it will set its exhausted flag to false.
	 */
	RuleTypedSubstitutionPair select_rule(AndBIT& andbit, BITNode& bitleaf);

private:
	// Reference to the BackwardChainer BIT
	const BIT& _bit;

	// AtomSpace holding the inference control rules
	AtomSpace* _control_as;
	
	/**
	 * Return all valid rules, in the sense that they may possibly be
	 * used to infer the target.
	 */
	RuleTypedSubstitutionMap get_valid_rules(const AndBIT& andbit,
	                                         const BITNode& bitleaf);

	/**
	 * Select a rule for expansion amongst a set of valid ones.
	 */
	RuleTypedSubstitutionPair select_rule(const AndBIT& andbit,
	                                      const BITNode& bitleaf,
	                                      const RuleTypedSubstitutionMap& rules);
};


} // namespace opencog

#endif /* OPENCOG_CONTROLPOLICY_H_ */
