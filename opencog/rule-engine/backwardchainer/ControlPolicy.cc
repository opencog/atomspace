/*
 * ControlPolicy.cc
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

#include "ControlPolicy.h"

#include <opencog/util/random.h>

#include "../URELogger.h"

using namespace opencog;

ControlPolicy::ControlPolicy(const RuleSet& rs, const BIT& bit,
                             AtomSpace* control_as) :
	rules(rs), _bit(bit), _control_as(control_as) {}

RuleTypedSubstitutionPair ControlPolicy::select_rule(AndBIT& andbit,
                                                     BITNode& bitleaf)
{
	// The rule is randomly selected amongst the valid ones, with
	// probability of selection being proportional to its weight.
	const RuleTypedSubstitutionMap valid_rules = get_valid_rules(andbit, bitleaf);
	if (valid_rules.empty()) {
		bitleaf.exhausted = true;
		return {Rule(), Unify::TypedSubstitution()};;
	}

	// Log all valid rules and their weights
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << "The following weighted rules are valid:";
		for (const auto& r : valid_rules)
			ss << std::endl << r.first.get_weight() << " " << r.first.get_name();
		LAZY_URE_LOG_DEBUG << ss.str();
	}

	return select_rule(andbit, bitleaf, valid_rules);
}

RuleTypedSubstitutionMap ControlPolicy::get_valid_rules(const AndBIT& andbit,
                                                        const BITNode& bitleaf)
{
	// Generate all valid rules
	RuleTypedSubstitutionMap valid_rules;
	for (const Rule& rule : rules) {
		// For now ignore meta rules as they are forwardly applied in
		// expand_bit()
		if (rule.is_meta())
			continue;

		// Get the leaf vardecl from fcs. We don't want to filter it
		// because otherwise the typed substitution obtained may miss some
		// variables in the FCS declaration that needs to be substituted
		// during expension.
		Handle vardecl;
		if (andbit.fcs)
			vardecl = BindLinkCast(andbit.fcs)->get_vardecl();

		RuleTypedSubstitutionMap unified_rules
			= rule.unify_target(bitleaf.body, vardecl);

		// Insert only rules with positive probability of success
		RuleTypedSubstitutionMap pos_rules;
		for (const auto& rule : unified_rules) {
			double p = (_bit.is_in(rule, bitleaf) ? 0.0 : 1.0)
				* rule.first.get_weight();
			if (p > 0) pos_rules.insert(rule);
		}

		valid_rules.insert(pos_rules.begin(), pos_rules.end());
	}
	return valid_rules;
}

RuleTypedSubstitutionPair ControlPolicy::select_rule(const AndBIT& andbit,
                                                     const BITNode& bitleaf,
                                                     const RuleTypedSubstitutionMap& rules)
{
	// Build weight vector to do weighted random selection
	std::vector<double> weights;
	for (const auto& rule : rules)
		weights.push_back(rule.first.get_weight());

	// No rule exhaustion, sample one according to the distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return rand_element(rules, dist);
}
