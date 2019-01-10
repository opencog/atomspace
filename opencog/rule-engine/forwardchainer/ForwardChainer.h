/*
 * ForwardChainer.h
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

#ifndef _OPENCOG_FORWARDCHAINERX_H_
#define _OPENCOG_FORWARDCHAINERX_H_

#include "../UREConfig.h"
#include "SourceSet.h"
#include "FCStat.h"

class ForwardChainerUTest;

namespace opencog
{

enum class source_selection_mode
{
	TV_FITNESS, STI, UNIFORM
};

class Rule;

// Pair of Rule and its probability estimate that it fullfils the
// objective
typedef std::pair<Rule, double> RuleProbabilityPair;

class ForwardChainer
{
private:
	friend class ::ForwardChainerUTest;

	// Knowledge base atomspace
	AtomSpace& _kb_as;

	// Rule base atomspace (can be the same as _kb_as)
	AtomSpace& _rb_as;

	// The focus set is copied into this atomspace; during chaining,
	// the pattern matcher is applied only to this atomspace.  This
	// is the primary mechanism by which chaining is restricted to
	// the focus set.  This is effective, but not very efficient;
	// perhaps there is some better mechanism?
	AtomSpace _focus_set_as;

	UREConfig _config;

	// Current iteration
	int _iteration;

	bool _search_focus_set;

	// Population of sources to expand forward
	SourceSet _sources;

	FCStat _fcstat;

	void init(const Handle& source,
	          const Handle& vardecl,
	          const HandleSeq& focus_set);

	void apply_all_rules();

	void validate(const Handle& source);

	void expand_meta_rules();

protected:
	RuleSet _rules; /* loaded rules */
	HandleSeq _focus_set;

	/**
	 * choose next source to expand
	 *
	 * @return  A Source to expand
	 *
	 * Warning: it is not const because the source is gonna be modified
	 * by keeping track of the rules applied to it.
	 */
	Source* select_source();

	/**
	 * Get rules that unify with the source
	 */
	RuleSet get_valid_rules(const Source& source);

	/**
	 * Choose an applicable rules from the rule base by selecting
	 * rules whose premise structurally matches with the source.
	 *
	 * If no rule can be chosen return invalid rule.
	 *
	 * @return  A rule that in which @param source could ground.
	 *
	 * TODO: move to ControlPolicy
	 */
	RuleProbabilityPair select_rule(const Handle& source);
	RuleProbabilityPair select_rule(Source& source);
	RuleProbabilityPair select_rule(const RuleSet& valid_rules);

	/**
	 * Apply rule.
	 */
	HandleSet apply_rule(const Rule& rule, Source& source);
	HandleSet apply_rule(const Rule& rule);

public:
	/**
	 * Ctor.
	 *
	 * @param kb_as     Knowledge-base atomspace
	 * @param rb_as     Rule-base atomspace
	 * @param rbs       Handle pointing to rule-based system.
	 * @param source    Source to start with, if it is a pattern, or a Set,
	 *                  multiple sources are considered
	 * @param vardecl   Variable declaration of Source if pattern
	 * @param focus_set Set of atoms under focus
	 */
	ForwardChainer(AtomSpace& kb_as,
	               AtomSpace& rb_as,
	               const Handle& rbs,
	               const Handle& source,
	               const Handle& vardecl=Handle::UNDEFINED,
	               const HandleSeq& focus_set=HandleSeq());

	/**
	 * Like above, but use as rule-base atomspace, the atomspace of rbs
	 * if any, otherwise use kb_as if rbs has no atomspace.
	 */
	ForwardChainer(AtomSpace& kb_as,
	               const Handle& rbs,
	               const Handle& source,
	               const Handle& vardecl=Handle::UNDEFINED,
	               const HandleSeq& focus_set=HandleSeq());
	~ForwardChainer();

	/**
	 * URE configuration accessors
	 */
	UREConfig& get_config();
	const UREConfig& get_config() const;

	/**
	 * Perform forward chaining inference till the termination
	 * criteria have been met.
	 */
	void do_chain();

	/**
	 * Perform a single forward chaining inference step.
	 */
	void do_step();

	/**
	 * @return true if the termination criteria have been met.
	 */
	bool termination();

	/**
	 * @return all results in their order of inference.
	 */
	HandleSet get_chaining_result();
};

} // ~namespace opencog

#endif /* _OPENCOG_FORWARDCHAINERX_H_ */
