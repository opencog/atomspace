/*
 * Rule.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  2015
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

#ifndef RULE_H_
#define RULE_H_

#include <boost/operators.hpp>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/VariableList.h>

namespace opencog {

using namespace std;

class Rule;
typedef vector<Rule> RuleSeq;

/**
 * Class for managing rules in the URE.
 *
 * A URE rule has the following formats
 *
 * 1. A single forward only premises to conclusion rule such as
 *
 * <premise-1>
 * ...
 * <premise-1>
 * |-
 * <conclusion>
 *
 * is represented as
 *
 * BindLink
 *    <variables>
 *    AndLink
 *       <premise-1>
 *       ...
 *       <premise-n>
 *    <conclusion>
 *
 * <conclusion> may represent explicitly the conclusion pattern, or
 * (most of the cases) it may be obfuscated in a grounded schema
 * node. In the such a case one the following format may be used. Note
 * that if the rule uses a GroundedSchema and no backward form in used
 * (as described below) then the last argument of the GroundedSchema
 * will represent the rule's conclusion pattern.
 *
 * 2. A list starting with a forward format and optionally 1 or more
 * backward forms. The backward forms allow to easily obtain
 * conclusion patterns and possibly reconstruct premises given a
 * conclusion. In such a case a rule is represented as follows
 *
 * ListLink
 *    <forward>
 *    <backward-1>
 *    ...
 *    <backward-n>
 *
 * Where <forward> is described as in 1. and backward-i is either
 *
 *    BindLink
 *       <variables>
 *       <conclusion>
 *       AndLink
 *          <premise-1>
 *          ...
 *          <premise-n>
 *
 * or if we don't need to translate a certain conclusion into premises
 *
 *    GetLink
 *       <variables>
 *       <conclusion>
 *
 * That second notation (forward and backward forms) is necessary when
 * the forward rule output(s) is obfuscated by a grounded schema(ta)
 * or can as well be useful when the transformations going from
 * conclusion to premises is non trivial. The reason we have several
 * backward forms is because a forward rule output several conclusions
 * (take for instance the PLN equivalence-to-double-implication rule
 * that given A<->B outputs A-> and B->A).
 *
 */
class Rule : public boost::less_than_comparable<Rule>,
             public boost::equality_comparable<Rule>
{
public:
	/**
	 * The rule argument has the format
	 *
	 * MemberLink <TV>
	 *    <rule alias>
	 *    <rbs>
	 *
	 * where <rule alias> is a DefinedSchemaNode elsewhere defined via
	 * a DefineLink.
	 */
	Rule();
	Rule(const Handle& rule);
	Rule(const Handle& rule_alias, const Handle& rbs);

	void init(const Handle& rule);
	
	// Comparison
	bool operator==(const Rule& r) const {
		return r._forward_rule_handle == _forward_rule_handle
			and r._backward_rule_handles == _backward_rule_handles;
	}
	bool operator<(const Rule& r) const {
		return _weight < r._weight;
	}

	// Modifiers
	void set_forward_handle(const Handle& h);
	void set_backward_handles(const HandleSeq& hs);
	void set_name(const string& name);
	void set_category(const string& name);
	void set_weight(float p);

	// Access
	string& get_name();
	const string& get_name() const;
	string& get_category();
	const string& get_category() const;
	Handle get_forward_rule() const;
	Handle get_alias() const;
	Handle get_forward_vardecl() const;
	HandleSeq get_backward_vardecls() const;
	Handle get_forward_implicant() const;
	Handle get_forward_implicand() const;

	// Properties
	bool is_valid() const;

	/**
	 * Return the premises of the rule. Optionally a conclusion can be
	 * provided in argument. In such a case the premises will be
	 * computed based on the backward rule.
	 */
	HandleSeq get_premises(const Handle& conclusion = Handle::UNDEFINED) const;

	/**
	 * Return the conclusion on the forward rule. Used for applying a
	 * forward step.
	 */
	Handle get_forward_conclusion() const;

	/**
	 * Return the list of conclusion patterns. Each pattern is a pair
	 * of Handles (variable declaration, body). Used for finding out
	 * is the rule matches a certain target.
	 */
	HandlePairSeq get_conclusions() const;
	float get_weight() const;

	/**
	 * Create a new rule where all variables are uniquely renamed.
	 *
	 * @param as  pointer to the atomspace where the new BindLink will be added
	 * @return    a new Rule object with its own new BindLink
	 *
	 * TODO: support backward rule handles.
	 */
	Rule gen_standardize_apart(AtomSpace* as);

	/**
	 * Used by the forward chainer to select rules. Given a source,
	 * generate all rule variations that may be applied over a given
	 * source. The variables in the rules are renamed to almost
	 * certainly avoid name collision.
	 *
	 * TODO: we probably want to support a vector of sources for rules
	 * with multiple premises.
	 */
	RuleSeq unify_source(const Handle& source, const Handle& vardecl);

	/**
	 * Used by the backward chainer to select rules. Given a target,
	 * generate all rule variations that may infer this target. The
	 * variables in the rules are renamed to almost certainly avoid
	 * name collision.
	 */
	RuleSeq unify_target(const Handle& target, const Handle& vardecl);

	std::string to_string() const;

private:
	// // Rule handle, a BindLink or a ListLink of forward and backward rule
	// Maybe not useful
	// Handle _rule_handle;

	// Forward rule handle, typically a BindLink
	//
	// TODO: Maybe replace that by ScopeLinkPtr
	Handle _forward_rule_handle;
	ScopeLinkPtr _forward_rule_scope_link;

	// Backward rule handles, BindLinks or a GetLinks
	//
	// TODO: Maybe replace that by vector<ScopeLinkPtr>
	HandleSeq _backward_rule_handles;
	vector<ScopeLinkPtr> _backward_rule_scope_links;

	// Rule alias: (DefineLink rule_alias_ rule_handle_)
	Handle _rule_alias;

	// Rule name, the name of the node referring to the rule body
	string _name;

	// Rule-based system name
	string _category;

	// Rule weight (indicated by the TV strength of the membership of
	// the rule to the RBS)
	float _weight;

	// Return a copy of the rule with the variables alpha-converted
	// into random variable names.
	Rule rand_alpha_converted() const;

	Handle standardize_helper(AtomSpace* as, const Handle&, HandleMap&);

	// Return the conclusions of the forward conclusions. There are
	// several of them because the conclusions can be wrapped in the
	// ListLink. In case each conclusion is an ExecutionOutputLink
	// then return the last argument of that ExecutionOutputLink.
	HandleSeq get_forward_conclusion_bodies() const;

	// Given an ExecutionOutputLink return its last argument
	Handle get_execution_output_last_argument(const Handle& h) const;
};

// For Gdb debugging
std::string oc_to_string(const Rule& rule);
std::string oc_to_string(const RuleSeq& rules);

} // ~namespace opencog

#endif /* RULE_H_ */
