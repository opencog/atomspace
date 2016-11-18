/*
 * Rule.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com>  2015
 *          Nil Geisweiller 2015-2016
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
#include <opencog/atoms/pattern/BindLink.h>

namespace opencog {

class Rule;
class RuleSeq : public std::vector<Rule>
{
	// Run all meta rules and insert them back in the rule sequence.
	void expand_meta_rules() {}
};

/**
 * Class for managing rules in the URE.
 *
 * A URE rule may have one of the following formats.
 *
 * 1. A single, forward-only premises-to-conclusion rule, of the style
 *
 *     <premise-1>
 *     ...
 *     <premise-1>
 *     |-
 *     <conclusion>
 *
 * is represented as
 *
 *     BindLink
 *        <variables>
 *        AndLink
 *           <premise-1>
 *           ...
 *           <premise-n>
 *        <conclusion>
 *
 * Here, `<conclusion>` may represent the conclusion pattern explicitly,
 * or, in most cases, it may be obfuscated in a grounded schema node.
 * In such a case, the following format may be used. Note that if the
 * rule uses a `GroundedSchema` and no backward form is used, as
 * described below, then the last argument of the `GroundedSchema`
 * will represent the rule's conclusion pattern.
 *
 * 2. A list starting with a forward-format and, optionally, one or
 * more backward-forms. The backward forms allow the conclusion
 * patterns to be easily obtained, as well as a means to reconstruct
 * the premises, given a conclusion. In this case, a rule is
 * represented as follows:
 *
 *     ListLink
 *        <forward>
 *        <backward-1>
 *        ...
 *        <backward-n>
 *
 * where `<forward>` is the structure described in part 1, above. The
 * `<backward-k>` terms are either
 *
 *      BindLink
 *         <variables>
 *         <conclusion>
 *         AndLink
 *            <premise-1>
 *            ...
 *            <premise-n>
 *
 * or, if we don't need to translate a certain conclusion into premises,
 * such terms take the form
 *
 *      GetLink
 *         <variables>
 *         <conclusion>
 *
 * This second notation (forward and backward forms) is necessary when
 * the forward rule output(s) is obfuscated by a grounded schema(ta).
 * It can also be useful when the transformations going from conclusion
 * to premises are non-trivial. The reason there are several backward
 * forms is because a forward rule may output several conclusions.
 * Take, for instance, the PLN equivalence-to-double-implication rule,
 * that given A<->B, outputs A->B and B->A.
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
	 * where `<rule alias>` is a `DefinedSchemaNode`, defined elsewhere,
	 * with a `DefineLink`.
	 */
	Rule();
	Rule(const Handle& rule);
	Rule(const Handle& rule_alias, const Handle& rbs);

	void init(const Handle& rule);
	
	// Comparison
	bool operator==(const Rule& r) const;
	/**
	 * Order by weight, or if equal by handle value.
	 */
	bool operator<(const Rule& r) const;
	bool is_alpha_equivalent(const Rule&) const;

	// Modifiers
	void set_forward_handle(const Handle&);
	void set_backward_handles(const HandleSeq&);
	void set_name(const std::string&);
	void set_category(const std::string&);
	void set_weight(double);

	// Access
	std::string& get_name();
	const std::string& get_name() const;
	std::string& get_category();
	const std::string& get_category() const;
	Handle get_forward_rule() const;
	Handle get_alias() const;

	/**
	 * Add the rule in AtomSpace as.
	 *
	 * Warning: this will only add the pattern and rewrite terms, not
	 * the scope links themselves. This is a hack to work around the
	 * alpha conversion that the atomspace may perform when inserting
	 * scope links, so that alpha-equivalent scope links are not
	 * redundantly added to an atomspace, which turns out to be
	 * inconvenient for combining multiple scope links, in the way
	 * that the backward chainer does when building forward chaining
	 * strategies.
	 *
	 * Nil, can you explain what the problem above actually is, and
	 * open a bug report, illustrating it? No hacks should be required:
	 * the alpha conversion should already be doing exactly the right
	 * thing, in every situation.  What is the "inconvenience"? What
	 * is the problem that you are trying to work around?
	 *
	 * TODO: support backward rule form.
	 */
	void add(AtomSpace&);

	/**
	 * Return the variable declaration of the forward rule form.
	 */
	Handle get_forward_vardecl() const;
	HandleSeq get_backward_vardecls() const;
	Handle get_forward_implicant() const;
	Handle get_forward_implicand() const;

	// Properties
	bool is_valid() const;

	/**
	 * Return the premises of the rule. Optionally, a conclusion can
	 * be provided as the argument. In such a case, the premises will
	 * be computed, based on the backward rule.
	 */
	HandleSeq get_premises(const Handle& = Handle::UNDEFINED) const;

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
	double get_weight() const;

	/**
	 * Create a new rule where all variables are uniquely renamed.
	 *
	 * @param as  pointer to the atomspace where the new BindLink will be added
	 * @return    a new Rule object with its own new BindLink
	 *
	 * TODO: support backward rule handles.
	 */
	Rule gen_standardize_apart(AtomSpace*);

	/**
	 * Used by the forward chainer to select rules. Given a source,
	 * generate all rule variations that may be applied over a given
	 * source. The variables in the rules are renamed to almost
	 * certainly avoid name collision.
	 *
	 * TODO: we probably want to support a vector of sources for rules
	 * with multiple premises.
	 */
	RuleSeq unify_source(const Handle& source, const Handle& vardecl) const;

	/**
	 * Used by the backward chainer. Given a target, generate all rule
	 * variations that may infer this target. The variables in the
	 * rules are renamed to almost certainly avoid name collision.
	 */
	RuleSeq unify_target(const Handle& target, const Handle& vardecl) const;

	std::string to_string() const;

private:
	// Forward rule
	BindLinkPtr _forward_rule;

	// Backward rule handles, BindLinks or a GetLinks
	//
	// TODO: Maybe replace that by vector<ScopeLinkPtr>
	HandleSeq _backward_rule_handles;
	std::vector<ScopeLinkPtr> _backward_rule_scope_links;

	// Rule alias: (DefineLink rule_alias_ rule_handle_)
	Handle _rule_alias;

	// Rule name, the name of the node referring to the rule body
	std::string _name;

	// Rule-based system name
	std::string _category;

	// Rule weight (indicated by the TV strength of the membership of
	// the rule to the RBS)
	double _weight;

	// Return a copy of the rule with the variables alpha-converted
	// into random variable names.
	Rule rand_alpha_converted() const;

	Handle standardize_helper(AtomSpace*, const Handle&, HandleMap&);

	// Return the conclusions of the forward conclusions. There are
	// several of them because the conclusions can be wrapped in the
	// ListLink. In case each conclusion is an ExecutionOutputLink
	// then return the last argument of that ExecutionOutputLink.
	HandleSeq get_forward_conclusion_bodies() const;

	// Given an ExecutionOutputLink return its last argument
	Handle get_execution_output_last_argument(const Handle& h) const;
};

// For Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Rule& rule);
std::string oc_to_string(const RuleSeq& rules);

} // ~namespace opencog

#endif /* RULE_H_ */
