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
#include <opencog/atomutils/Unify.h>

namespace opencog {

class Rule;
class RuleSet : public std::set<Rule>
{
public:
	// Run all meta rules and insert the resulting rules back in the rule set.
	void expand_meta_rules(AtomSpace& as);
};

typedef std::map<Rule, Unify::TypedSubstitution> RuleTypedSubstitutionMap;
typedef RuleTypedSubstitutionMap::value_type RuleTypedSubstitutionPair;

/**
 * Class for managing rules in the URE.
 *
 * A URE rule have one of the following format
 *
 *     <premise-1>
 *     ...
 *     <premise-1>
 *     |-
 *     <conclusion>
 *
 * represented either as
 *
 *     BindLink
 *        <variables>
 *        AndLink
 *           <premise-1>
 *           ...
 *           <premise-n>
 *        <conclusion>
 *
 * if there is ExecutionOutputLink in the BindLink rewrite term. Or
 * 
 *     BindLink
 *        <variables>
 *        AndLink
 *           <clauses-1>
 *           ...
 *           <clauses-n>
 *        ExecutionOutputLink
 *           <formula>
 *           ListLink
 *              <conclusion>
 *              <premises-1>
 *              ...
 *              <premises-m>
 *
 * Also, unordered premises can be wrapped in SetLink as this may
 * speed up a bit the Backward Chainer.
 */
class Rule : public boost::totally_ordered<Rule>
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
	explicit Rule(const Handle& rule);
	Rule(const Handle& rule_alias, const Handle& rbs);
	Rule(const Handle& rule_alias, const Handle& rule, const Handle& rbs);

	void init(const Handle& rule_member);
	void init(const Handle& rule_alias, const Handle& rbs);
	void init(const Handle& rule_alias, const Handle& rule, const Handle& rbs);
	
	// Comparison
	bool operator==(const Rule& r) const;
	/**
	 * Order by weight, or if equal by handle value.
	 */
	bool operator<(const Rule& r) const;
	bool is_alpha_equivalent(const Rule&) const;

	// Modifiers
	void set_rule(const Handle&);
	void set_name(const std::string&);
	void set_category(const std::string&);
	void set_weight(double);

	// Access
	std::string& get_name();
	const std::string& get_name() const;
	Handle get_rule() const;
	Handle get_alias() const;
	Handle get_rbs() const;

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
	 * Return the variable declaration of the rule.
	 */
	Handle get_vardecl() const;
	Handle get_implicant() const;
	Handle get_implicand() const;

	// Properties
	bool is_valid() const;
	bool is_meta() const;       // does that rule produces a rule

	/**
	 * Return the pattern matcher clauses of the rule. This may not
	 * necessarily represent the premises of the rule which may be the
	 * last arguments of the rewrite term, but rather the pattern
	 * matcher clauses required to trigger that rule.
	 */
	HandleSeq get_clauses() const;

	/**
	 * Return the rule premises, that is the last arguments of the
	 * rewrite term's ExecutionOutputLink. If the last arguments are
	 * SetLinks, then return their outgoings as well. That is because
	 * SetLink is used to represent unordered arguments.
	 */
	HandleSeq get_premises() const;

	/**
	 * Return the conclusion of the rule. Used for applying a forward
	 * step.
	 */
	Handle get_conclusion() const;

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
	 *
	 * TODO: we probably want to return only typed substitutions.
	 * However due to the unifier not supporting well same variables
	 * one different sides, we need to perform alpha conversion to
	 * avoid troubles, thus having to return the rules along side the
	 * typed substitutions.
	 *
	 * TODO: it's not clear the forward chainer needs the
	 * TypedSubtitution at all. Maybe only the rules are enough. For
	 * now we return both.
	 */
	RuleTypedSubstitutionMap unify_source(const Handle& source,
	                                      const Handle& vardecl=Handle::UNDEFINED) const;

	/**
	 * Used by the backward chainer. Given a target, generate all rule
	 * variations that may infer this target. The variables in the
	 * rules are renamed to almost certainly avoid name collision.
	 *
	 * TODO: we probably want to return only typed substitutions.
	 * However due to the unifier not supporting well same variables
	 * one different sides, we need to perform alpha conversion to
	 * avoid troubles, thus having to return the rules along side the
	 * typed substitutions.
	 */
	 RuleTypedSubstitutionMap unify_target(const Handle& target,
	                                       const Handle& vardecl=Handle::UNDEFINED) const;

	/**
	 * Remove the typed substitutions from the rule typed substitution map.
	 */
	static RuleSet strip_typed_substitution(const RuleTypedSubstitutionMap& rules);

	/**
	 * Apply rule (in a forward way) over atomspace as.
	 */
	Handle apply(AtomSpace& as) const;

	std::string to_string() const;

	// This flag allows to only sonsider the Rule clauses as
	// premises. This is for backward compatibility with some rule
	// base like R2L.
	mutable bool premises_as_clauses;

private:
	// Rule
	BindLinkPtr _rule;

	// Rule alias: (DefineLink _rule_alias _rule_handle)
	Handle _rule_alias;

	// Rule name, the name of the node referring to the rule body
	std::string _name;

	// Rule-based system name
	Handle _rbs;

	// Rule weight (indicated by the TV strength of the membership of
	// the rule to the RBS)
	double _weight;

	// Return a copy of the rule with the variables alpha-converted
	// into random variable names.
	Rule rand_alpha_converted() const;

	Handle standardize_helper(AtomSpace*, const Handle&, HandleMap&);

	// Return the conclusion patterns of the rule. There are several
	// of them because the conclusions can be wrapped in the
	// ListLink. In case each conclusion is an ExecutionOutputLink
	// then return the first argument of that ExecutionOutputLink.
	HandleSeq get_conclusion_patterns() const;
	Handle get_conclusion_pattern(const Handle& h) const;

	// Given an ExecutionOutputLink return its first argument
	Handle get_execution_output_first_argument(const Handle& h) const;

	// Given a typed substitution obtained from typed_substitutions
	// unify function, generate a new partially substituted rule.
	Rule substituted(const Unify::TypedSubstitution& ts) const;
};

// For Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Rule& rule);
std::string oc_to_string(const RuleSet& rules);
std::string oc_to_string(const RuleTypedSubstitutionPair& rule_ts_pair);
std::string oc_to_string(const RuleTypedSubstitutionMap& rule_ts_map);

} // ~namespace opencog

#endif /* RULE_H_ */
