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
#include <opencog/atoms/core/VariableList.h>


namespace opencog {

using namespace std;

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
 * node. In the such a case one the following 2 formats may be used.
 *
 * 2. A pair of forward and backward rules. The backward part allows
 * to easily obtain the conclusion pattern and possibly reconstruct
 * the premises given the conclusion. In such a case a rule pair is
 * represented as follows
 *
 * ListLink
 *    BindLink     ; Forward form
 *       <variables>
 *       AndLink
 *          <premise-1>
 *          ...
 *          <premise-n>
 *       <conclusion>
 *    BindLink     ; Backward form
 *       <variables>
 *       <conclusion>
 *       AndLink
 *          <premise-1>
 *          ...
 *          <premise-n>
 *
 * Of course grounded schemata can be used in either of these 2
 * notations. Also a conclusion could be a disjunction of different
 * conclusion terms (if the pattern matcher supports that). The second
 * notation (forward and backward pair) is necessary when the forward
 * rule output is obfuscated by a grounded schema and can as well be
 * useful when the transformations going from conclusion to premises
 * is not trivial.
 *
 * 3. In case we only care about de-obfuscating the conclusion a rule
 * may simply take the following form
 *
 * ListLink
 *    BindLink     ; Forward form
 *       <variables>
 *       AndLink
 *          <premise-1>
 *          ...
 *          <premise-n>
 *       <conclusion>
 *    GetLink      ; backward form (conclusion only)
 *       <variables>
 *       <conclusion>
 *
 * Note that the variables in the forward and backward formats (1. and
 * 2.) need not to be the same as their scopes are disjoint.
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
	Rule(const Handle& rule);
	Rule(const Handle& rule_alias, const Handle& rbs);

	void init(const Handle& rule);
	
	// Comparison
	bool operator==(const Rule& r) const {
		return r.forward_rule_handle_ == forward_rule_handle_
			and r.backward_rule_handle_ == backward_rule_handle_
			;
	}
	bool operator<(const Rule& r) const {
		return weight_ < r.weight_;
	}

	// Modifiers
	void set_forward_handle(const Handle& h);
	void set_name(const string& name);
	void set_category(const string& name);
	void set_weight(float p);

	// Access
	string& get_name();
	const string& get_name() const;
	string& get_category();
	const string& get_category() const;
	Handle get_forward_handle() const;
	Handle get_alias() const;
	Handle get_forward_vardecl() const;
	Handle get_backward_vardecl() const;
	Handle get_forward_implicant() const;

	/**
	 * Return the premises of the rule. Optionally a conclusion can be
	 * provided in argument. In such a case the premises will be
	 * computed based on the backward rule.
	 */
	HandleSeq get_premises(const Handle& conclusion = Handle::UNDEFINED) const;
	Handle get_forward_conclusion() const;
	HandleSeq get_conclusion_seq() const;
	float get_weight() const;

	Rule gen_standardize_apart(AtomSpace* as);

private:
	// // Rule handle, a BindLink or a ListLink of forward and backward rule
	// Maybe not useful
	// Handle rule_handle_;

	// Forward rule handle, typically a BindLink
	Handle forward_rule_handle_;

	// Backward rule handle, a BindLink or a GetLink
	Handle backward_rule_handle_;

	// Rule alias: (DefineLink rule_alias_ rule_handle_)
	Handle rule_alias_;

	// Rule name, the name of the node referring to the rule body
	string name_;

	// Rule-based system name
	string category_;

	// Rule weight (indicated by the TV strength of the membership of
	// the rule to the RBS)
	float weight_;

	Handle standardize_helper(AtomSpace* as, const Handle&, HandleMap&);
};

} // ~namespace opencog

#endif /* RULE_H_ */
