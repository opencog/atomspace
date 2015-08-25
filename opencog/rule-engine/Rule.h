/*
 * Rule.h
 *
 * Copyright (C) 2015 Misgana Bayetta
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
 * Mainly wraps a BindLink but with other important attributes
 */
class Rule : public boost::less_than_comparable<Rule>,
             public boost::equality_comparable<Rule>
{
public:
	// rule is actually
	//
	// MemberLink <TV>
	//    <rule name>
	//    <rbs>
	Rule(Handle rule);

	// Comparison
	bool operator==(const Rule& r) const {
		return r.rule_handle_ == rule_handle_;
	}
	bool operator<(const Rule& r) const {
		return weight_ < r.weight_;
	}

	// Modifiers
	void set_handle(Handle h) throw (InvalidParamException);
	void set_name(const string& name);
	void set_category(const string& name);
	void set_weight(float p);

	// Access
	string& get_name();
	const string& get_name() const;
	string& get_category();
	const string& get_category() const;
	Handle get_handle() const;
	Handle get_vardecl() const;
	Handle get_implicant() const;
	HandleSeq get_implicant_seq() const;
	Handle get_implicand() const;
	HandleSeq get_implicand_seq() const;
	float get_weight() const;

	Rule gen_standardize_apart(AtomSpace* as);

private:
	// Rule handle, typically a BindLink
	Handle rule_handle_;

	// Rule name, the name of the node referring to the rule body
	string name_;

	// Rule base system name
	string category_;

	// Rule weight (indicated by the TV strength of the membership of
	// the rule to the RBS)
	float weight_;

	Handle standardize_helper(AtomSpace* as, Handle, std::map<Handle, Handle>&);
};

} // ~namespace opencog

#endif /* RULE_H_ */
