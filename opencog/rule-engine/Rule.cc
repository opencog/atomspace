/*
 * Rule.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com> 2015
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

#include <queue>

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/pattern/BindLink.h>

#include <opencog/atomutils/TypeUtils.h>

#include "Rule.h"

using namespace opencog;

Rule::Rule()
	: forward_rule_handle_(Handle::UNDEFINED), rule_alias_(Handle::UNDEFINED) {}

Rule::Rule(const Handle& rule_ml)
	: forward_rule_handle_(Handle::UNDEFINED)
{
	init(rule_ml);
}

Rule::Rule(const Handle& rule_name, const Handle& rbs)
{
	AtomSpace* as = rule_name->getAtomSpace();
	init(as->get_link(MEMBER_LINK, rule_name, rbs));
}

void Rule::init(const Handle& rule_ml)
{
	OC_ASSERT(rule_ml != Handle::UNDEFINED);
	if (not classserver().isA(rule_ml->getType(), MEMBER_LINK))
		throw InvalidParamException(TRACE_INFO,
		                            "Rule '%s' is expected to be a MemberLink",
		                            rule_ml->toString().c_str());

	rule_alias_ = rule_ml->getOutgoingAtom(0);
	Handle rbs = rule_ml->getOutgoingAtom(1);

	Handle rule = DefineLink::get_definition(rule_alias_);
	if (rule->getType() == LIST_LINK)
	{
		OC_ASSERT(rule->getArity() > 0);
		// Split the rule into a forward and backward parts
		forward_rule_handle_ = rule->getOutgoingAtom(0);
		for (unsigned i = 1; i < rule->getArity(); i++)
			backward_rule_handles_.push_back(rule->getOutgoingAtom(i));
	}
	else
	{
		OC_ASSERT(rule->getType() == BIND_LINK);
		forward_rule_handle_ = rule;
	}
	name_ = rule_alias_->getName();
	category_ = rbs->getName();
	weight_ = rule->getTruthValue()->getMean();
}

float Rule::get_weight() const
{
	return weight_;
}

void Rule::set_category(const string& name)
{
	category_ = name;
}

string& Rule::get_category()
{
	return category_;
}

const string& Rule::get_category() const
{
	return category_;
}

void Rule::set_name(const string& name)
{
	name_ = name;
}

string& Rule::get_name()
{
	return name_;
}

const string& Rule::get_name() const
{
	return name_;
}

void Rule::set_forward_handle(const Handle& h)
{
	forward_rule_handle_ = h;
}

Handle Rule::get_forward_rule() const
{
	return forward_rule_handle_;
}

Handle Rule::get_alias() const
{
	return rule_alias_;
}

/**
 * Get the typed variable list of the Rule.
 *
 * @return the VariableList or the lone VariableNode
 */
Handle Rule::get_forward_vardecl() const
{
	// if the rule's handle has not been set yet
	if (forward_rule_handle_ == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return forward_rule_handle_->getOutgoingAtom(0);
}

/**
 * Get the typed variable list of the Rule.
 *
 * @return the VariableList or the lone VariableNode
 */
HandleSeq Rule::get_backward_vardecls() const
{
	HandleSeq results;
	for (const Handle& h : backward_rule_handles_)
		results.push_back(h->getOutgoingAtom(0));
	return results;
}

/**
 * Get the implicant (input) of the rule defined in a BindLink.
 *
 * @return the Handle of the implicant
 */
Handle Rule::get_forward_implicant() const
{
	// if the rule's handle has not been set yet
	if (forward_rule_handle_ == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(forward_rule_handle_)->get_body();
}

Handle Rule::get_forward_implicand() const
{
	return BindLinkCast(forward_rule_handle_)->get_implicand();
}

/**
 * Get the set of members of the implicant which are
 * connected by a root logical link.
 *
 * @return HandleSeq of members of the implicant
 */
HandleSeq Rule::get_premises(const Handle& conclusion) const
{
	// if the rule's handle has not been set yet
	if (forward_rule_handle_ == Handle::UNDEFINED)
		return HandleSeq();

    Handle implicant = get_forward_implicant();
    Type t = implicant->getType();
    HandleSeq hs;

    if (t == AND_LINK or t == OR_LINK)
        hs = implicant->getOutgoingSet();
    else
        hs.push_back(implicant);

    return hs;
}

/**
 * Get the conclusion (output) of the rule.  defined in a BindLink.
 *
 * @return the Handle of the implicand
 */
Handle Rule::get_forward_conclusion() const
{
	// if the rule's handle has not been set yet
	if (forward_rule_handle_ == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(forward_rule_handle_)->get_implicand();
}

HandlePairSeq Rule::get_conclusions() const
{
	HandlePairSeq results;

	// If the rule's handle has not been set yet
	if (forward_rule_handle_ == Handle::UNDEFINED)
		return HandlePairSeq();

	// If no backward rule then extract the conclusions from the
	// forward rule
	if (backward_rule_handles_.empty())
	{
		Handle vardecl = get_forward_vardecl();
		for (const Handle& c : get_forward_conclusion_bodies())
			results.push_back({filter_vardecl(vardecl, c), c});
	}
	// There are backward rules so return directly their patterns
	else
	{
		for (const Handle& h : backward_rule_handles_)
		{
			Type t = h->getType();
			OC_ASSERT(t == BIND_LINK or t == GET_LINK);
			results.push_back({h->getOutgoingAtom(0), h->getOutgoingAtom(1)});
		}
	}

	return results;
}

void Rule::set_weight(float p)
{
	weight_ = p;
}

/**
 * Create a new rule where all variables are renamed.
 *
 * @param as  pointer to the atomspace where the new BindLink will be added
 * @return    a new Rule object with its own new BindLink
 */
Rule Rule::gen_standardize_apart(AtomSpace* as)
{
	if (forward_rule_handle_ == Handle::UNDEFINED)
		throw InvalidParamException(TRACE_INFO,
		                            "Attempted standardized-apart on "
		                            "invalid Rule");

	// clone the Rule
	Rule st_ver = *this;
	HandleMap dict;

	Handle vdecl = get_forward_vardecl();
	OrderedHandleSet varset;

	if (VariableListCast(vdecl))
		varset = VariableListCast(vdecl)->get_variables().varset;
	else
		varset.insert(vdecl);

	for (auto& h : varset)
		dict[h] = Handle::UNDEFINED;

	Handle st_bindlink = standardize_helper(as, forward_rule_handle_, dict);
	st_ver.set_forward_handle(st_bindlink);

	return st_ver;
}

/**
 * Basic helper function to standardize apart the BindLink.
 *
 * @param as     pointer to an atomspace where new atoms are added
 * @param h      an input atom to standardize apart
 * @param dict   a mapping of old VariableNode and new VariableNode
 * @return       the new atom
 */
Handle Rule::standardize_helper(AtomSpace* as, const Handle& h,
                                HandleMap& dict)
{
	if (h->isLink())
	{
		HandleSeq old_outgoing = h->getOutgoingSet();
		HandleSeq new_outgoing;

		for (auto ho : old_outgoing)
			new_outgoing.push_back(standardize_helper(as, ho, dict));

		return as->add_atom(createLink(h->getType(), new_outgoing,
		                               h->getTruthValue()));
	}

	// normal node does not need to be changed
	if (h->getType() != VARIABLE_NODE)
		return h;

	// If the VariableNode is not scoped by the rule's scope, but is
	// instead scoped by something generated by the output, then we
	// want to generate a completely unique variable
	if (dict.count(h) == 0)
	{
		std::string new_name = h->getName() + "-"
			+ to_string(boost::uuids::random_generator()());
		Handle hcpy = as->add_atom(createNode(h->getType(), new_name,
		                                      h->getTruthValue()));

		dict[h] = hcpy;

		return hcpy;
	}

	// use existing mapping if the VariableNode is already mapped
	if (dict.at(h) != Handle::UNDEFINED)
		return dict[h];

	std::string new_name = h->getName() + "-" + name_;
	Handle hcpy = as->add_atom(createNode(h->getType(), new_name,
	                                      h->getTruthValue()));

	dict[h] = hcpy;

	return hcpy;
}

HandleSeq Rule::get_forward_conclusion_bodies() const
{
	HandleSeq results;
	Handle implicand = get_forward_implicand();
	Type t = implicand->getType();
	if (EXECUTION_OUTPUT_LINK == t) {
		results.push_back(get_execution_output_last_argument(implicand));
	} else if (LIST_LINK == t) {
		for (const Handle& h : implicand->getOutgoingSet()) {
			t = h->getType();
			if (EXECUTION_OUTPUT_LINK == t)
				results.push_back(get_execution_output_last_argument(h));
			else
				results.push_back(h);
		}
	} else {
		results.push_back(implicand);
	}
	return results;
}

Handle Rule::get_execution_output_last_argument(const Handle& h) const
{
	OC_ASSERT(h->getType() == EXECUTION_OUTPUT_LINK);
	Handle args = h->getOutgoingAtom(1);
	OC_ASSERT(args->getType() == LIST_LINK and args->getArity() > 0);
	return args->getOutgoingAtom(args->getArity()-1);
}
