/*
 * Rule.cc
 *
 * Copyright (C) 2015 Misgana Bayetta
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
#include <opencog/atoms/bind/BindLink.h>

#include "Rule.h"

using namespace opencog;

Rule::Rule(Handle rule, float weight)
{
	rule_handle_ = rule;
	weight_ = weight;
}


Rule::~Rule()
{

}

float Rule::get_weight()
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

void Rule::set_handle(Handle h) throw (InvalidParamException)
{
	rule_handle_ = h;
}

Handle Rule::get_handle()
{
	return rule_handle_;
}

/**
 * Get the typed variable list of the Rule.
 *
 * @return the VariableList or the lone VariableNode
 */
Handle Rule::get_vardecl()
{
	return LinkCast(rule_handle_)->getOutgoingSet()[0];
}

/**
 * Get the implicant (input) of the rule defined in a BindLink.
 *
 * @return the Handle of the implicant
 */
Handle Rule::get_implicant()
{
	// if the rule's handle has not been set yet
	if (rule_handle_ == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(rule_handle_)->get_body();
}

/**
 * Get the set of members of the implicant which are
 * connected by a root logical link.
 *
 * @return HandleSeq of members of the implicant
 */
HandleSeq Rule::get_implicant_seq()
{
    Handle implicant= get_implicant();
    Type t = implicant->getType();
    HandleSeq hs;

    if (t == AND_LINK or t == OR_LINK)
        hs = LinkCast(implicant)->getOutgoingSet();
    else
        hs.push_back(implicant);

    return hs;
}
/**
 * Get the implicand (output) of the rule defined in a BindLink.
 *
 * @return the Handle of the implicand
 */
Handle Rule::get_implicand()
{
	// if the rule's handle has not been set yet
	if (rule_handle_ == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(rule_handle_)->get_implicand();
}

/**
 * Get the implicand (output) of the rule defined in a BindLink.
 *
 * This function does extra processing to find the real output over an
 * ExecutionOutputLink.  ie, skip to the ListLink under the ExLink.
 *
 * @return the HandleSeq of the implicand
 */
HandleSeq Rule::get_implicand_seq()
{
	// if the rule's handle has not been set yet
	if (rule_handle_ == Handle::UNDEFINED)
		return HandleSeq();

	Handle implicand = BindLinkCast(rule_handle_)->get_implicand();

	std::queue<Handle> pre_output;
	HandleSeq final_output;

	// skip the top level ListLink
	if (implicand->getType() == LIST_LINK)
	{
		for (Handle h : LinkCast(implicand)->getOutgoingSet())
			pre_output.push(h);
	}
	else
	{
		pre_output.push(implicand);
	}

	// check all output of ExecutionOutputLink
	while (not pre_output.empty())
	{
		Handle hfront = pre_output.front();
		pre_output.pop();

		if (hfront->getType() == EXECUTION_OUTPUT_LINK)
		{
			// get the ListLink containing the arguments of the ExecutionOutputLink
			Handle harg = LinkCast(hfront)->getOutgoingSet()[1];

			for (Handle h : LinkCast(harg)->getOutgoingSet())
				pre_output.push(h);

			continue;
		}

		// if not an ExecutionOutputLink, it is a final output
		final_output.push_back(hfront);
	}

	return final_output;
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
	// clone the Rule
	Rule st_ver = *this;
	std::map<Handle, Handle> dict;

	Handle st_bindlink = standardize_helper(as, rule_handle_, dict);
	st_ver.set_handle(st_bindlink);

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
Handle Rule::standardize_helper(AtomSpace* as, const Handle h, std::map<Handle, Handle>& dict)
{
	if (LinkCast(h))
	{
		HandleSeq old_outgoing = LinkCast(h)->getOutgoingSet();
		HandleSeq new_outgoing;

		for (auto ho : old_outgoing)
			new_outgoing.push_back(standardize_helper(as, ho, dict));

		return as->add_atom(createLink(h->getType(), new_outgoing, h->getTruthValue()));
	}

	// normal node does not need to be changed
	if (h->getType() != VARIABLE_NODE)
		return h;

	// use existing mapping if the VariableNode is already mapped
	if (dict.count(h) == 1)
		return dict[h];

	std::string new_name = NodeCast(h)->getName() + "-standardize_apart-" + to_string(boost::uuids::random_generator()());

	Handle hcpy = as->add_atom(createNode(h->getType(), new_name, h->getTruthValue()));
	dict[h] = hcpy;

	return hcpy;
}
