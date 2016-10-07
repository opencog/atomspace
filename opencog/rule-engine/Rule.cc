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
#include <opencog/atomutils/Unify.h>

#include "Rule.h"

namespace opencog {

Rule::Rule()
	: _forward_rule_handle(Handle::UNDEFINED), _rule_alias(Handle::UNDEFINED) {}

Rule::Rule(const Handle& rule_ml)
	: _forward_rule_handle(Handle::UNDEFINED)
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

	_rule_alias = rule_ml->getOutgoingAtom(0);
	Handle rbs = rule_ml->getOutgoingAtom(1);

	Handle rule = DefineLink::get_definition(_rule_alias);
	if (rule->getType() == LIST_LINK)
	{
		OC_ASSERT(rule->getArity() > 0);
		// Split the rule into a forward and backward parts
		_forward_rule_handle = rule->getOutgoingAtom(0);
		_forward_rule_scope_link = ScopeLinkCast(_forward_rule_handle);
		for (unsigned i = 1; i < rule->getArity(); i++) {
			Handle bch = rule->getOutgoingAtom(i);
			_backward_rule_handles.push_back(bch);
			_backward_rule_scope_links.push_back(ScopeLinkCast(bch));
		}
	}
	else
	{
		OC_ASSERT(rule->getType() == BIND_LINK);
		_forward_rule_handle = rule;
		_forward_rule_scope_link = ScopeLinkCast(rule);
	}
	_name = _rule_alias->getName();
	_category = rbs->getName();
	_weight = rule->getTruthValue()->getMean();
}

float Rule::get_weight() const
{
	return _weight;
}

void Rule::set_category(const string& name)
{
	_category = name;
}

string& Rule::get_category()
{
	return _category;
}

const string& Rule::get_category() const
{
	return _category;
}

void Rule::set_name(const string& name)
{
	_name = name;
}

string& Rule::get_name()
{
	return _name;
}

const string& Rule::get_name() const
{
	return _name;
}

void Rule::set_forward_handle(const Handle& h)
{
	_forward_rule_handle = h;
	_forward_rule_scope_link = ScopeLinkCast(h);
}

void Rule::set_backward_handles(const HandleSeq& hs)
{
	_backward_rule_handles = hs;
	_backward_rule_scope_links.clear();
	for (const Handle& h : hs)
		_backward_rule_scope_links.push_back(ScopeLinkCast(h));
}

Handle Rule::get_forward_rule() const
{
	return _forward_rule_handle;
}

Handle Rule::get_alias() const
{
	return _rule_alias;
}

/**
 * Get the typed variable list of the Rule.
 *
 * @return the VariableList or the lone VariableNode
 */
Handle Rule::get_forward_vardecl() const
{
	// if the rule's handle has not been set yet
	if (_forward_rule_handle == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return _forward_rule_handle->getOutgoingAtom(0);
}

/**
 * Get the typed variable list of the Rule.
 *
 * @return the VariableList or the lone VariableNode
 */
HandleSeq Rule::get_backward_vardecls() const
{
	HandleSeq results;
	for (const Handle& h : _backward_rule_handles)
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
	if (_forward_rule_handle == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(_forward_rule_handle)->get_body();
}

Handle Rule::get_forward_implicand() const
{
	return BindLinkCast(_forward_rule_handle)->get_implicand();
}

bool Rule::is_valid() const
{
	return _forward_rule_handle != Handle::UNDEFINED;
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
	if (_forward_rule_handle == Handle::UNDEFINED)
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
	if (_forward_rule_handle == Handle::UNDEFINED)
		return Handle::UNDEFINED;

	return BindLinkCast(_forward_rule_handle)->get_implicand();
}

HandlePairSeq Rule::get_conclusions() const
{
	HandlePairSeq results;

	// If the rule's handle has not been set yet
	if (_forward_rule_handle == Handle::UNDEFINED)
		return HandlePairSeq();

	// If no backward rule then extract the conclusions from the
	// forward rule
	if (_backward_rule_handles.empty())
	{
		Handle vardecl = get_forward_vardecl();
		for (const Handle& c : get_forward_conclusion_bodies())
			results.push_back({filter_vardecl(vardecl, c), c});
	}
	// There are backward rules so return directly their patterns
	else
	{
		for (const Handle& h : _backward_rule_handles)
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
	_weight = p;
}

Rule Rule::gen_standardize_apart(AtomSpace* as)
{
	if (_forward_rule_handle == Handle::UNDEFINED)
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

	Handle st_bindlink = standardize_helper(as, _forward_rule_handle, dict);
	st_ver.set_forward_handle(st_bindlink);

	return st_ver;
}

std::vector<Rule> Rule::unify_source(const Handle& source,
                                     const Handle& vardecl)
{
	// TODO
	return {};
}

std::vector<Rule> Rule::unify_target(const Handle& target,
                                     const Handle& vardecl)
{
	// If the rule's handle has not been set yet
	if (_forward_rule_handle == Handle::UNDEFINED)
		return {};

	Rule alpha_rule = rand_alpha_converted();

	std::vector<Rule> unified_rules;

	// If no backward rule then only consider the conclusions from the
	// forward rule
	if (_backward_rule_handles.empty())
	{
		Handle alpha_vardecl = alpha_rule.get_forward_vardecl();
		ScopeLinkPtr alpha_sc = alpha_rule._forward_rule_scope_link;
		for (const Handle& alpha_pat : alpha_rule.get_forward_conclusion_bodies())
		{
			UnificationSolutionSet sol =
				unify(target, alpha_pat, vardecl, alpha_vardecl);
			if (sol.satisfiable) {
				TypedSubstitutions tss = typed_substitutions(sol, target);
				for (const auto& ts : tss) {
					// For each typed substitution produce an alpha
					// converted, possibly partially substituted rule
					HandleSeq values = alpha_sc->get_variables().make_values(ts.first);
					Handle h = alpha_sc->alpha_conversion(values, ts.second);
					Rule unified_rule(alpha_rule);
					unified_rule.set_forward_handle(h);
					unified_rules.push_back(unified_rule);
				}
			}
		}
	}
	// There are backward rules so return directly their patterns
	else {
		OC_ASSERT(false, "TODO: support backward rules");
	}

	return unified_rules;
}

Rule Rule::rand_alpha_converted() const
{
	// Clone the rule
	Rule result = *this;

	// Alpha convert the forward rule
	result.set_forward_handle(_forward_rule_scope_link->alpha_conversion());

	// Alpha convert the backward rules
	HandleSeq bhs;
	for (ScopeLinkPtr sc : _backward_rule_scope_links)
		bhs.push_back(Handle(sc->alpha_conversion()));
	result.set_backward_handles(bhs);

	return result;
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
		// TODO: use opencog's random generator
		std::string new_name = h->getName() + "-"
			+ boost::uuids::to_string(boost::uuids::random_generator()());
		Handle hcpy = as->add_atom(createNode(h->getType(), new_name,
		                                      h->getTruthValue()));

		dict[h] = hcpy;

		return hcpy;
	}

	// use existing mapping if the VariableNode is already mapped
	if (dict.at(h) != Handle::UNDEFINED)
		return dict[h];

	std::string new_name = h->getName() + "-" + _name;
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

std::string Rule::to_string() const
{
	std::stringstream ss;
	ss << "name: " << _name << std::endl
	   << "forward rule:" << std::endl
	   << oc_to_string(_forward_rule_handle)
	   << "backward rules:" << std::endl
	   << oc_to_string(_backward_rule_handles);
	return ss.str();
}

std::string oc_to_string(const Rule& rule)
{
	return rule.to_string();
}

std::string oc_to_string(const RuleSeq& rules)
{
	std::stringstream ss;
	ss << "size = " << rules.size() << std::endl;
	for (size_t i = 0; i < rules.size(); ++i)
		ss << "rule[" << i << "]:" << std::endl
		   << oc_to_string(rules[i]) << std::endl;
	return ss.str();
}

} // ~namespace opencog
