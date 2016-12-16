/*
 * Rule.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com> 2015
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

#include <queue>

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Quotation.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/pattern/BindLink.h>

#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atomutils/Unify.h>

#include <opencog/query/BindLinkAPI.h>

#include "Rule.h"

namespace opencog {

void RuleSet::expand_meta_rules(AtomSpace& as)
{
	for (const Rule& rule : *this) {
		if (rule.is_meta()) {
			Handle result = rule.apply(as);
			for (const Handle& produced_h : result->getOutgoingSet()) {
				Rule produced(rule.get_alias(), produced_h, rule.get_rbs());
				this->insert(produced);
			}
		}
	}
}

Rule::Rule()
	: _rule_alias(Handle::UNDEFINED) {}

Rule::Rule(const Handle& rule_member)
{
	init(rule_member);
}

Rule::Rule(const Handle& rule_alias, const Handle& rbs)
{
	init(rule_alias, rbs);
}

Rule::Rule(const Handle& rule_alias, const Handle& rule, const Handle& rbs)
{
	init(rule_alias, rule, rbs);
}

void Rule::init(const Handle& rule_member)
{
	OC_ASSERT(rule_member != Handle::UNDEFINED);
	if (not classserver().isA(rule_member->getType(), MEMBER_LINK))
		throw InvalidParamException(TRACE_INFO,
		                            "Rule '%s' is expected to be a MemberLink",
		                            rule_member->toString().c_str());

	Handle rule_alias = rule_member->getOutgoingAtom(0);
	Handle rbs = rule_member->getOutgoingAtom(1);
	init(rule_alias, rbs);
}

void Rule::init(const Handle& rule_alias, const Handle& rbs)
{
	Handle rule = DefineLink::get_definition(rule_alias);
	init(rule_alias, rule, rbs);
}

void Rule::init(const Handle& rule_alias, const Handle& rule, const Handle& rbs)
{
	if (rule->getType() == LIST_LINK)
	{
		OC_ASSERT(rule->getArity() > 0);
		// Split the rule into a forward and backward parts
		_forward_rule = BindLinkCast(rule->getOutgoingAtom(0));
		for (unsigned i = 1; i < rule->getArity(); i++) {
			Handle bch = rule->getOutgoingAtom(i);
			_backward_rule_handles.push_back(bch);
			_backward_rule_scope_links.push_back(ScopeLinkCast(bch));
		}
	}
	else
	{
		OC_ASSERT(rule->getType() == BIND_LINK);
		_forward_rule = BindLinkCast(rule);
	}

	_rule_alias = rule_alias;
	_name = _rule_alias->getName();
	_rbs = rbs;
	_weight = rule->getTruthValue()->getMean();
}

bool Rule::operator==(const Rule& r) const
{
	return r._forward_rule == _forward_rule
		and r._backward_rule_handles == _backward_rule_handles;
}

bool Rule::operator<(const Rule& r) const
{
	return _weight == r._weight ?
		Handle(_forward_rule).value() < Handle(r._forward_rule).value()
		: _weight < r._weight;
}

bool Rule::is_alpha_equivalent(const Rule& r) const
{
	if (not _forward_rule->is_equal(Handle(r._forward_rule)))
		return false;

	size_t n_backrules = r._backward_rule_scope_links.size();
	if (n_backrules != _backward_rule_scope_links.size())
		return false;

	for (size_t i = 0; i < n_backrules; i++) {
		if (not _backward_rule_scope_links[i]->is_equal(r._backward_rule_handles[i]))
			return false;
	}
	return true;
}

double Rule::get_weight() const
{
	return _weight;
}

void Rule::set_name(const std::string& name)
{
	_name = name;
}

std::string& Rule::get_name()
{
	return _name;
}

const std::string& Rule::get_name() const
{
	return _name;
}

void Rule::set_forward_handle(const Handle& h)
{
	_forward_rule = BindLinkCast(h);
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
	return Handle(_forward_rule);
}

Handle Rule::get_alias() const
{
	return _rule_alias;
}

Handle Rule::get_rbs() const
{
	return _rbs;
}

void Rule::add(AtomSpace& as)
{
	if (!_forward_rule)
		return;

	HandleSeq outgoings;
	for (const Handle& h : _forward_rule->getOutgoingSet())
		outgoings.push_back(as.add_atom(h));
	_forward_rule = createBindLink(outgoings);

	// TODO: support backward rule
}

Handle Rule::get_forward_vardecl() const
{
	if (_forward_rule)
		return _forward_rule->get_vardecl();
	return Handle::UNDEFINED;
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
	if (_forward_rule)
		return _forward_rule->get_body();
	return Handle::UNDEFINED;
}

Handle Rule::get_forward_implicand() const
{
	if (_forward_rule)
		return _forward_rule->get_implicand();
	return Handle::UNDEFINED;
}

bool Rule::is_valid() const
{
	return (bool)_forward_rule;
}

bool Rule::is_meta() const
{
	Handle implicand = get_forward_implicand();

	if (implicand.is_undefined())
		return false;

	Type itype = implicand->getType();
	return (Quotation::is_quotation_type(itype) ?
	        implicand->getOutgoingAtom(0)->getType() : itype) == BIND_LINK;
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
	if (!_forward_rule)
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
 *
 * TODO: indentical to get_forward_implicand()
 */
Handle Rule::get_forward_conclusion() const
{
	if (_forward_rule)
		return _forward_rule->get_implicand();
	return Handle::UNDEFINED;
}

HandlePairSeq Rule::get_conclusions() const
{
	HandlePairSeq results;

	// If the rule's handle has not been set yet
	if (!_forward_rule)
		return HandlePairSeq();

	// If no backward rule then extract the conclusions from the
	// forward rule
	if (_backward_rule_handles.empty())
	{
		Handle vardecl = get_forward_vardecl();
		for (const Handle& c : get_forward_conclusion_patterns())
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

void Rule::set_weight(double p)
{
	_weight = p;
}

Rule Rule::gen_standardize_apart(AtomSpace* as)
{
	if (!_forward_rule)
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

	Handle st_bindlink = standardize_helper(as, Handle(_forward_rule), dict);
	st_ver.set_forward_handle(st_bindlink);

	return st_ver;
}

RuleSet Rule::unify_source(const Handle& source,
                           const Handle& vardecl) const
{
	// TODO
	return {};
}

RuleSet Rule::unify_target(const Handle& target,
                           const Handle& vardecl) const
{
	// If the rule's handle has not been set yet
	if (!_forward_rule)
		return {};

	Rule alpha_rule = rand_alpha_converted();

	RuleSet unified_rules;

	// If no backward rule then only consider the conclusions from the
	// forward rule
	if (_backward_rule_handles.empty())
	{
		Handle alpha_vardecl = alpha_rule.get_forward_vardecl();
		for (const Handle& alpha_pat : alpha_rule.get_forward_conclusion_patterns())
		{
			UnificationSolutionSet sol =
				unify(target, alpha_pat, vardecl, alpha_vardecl);
			if (sol.satisfiable) {
				TypedSubstitutions tss = typed_substitutions(sol, target);
				// For each typed substitution produce a new rule by
				// substituting all variables by their associated
				// values.
				for (const auto& ts : tss)
					unified_rules.insert(alpha_rule.substituted(ts));
			}
		}
	}
	// There are backward rules so return directly their patterns
	else {
		std::stringstream ss;
		ss << "TODO: support backward rules (offending rule is `"
		   << get_name() << "')";
		OC_ASSERT(false, ss.str());
	}

	return unified_rules;
}

Handle Rule::apply(AtomSpace& as) const
{
	return bindlink(&as, Handle(_forward_rule));
}

std::string Rule::to_string() const
{
	std::stringstream ss;
	ss << "name: " << _name << std::endl
	   << "forward rule:" << std::endl
	   << _forward_rule->toString()
	   << "backward rules:" << std::endl
	   << oc_to_string(_backward_rule_handles);
	return ss.str();
}

Rule Rule::rand_alpha_converted() const
{
	// Clone the rule
	Rule result = *this;

	// Alpha convert the forward rule
	result.set_forward_handle(_forward_rule->alpha_conversion());

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

HandleSeq Rule::get_forward_conclusion_patterns() const
{
	HandleSeq results;
	Handle implicand = get_forward_implicand();
	Type t = implicand->getType();
	if (LIST_LINK == t)
		for (const Handle& h : implicand->getOutgoingSet())
			results.push_back(get_forward_conclusion_pattern(h));
	else
		results.push_back(get_forward_conclusion_pattern(implicand));

	return results;
}

Handle Rule::get_forward_conclusion_pattern(const Handle& h) const
{
	Type t = h->getType();
	if (EXECUTION_OUTPUT_LINK == t)
		return get_execution_output_last_argument(h);
	else
		return h;
}

Handle Rule::get_execution_output_last_argument(const Handle& h) const
{
	OC_ASSERT(h->getType() == EXECUTION_OUTPUT_LINK);
	Handle args = h->getOutgoingAtom(1);
	if (args->getType() == LIST_LINK) {
		OC_ASSERT(args->getArity() > 0);
		return args->getOutgoingAtom(args->getArity()-1);
	} else
		return args;
}

Rule Rule::substituted(const TypedSubstitutions::value_type& ts) const
{
	// Get the list of values to stustitute from ts
	HandleSeq values = _forward_rule->get_variables().make_values(ts.first);
	// Perform alpha-conversion, this will work over valiues that are
	// non variables as well
	Handle h = _forward_rule->alpha_conversion(values, ts.second);

	Rule new_rule(*this);
	new_rule.set_forward_handle(h);

	// If the quotation are useless or even harmful, then consume them
	if (is_bad_quotation(BindLinkCast(new_rule.get_forward_rule())))
		new_rule.consume_quotations();

	return new_rule;
}

bool Rule::is_bad_quotation(BindLinkPtr bl) const
{
	return bl->get_vardecl().is_undefined();
}

bool Rule::is_pm_connector(const Handle& h) const
{
	return is_pm_connector(h->getType());
}

bool Rule::is_pm_connector(Type t) const
{
	return t == AND_LINK or t == OR_LINK or t == NOT_LINK;
}

void Rule::consume_quotations()
{
	OC_ASSERT(_forward_rule->get_vardecl().is_undefined(),
	          "Should only consume quotations of BindLink without variable")
	Handle pattern = _forward_rule->get_body();
	Handle rewrite = _forward_rule->get_implicand();

	// Consume the pattern's quotations
	if (pattern->getType() == LOCAL_QUOTE_LINK
	    and is_pm_connector(pattern->getOutgoingAtom(0))) {
		Handle connector = consume_quotations(pattern->getOutgoingAtom(0));
		pattern = createLink(LOCAL_QUOTE_LINK, connector);
	} else
		pattern = consume_quotations(pattern);

	// Consume the rewrite's quotations
	rewrite = consume_quotations(rewrite);

	// Recreate the BindLink
	_forward_rule = createBindLink(pattern, rewrite);
}

Handle Rule::consume_quotations(Handle h, Quotation quotation)
{
	// Base case
	if (h->isNode())
		return h;

	// Recursive cases
	Type t = h->getType();
	if (quotation.consumable(t)) {
		quotation.update(t);
		return consume_quotations(h->getOutgoingAtom(0), quotation);
	}

	quotation.update(t);
	HandleSeq consumed;
	for (const Handle outh : h->getOutgoingSet())
		consumed.push_back(consume_quotations(outh, quotation));

	// TODO: call all factories
	bool is_scope = classserver().isA(t, SCOPE_LINK);
	return is_scope ? Handle(ScopeLink::factory(t, consumed))
		: Handle(createLink(t, consumed));
}

std::string oc_to_string(const Rule& rule)
{
	return rule.to_string();
}

std::string oc_to_string(const RuleSet& rules)
{
	std::stringstream ss;
	ss << "size = " << rules.size() << std::endl;
	size_t i = 0;
	for (const Rule& rule : rules)
		ss << "rule[" << i++ << "]:" << std::endl
		   << oc_to_string(rule) << std::endl;
	return ss.str();
}

} // ~namespace opencog
