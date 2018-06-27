/*
 * Rule.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com> 2015
 *          Nil Geisweiller 2015-2016
 *          Shujing Ke 2018
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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/pattern/BindLink.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/unify/Unify.h>

#include <opencog/query/BindLinkAPI.h>

#include "URELogger.h"

#include "Rule.h"

namespace opencog {

void RuleSet::expand_meta_rules(AtomSpace& as)
{
	for (const Rule& rule : *this) {
		if (rule.is_meta()) {
			Handle result = rule.apply(as);
			for (const Handle& produced_h : result->getOutgoingSet()) {
				Rule produced(rule.get_alias(), produced_h, rule.get_rbs());
				if (find(produced) == end()) {
					insert(produced);
					ure_logger().debug() << "New rule produced from meta rule:"
					                     << std::endl << oc_to_string(produced);
				}
			}
		}
	}
}

Rule::Rule()
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED) {}

Rule::Rule(const Handle& rule_member)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_member);
}

Rule::Rule(const Handle& rule_alias, const Handle& rbs)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_alias, rbs);
}

Rule::Rule(const Handle& rule_alias, const Handle& rule, const Handle& rbs)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_alias, rule, rbs);
}

void Rule::init(const Handle& rule_member)
{
	OC_ASSERT(rule_member != Handle::UNDEFINED);
	if (not nameserver().isA(rule_member->get_type(), MEMBER_LINK))
		throw InvalidParamException(TRACE_INFO,
		                            "Rule '%s' is expected to be a MemberLink",
		                            rule_member->to_string().c_str());

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
	OC_ASSERT(rule->get_type() == BIND_LINK);
	_rule = BindLinkCast(rule);

	_rule_alias = rule_alias;
	_name = _rule_alias->get_name();
	_rbs = rbs;
	AtomSpace& as = *rule_alias->getAtomSpace();
	Handle ml = as.get_link(MEMBER_LINK, rule_alias, rbs);
	_tv = ml->getTruthValue();

    verify_rule();
}

bool Rule::verify_rule()
{
    // currently do not verify meta rules
    if (is_meta())
        return true;

    Handle rewrite = _rule->get_implicand();
    Type rewrite_type = rewrite->get_type();

    // check 1: If there are multiple conclusions
    if ((rewrite_type == AND_LINK) || (rewrite_type == LIST_LINK))
    {
        logger().warn() << "\nRule::verify_rule: " << _rule_alias->get_name()
                        << " contains multiple conclusions.\n"
                        << "This rule will not work in backwardchainer.\n"
                        << "All the conclusions should be wrapped with an ExecutionOutPutLink.\n"
                        << "Please check /atomspace/examples/rule-engine/DummyExecutionOutput.scm for example."
                        << std::endl;
        return false;
    }

    return true;
}

bool Rule::operator==(const Rule& r) const
{
	return content_eq(Handle(_rule), Handle(r._rule));
}

bool Rule::operator<(const Rule& r) const
{
	return content_based_handle_less()(Handle(_rule), Handle(r._rule));
}

bool Rule::is_alpha_equivalent(const Rule& r) const
{
	return _rule->is_equal(Handle(r._rule));
}

TruthValuePtr Rule::get_tv() const
{
	return _tv;
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

void Rule::set_rule(const Handle& h)
{
	_rule = BindLinkCast(h);
}

Handle Rule::get_rule() const
{
	return Handle(_rule);
}

Handle Rule::get_alias() const
{
	return _rule_alias;
}

Handle Rule::get_definition() const
{
	return DefineLink::get_definition(_rule_alias);
}

Handle Rule::get_rbs() const
{
	return _rbs;
}

void Rule::add(AtomSpace& as)
{
	if (!_rule)
		return;

	HandleSeq outgoings;
	for (const Handle& h : _rule->getOutgoingSet())
		outgoings.push_back(as.add_atom(h));
	_rule = createBindLink(outgoings);
}

Handle Rule::get_vardecl() const
{
	if (_rule)
		return _rule->get_vardecl();
	return Handle::UNDEFINED;
}

/**
 * Get the implicant (input) of the rule defined in a BindLink.
 *
 * @return the Handle of the implicant
 */
Handle Rule::get_implicant() const
{
	if (_rule)
		return _rule->get_body();
	return Handle::UNDEFINED;
}

Handle Rule::get_implicand() const
{
	if (_rule)
		return _rule->get_implicand();
	return Handle::UNDEFINED;
}

bool Rule::is_valid() const
{
	return (bool)_rule;
}

bool Rule::is_meta() const
{
	Handle implicand = get_implicand();

	if (not implicand)
		return false;

	Type itype = implicand->get_type();
	return (Quotation::is_quotation_type(itype) ?
	        implicand->getOutgoingAtom(0)->get_type() : itype) == BIND_LINK;
}

bool Rule::has_cycle() const
{
	// Return true iff at least one premise is equal to the conclusion
	const Handle c = get_conclusion();
	for (const Handle& h : get_premises())
		if (content_eq(c, h))
			return true;
	return false;
}

/**
 * Get the set of members of the implicant which are
 * connected by a root logical link.
 *
 * @return HandleSeq of members of the implicant
 */
HandleSeq Rule::get_clauses() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandleSeq();

    Handle implicant = get_implicant();
    Type t = implicant->get_type();
    HandleSeq hs;

    if (t == AND_LINK or t == OR_LINK)
        hs = implicant->getOutgoingSet();
    else
        hs.push_back(implicant);

    return hs;
}

HandleSeq Rule::get_premises() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandleSeq();

	Handle rewrite = _rule->get_implicand();
	Type rewrite_type = rewrite->get_type();

	// If not an ExecutionOutputLink then return the clauses
	if (premises_as_clauses or rewrite_type != EXECUTION_OUTPUT_LINK)
		return get_clauses();

	// Otherwise search the premises in the rewrite term's ExecutionOutputLink
	HandleSeq premises;
	if (rewrite_type == EXECUTION_OUTPUT_LINK) {
		Handle args = rewrite->getOutgoingAtom(1);
		if (args->get_type() == LIST_LINK) {
			OC_ASSERT(args->get_arity() > 0);
			for (Arity i = 1; i < args->get_arity(); i++) {
				Handle argi = args->getOutgoingAtom(i);
				// Return unordered premises
				if (argi->get_type() == SET_LINK) {
					for (Arity j = 0; j < argi->get_arity(); j++)
						premises.push_back(argi->getOutgoingAtom(j));
				}
				// Return ordered premise
				else {
					premises.push_back(argi);
				}
			}
		}
	}
	return premises;
}

Handle Rule::get_conclusion() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return Handle::UNDEFINED;

	Handle rewrite = _rule->get_implicand();
	Type rewrite_type = rewrite->get_type();

	// If not an ExecutionOutputLink then return the rewrite term
	if (rewrite_type != EXECUTION_OUTPUT_LINK)
		return rewrite;

	Handle args = rewrite->getOutgoingAtom(1);
	if (args->get_type() == LIST_LINK) {
		OC_ASSERT(args->get_arity() > 0);
		return args->getOutgoingAtom(0);
	} else {
		return args;
	}

}

HandlePairSeq Rule::get_conclusions() const
{
	HandlePairSeq results;

	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandlePairSeq();

	Handle vardecl = get_vardecl();
	for (const Handle& c : get_conclusion_patterns())
		results.push_back({filter_vardecl(vardecl, c), c});

	return results;
}

Rule Rule::gen_standardize_apart(AtomSpace* as)
{
	if (!_rule)
		throw InvalidParamException(TRACE_INFO,
		                            "Attempted standardized-apart on "
		                            "invalid Rule");

	// clone the Rule
	Rule st_ver = *this;
	HandleMap dict;

	Handle vdecl = get_vardecl();
	HandleSet varset;

	if (VariableListCast(vdecl))
		varset = VariableListCast(vdecl)->get_variables().varset;
	else
		varset.insert(vdecl);

	for (auto& h : varset)
		dict[h] = Handle::UNDEFINED;

	Handle st_bindlink = standardize_helper(as, Handle(_rule), dict);
	st_ver.set_rule(st_bindlink);

	return st_ver;
}

RuleTypedSubstitutionMap Rule::unify_source(const Handle& source,
                                            const Handle& vardecl,
                                            const AtomSpace* queried_as) const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return {};

	// To guarantee that the rule variable does not have the same name
	// as any variable in the source. XXX This is only a stochastic
	// guarantee, there is a small chance that the new random name
	// will still collide.
	Rule alpha_rule = rand_alpha_converted();

	RuleTypedSubstitutionMap unified_rules;
	Handle rule_vardecl = alpha_rule.get_vardecl();
	for (const Handle& premise : alpha_rule.get_premises())
	{
		Unify unify(source, premise, vardecl, rule_vardecl);
		Unify::SolutionSet sol = unify();
		if (sol.is_satisfiable()) {
			Unify::TypedSubstitutions tss =
				unify.typed_substitutions(sol, source);
			// For each typed substitution produce a new rule by
			// substituting all variables by their associated
			// values.
			for (const auto& ts : tss)
				unified_rules.insert({alpha_rule.substituted(ts, queried_as), ts});
		}
	}

	return unified_rules;
}

RuleTypedSubstitutionMap Rule::unify_target(const Handle& target,
                                            const Handle& vardecl,
                                            const AtomSpace* queried_as) const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return {};

	// To guarantee that the rule variable does not have the same name
	// as any variable in the target. XXX This is only a stochastic
	// guarantee, there is a small chance that the new random name
	// will still collide.
	Rule alpha_rule = rand_alpha_converted();

	RuleTypedSubstitutionMap unified_rules;
	Handle alpha_vardecl = alpha_rule.get_vardecl();
	for (const Handle& alpha_pat : alpha_rule.get_conclusion_patterns())
	{
		Unify unify(target, alpha_pat, vardecl, alpha_vardecl);
		Unify::SolutionSet sol = unify();
		if (sol.is_satisfiable()) {
			Unify::TypedSubstitutions tss =
				unify.typed_substitutions(sol, target);
			// For each typed substitution produce a new rule by
			// substituting all variables by their associated
			// values.
			for (const auto& ts : tss) {
				unified_rules.insert({alpha_rule.substituted(ts, queried_as), ts});
			}
		}
	}

	return unified_rules;
}

RuleSet Rule::strip_typed_substitution(const RuleTypedSubstitutionMap& rules)
{
	RuleSet rs;
	for (const auto& r : rules)
		rs.insert(r.first);

	return rs;
}

Handle Rule::apply(AtomSpace& as) const
{
	return bindlink(&as, Handle(_rule));
}

std::string Rule::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "name: " << _name << std::endl
	   << indent << "rule:" << std::endl
	   << _rule->to_string(indent + OC_TO_STRING_INDENT);
	return ss.str();
}

Rule Rule::rand_alpha_converted() const
{
	// Clone the rule
	Rule result = *this;

	// Alpha convert the rule
	result.set_rule(_rule->alpha_convert());

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
	if (h->is_link())
	{
		HandleSeq old_outgoing = h->getOutgoingSet();
		HandleSeq new_outgoing;

		for (auto ho : old_outgoing)
			new_outgoing.push_back(standardize_helper(as, ho, dict));

		Handle hcpy(as->add_atom(createLink(new_outgoing, h->get_type())));
		hcpy->copyValues(h);
		return hcpy;
	}

	// normal node does not need to be changed
	if (h->get_type() != VARIABLE_NODE)
		return h;

	// If the VariableNode is not scoped by the rule's scope, but is
	// instead scoped by something generated by the output, then we
	// want to generate a completely unique variable
	if (dict.count(h) == 0)
	{
		// TODO: use opencog's random generator
		std::string new_name = h->get_name() + "-"
			+ boost::uuids::to_string(boost::uuids::random_generator()());

		Handle hcpy(as->add_atom(createNode(h->get_type(), new_name)));
		hcpy->copyValues(h);

		dict[h] = hcpy;
		return hcpy;
	}

	// use existing mapping if the VariableNode is already mapped
	if (dict.at(h) != Handle::UNDEFINED)
		return dict[h];

	std::string new_name = h->get_name() + "-" + _name;
	Handle hcpy(as->add_atom(createNode(h->get_type(), new_name)));
	hcpy->copyValues(h);

	dict[h] = hcpy;
	return hcpy;
}

HandleSeq Rule::get_conclusion_patterns() const
{
	HandleSeq results;
	Handle implicand = get_implicand();
	Type t = implicand->get_type();
	if (LIST_LINK == t)
		for (const Handle& h : implicand->getOutgoingSet())
			results.push_back(get_conclusion_pattern(h));
	else
		results.push_back(get_conclusion_pattern(implicand));

	return results;
}

Handle Rule::get_conclusion_pattern(const Handle& h) const
{
	Type t = h->get_type();
	if (EXECUTION_OUTPUT_LINK == t)
		return get_execution_output_first_argument(h);
	else
		return h;
}

Handle Rule::get_execution_output_first_argument(const Handle& h) const
{
	OC_ASSERT(h->get_type() == EXECUTION_OUTPUT_LINK);
	Handle args = h->getOutgoingAtom(1);
	if (args->get_type() == LIST_LINK) {
		OC_ASSERT(args->get_arity() > 0);
		return args->getOutgoingAtom(0);
	} else
		return args;
}

Rule Rule::substituted(const Unify::TypedSubstitution& ts,
                       const AtomSpace* queried_as) const
{
	Rule new_rule(*this);
	new_rule.set_rule(Unify::substitute(_rule, ts, queried_as));
	return new_rule;
}

std::string oc_to_string(const Rule& rule, const std::string& indent)
{
	return rule.to_string(indent);
}
std::string oc_to_string(const RuleSet& rules, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << rules.size() << std::endl;
	size_t i = 0;
	for (const Rule& rule : rules)
		ss << indent << "rule[" << i++ << "]:" << std::endl
		   << oc_to_string(rule, indent + OC_TO_STRING_INDENT) << std::endl;
	return ss.str();
}
std::string oc_to_string(const RuleTypedSubstitutionPair& rule_ts,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "rule:" << std::endl
	   << oc_to_string(rule_ts.first, indent + OC_TO_STRING_INDENT)
	   << std::endl;
	ss << indent << "typed substitutions:" << std::endl
	   << oc_to_string(rule_ts.second, indent + OC_TO_STRING_INDENT)
	   << std::endl;
	return ss.str();
}
std::string oc_to_string(const RuleTypedSubstitutionMap& rules,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << rules.size() << std::endl;
	size_t i = 0;
	for (const RuleTypedSubstitutionPair& rule : rules)
		ss << indent << "rule[" << i++ << "]:" << std::endl
		   << oc_to_string(rule, indent + OC_TO_STRING_INDENT)
		   << std::endl;
	return ss.str();
}

} // ~namespace opencog
