/*
 * UREConfigReader.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com>
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

using namespace opencog;

UREConfigReader::UREConfigReader(AtomSpace& as) : _as(as)
{
	;
	// Iterate over all rule-based systems and retrieve their rules
	// and parameters
	for (Handle rbs : fetch_rule_based_systems()) {
		// Retrieve rule names and instantiate all associated rules
		for (Handle rn : fetch_rules(rbd)) {
			// TODO:
			// 1. get the rule Handle
			// 2. construct the rule
			// 3. put it in _sys2params[system].rules = Rule
		}

		// Fetch parameters for that system and put them in
		// _sys2params[system].<PARAM_NAME>
	}
}

const std::vector<Rule*>&
UREConfigReader::get_rules(const std::string& system) const
{
	return _sys2params.at(system).rules;
}

bool UREConfigReader::get_attention_allocation(const std::string& system) const
{
	return _sys2params.at(sysem).attention_alloc;
}

int UREConfigReader::get_maximum_iteration(const std::string& system) const
{
	return _sys2params.at(system).max_iter;
}

HandleSeq UREConfigReader::fetch_rule_based_systems()
{
    // Retrieve all rule-based systems inheriting URE_top_rulebase
	Handle sys_var = as.addNode(VARIABLE_NODE, "__URE_SYSTEM__");
	BindLink bl({sys_var,
				as.addLink(IMPLICATION_LINK,
				           // Clause:
				           // InheritanceLink
				           //    VariableNode "__URE_SUB_SYSTEMS__"
				           //    ConceptNode URE_top_name
				           as.addLink(INHERITANCE_LINK,
				                      var,
				                      as.addNode(CONCEPT_NODE,
				                                 URE_top_name)),
				           // Rewrite: (all inherited systems)
				           sys_var)});
	Handle rule_based_systems = bindlink(as, bl);

	// Remove the BindLink from the AtomSpace as it is no longer useful
	as.removeAtom(bl.getHandle());

	return LinkCast(rule_based_systems)->getOutgoingSet();
}

HandleSeq UREConfigReader::fetch_rules(Handle rule_based_system)
{
	// Retrieve rules
	Handle rule_var = as.addNode(VARIABLE_NODE, "__URE_RULE__");
	BindLink bl({rule_var,
				as.addLink(IMPLICATION_LINK,
				           // Clause:
				           // MemberLinkLink
				           //    VariableNode "__URE_RULE__"
				           //    ConceptNode <SYSTEM>
				           as.addLink(MEMBER_LINK,
				                      rule_var,
				                      rule_base_system),
				           // Rewrite: (all member rules)
				           rule_var)});
	Handle rule_names = bindlink(as, bl);

	returnfor LinkCast(rule_names)->getOutgoingSet();
	
}
