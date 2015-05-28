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

#include "UREConfigReader.h"

#include <opencog/atoms/bind/BindLink.h>
#include <opencog/atoms/bind/DefineLink.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/query/BindLink.h>

using namespace opencog;

UREConfigReader::UREConfigReader(AtomSpace& as) : _as(as)
{
	// Iterate over all rule-based systems and retrieve their rules
	// and parameters
	for (Handle rbs : fetch_rule_based_systems()) {
		// Retrieve rule names and instantiate them
		for (Handle rn : fetch_rules(rbs)) {
			// Build rule and cache it in _sys2params
			Handle rule_h = fetch_definition(rn);
			_sys2params[rbs].rules.emplace_back(rule_h);
		}

		// Fetch maximum number of iterations
		_sys2params[rbs].max_iter = fetch_num_param(max_iter_name, rbs);

		// Fetch attention allocation parameter and cache it
		_sys2params[rbs].attention_alloc =
			fetch_bool_param(attention_alloc_name, rbs);
	}
}

const std::vector<Rule>& UREConfigReader::get_rules(Handle rbs) const
{
	return _sys2params.at(rbs).rules;
}

bool UREConfigReader::get_attention_allocation(Handle rbs) const
{
	return _sys2params.at(rbs).attention_alloc;
}

int UREConfigReader::get_maximum_iterations(Handle rbs) const
{
	return _sys2params.at(rbs).max_iter;
}

HandleSeq UREConfigReader::fetch_rule_based_systems()
{
    // Retrieve all rule-based systems inheriting URE_top_rulebase
	Handle rbs_var = _as.addNode(VARIABLE_NODE, "__URE_RBS__");
	BindLink bl({rbs_var,
				// Clause:
				// InheritanceLink
				//    VariableNode "__URE_SUB_SYSTEMS__"
				//    ConceptNode URE_top_name
				_as.addLink(INHERITANCE_LINK,
				            rbs_var,
				            _as.addNode(CONCEPT_NODE,
				                        URE_top_name)),
				// Rewrite: (all inherited rule-based systems)
				rbs_var});
	Handle bl_h = bl.getHandle();
	Handle rule_based_systems = bindlink(&_as, bl_h);

	// Remove the BindLink from the AtomSpace as it is no longer useful
	_as.removeAtom(bl_h);

	return LinkCast(rule_based_systems)->getOutgoingSet();
}

HandleSeq UREConfigReader::fetch_rules(Handle rbs)
{
	// Retrieve rules
	Handle rule_var = _as.addNode(VARIABLE_NODE, "__URE_RULE__");
	BindLink bl({rule_var,
				// Clause:
				// MemberLink
				//    VariableNode "__URE_RULE__"
				//    ConceptNode <RBS>
				_as.addLink(MEMBER_LINK, rule_var, rbs),
				// Rewrite: (all member rules)
				rule_var});
	Handle rule_names = bindlink(&_as, bl.getHandle());

	return LinkCast(rule_names)->getOutgoingSet();
}

Handle UREConfigReader::fetch_definition(Handle label)
{
	HandleSeq defines;
	label->getIncomingSetByType(back_inserter(defines), DEFINE_LINK);
	OC_ASSERT(defines.size() == 1, "There should be only one definition");
	return Handle::UNDEFINED; // DefineLinkCast(defines.front())->get_definition();
                              // commented out till get_definition is
                              // extended to any object not just
                              // functions.
}

Handle UREConfigReader::fetch_execution_output(Handle schema, Handle input)
{
	// Retrieve rules
	Handle output_var = _as.addNode(VARIABLE_NODE, "__EXECUTION_OUTPUT_VAR__");
	BindLink bl({output_var,
				// Clause:
				// ExecutionLink
				//    <schema>
				//    <input>
				//    output_var
				_as.addLink(EXECUTION_LINK,
				            schema,
				            input,
				            output_var),
				// Rewrite: (all outputs)
				output_var});
	Handle bl_h = bl.getHandle();
	Handle outputs = bindlink(&_as, bl_h);

	// Remove the BindLink from the AtomSpace as it is no longer useful
	_as.removeAtom(bl_h);

	HandleSeq outputs_hs = LinkCast(outputs)->getOutgoingSet();
	OC_ASSERT(outputs_hs.size() == 1, "There should be only one output");
	
	return outputs_hs.front();
}

double UREConfigReader::fetch_num_param(const string& schema_name, Handle input)
{
	Handle param_schema = _as.addNode(SCHEMA_NODE, schema_name);
	Handle output_h = fetch_execution_output(param_schema, input);
	return NumberNodeCast(output_h)->getValue();
}

bool UREConfigReader::fetch_bool_param(const string& pred_name, Handle input)
{
	Handle pred = _as.addNode(PREDICATE_NODE, pred_name);
	TruthValuePtr tv = _as.addLink(EVALUATION_LINK, pred, input)->getTruthValue();
	return tv->getMean() > 0.5;
}
