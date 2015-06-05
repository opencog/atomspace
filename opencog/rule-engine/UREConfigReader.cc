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

#include <opencog/atoms/NumberNode.h>
#include <opencog/query/BindLinkAPI.h>

using namespace opencog;

const std::string UREConfigReader::URE_top_name = "URE";
const std::string UREConfigReader::attention_alloc_name = "URE:attention-allocation";
const std::string UREConfigReader::max_iter_name = "URE:maximum-iterations";

UREConfigReader::UREConfigReader(AtomSpace& as, Handle rbs) : _as(as)
{
	// Retrieve rule names and instantiate them
	for (Handle rn : fetch_rules(rbs)) {
		// Build rule and store it
		Handle rule_h = fetch_definition(rn);
		_rbparams.rules.emplace_back(rule_h);
	}

	// Fetch maximum number of iterations
	_rbparams.max_iter = fetch_num_param(max_iter_name, rbs);

	// Fetch attention allocation parameter
	_rbparams.attention_alloc = fetch_bool_param(attention_alloc_name, rbs);
}

const std::vector<Rule>& UREConfigReader::get_rules() const
{
	return _rbparams.rules;
}

std::vector<Rule>& UREConfigReader::get_rules()
{
	return _rbparams.rules;
}

bool UREConfigReader::get_attention_allocation() const
{
	return _rbparams.attention_alloc;
}

int UREConfigReader::get_maximum_iterations() const
{
	return _rbparams.max_iter;
}

HandleSeq UREConfigReader::fetch_rules(Handle rbs)
{
	// Retrieve rules
	Handle rule_var = _as.addNode(VARIABLE_NODE, "__URE_RULE__");
	Handle gl = _as.addLink(GET_LINK,
	                        // MemberLink
	                        //    VariableNode "__URE_RULE__"
	                        //    ConceptNode <RBS>
	                        _as.addLink(MEMBER_LINK, rule_var, rbs));
	Handle rule_names = satisfying_set(&_as, gl);

	// Remove the GetLink from the AtomSpace as it is no longer useful
	_as.removeAtom(gl);

	return LinkCast(rule_names)->getOutgoingSet();
}

Handle UREConfigReader::fetch_definition(Handle label)
{
	HandleSeq defines;
	label->getIncomingSetByType(back_inserter(defines), EQUIVALENCE_LINK);
	OC_ASSERT(defines.size() == 1, "There should be only one definition");
	// The following line is disabled till DefineLink is fully supported
	// return Handle(DefineLinkCast(defines.front())->get_definition());
	return LinkCast(defines.front())->getOutgoingAtom(1);
}

Handle UREConfigReader::fetch_execution_output(Handle schema, Handle input)
{
	// Retrieve rules
	Handle output_var = _as.addNode(VARIABLE_NODE, "__EXECUTION_OUTPUT_VAR__");
	Handle gl = _as.addLink(BIND_LINK,
	                        output_var,
	                        // ExecutionLink
	                        //    <schema>
	                        //    <input>
	                        //    output_var
	                        _as.addLink(EXECUTION_LINK,
	                                    schema,
	                                    input,
	                                    output_var),
	                        output_var);
	Handle outputs = bindlink(&_as, gl);

	// Remove the GetLink from the AtomSpace as it is no longer useful
	_as.removeAtom(gl);

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
	TruthValuePtr tv =
		_as.addLink(EVALUATION_LINK, pred, input)->getTruthValue();
	return tv->getMean() > 0.5;
}
