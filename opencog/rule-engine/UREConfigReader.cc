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
#include <opencog/atomspaceutils/AtomSpaceUtils.h>

using namespace opencog;

const std::string UREConfigReader::top_rbs_name = "URE";
const std::string UREConfigReader::attention_alloc_name = "URE:attention-allocation";
const std::string UREConfigReader::max_iter_name = "URE:maximum-iterations";

UREConfigReader::UREConfigReader(AtomSpace& as, Handle rbs) : _as(as)
{
	if (Handle::UNDEFINED == rbs)
		throw RuntimeException(TRACE_INFO,
			"UREConfigReader - invalid rulebase specified!");

	// Retrieve the rules (MemberLinks) and instantiate them
	for (Handle rule_name : fetch_rule_names(rbs))
		_rbparams.rules.emplace_back(rule_name, rbs);

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

const Rule& UREConfigReader::get_rule(const Handle& hblink)
{
    if (hblink->getType() != BIND_LINK) {
        throw InvalidParamException(TRACE_INFO,
                                    "UREConfigReader - Expected a BindLink.");
    }

    return _rbparams.get_rule(hblink);
}

bool UREConfigReader::get_attention_allocation() const
{
	return _rbparams.attention_alloc;
}

int UREConfigReader::get_maximum_iterations() const
{
	return _rbparams.max_iter;
}

void UREConfigReader::set_attention_allocation(bool aa)
{
	_rbparams.attention_alloc = aa;
}

void UREConfigReader::set_maximum_iterations(int mi)
{
	_rbparams.max_iter = mi;
}

HandleSeq UREConfigReader::fetch_rule_names(Handle rbs)
{
	// Retrieve rules
	Handle rule_var = _as.add_node(VARIABLE_NODE, "__URE_RULE__");
	Handle rule_pat = _as.add_link(MEMBER_LINK, rule_var, rbs);
	Handle gl = _as.add_link(GET_LINK, rule_pat);
	Handle rule_names = satisfying_set(&_as, gl);

	// Remove the GetLink pattern from the AtomSpace as it is no
	// longer useful
	remove_hypergraph(_as, gl);

	return LinkCast(rule_names)->getOutgoingSet();
}

HandleSeq UREConfigReader::fetch_execution_outputs(Handle schema,
                                                   Handle input,
                                                   Type type)
{
	// Retrieve rules
	Handle var_node = _as.add_node(VARIABLE_NODE, "__EXECUTION_OUTPUT_VAR__"),
		type_node = _as.add_node(TYPE_NODE, classserver().getTypeName(type)),
		typed_var = _as.add_link(TYPED_VARIABLE_LINK, var_node, type_node),
		gl = _as.add_link(GET_LINK,
		                  // TypedVariableLink
		                  //    var_node
		                  //    type_node
		                  typed_var,
		                  // ExecutionLink
		                  //    <schema>
		                  //    <input>
		                  //    var_node
		                  _as.add_link(EXECUTION_LINK,
		                               schema,
		                               input,
		                               var_node)),
		outputs = satisfying_set(&_as, gl);

	// Remove the GetLink pattern from the AtomSpace as it is no
	// longer useful
	remove_hypergraph(_as, gl);

	return LinkCast(outputs)->getOutgoingSet();
}

double UREConfigReader::fetch_num_param(const string& schema_name, Handle input)
{
	Handle param_schema = _as.add_node(SCHEMA_NODE, schema_name);
	HandleSeq outputs = fetch_execution_outputs(param_schema, input, NUMBER_NODE);
	{
		string input_name = NodeCast(input)->getName();
		Type input_type = input->getType();
		string input_str =
			classserver().getTypeName(input_type) + " \"" + input_name + "\"";
		OC_ASSERT(outputs.size() == 1,
		          "Could not retrieve parameter %s for rule-based system %s. "
		          "There should be one and only one output for\n"
		          "ExecutionLink\n"
		          "   SchemaNode \"%s\"\n"
		          "   %s\n"
		          "   <N>\n"
		          "instead there are %u",
		          schema_name.c_str(), input_name.c_str(),
		          schema_name.c_str(), input_str.c_str(), outputs.size());
	}
	return NumberNodeCast(outputs.front())->get_value();
}

bool UREConfigReader::fetch_bool_param(const string& pred_name, Handle input)
{
	Handle pred = _as.add_node(PREDICATE_NODE, pred_name);
	TruthValuePtr tv =
		_as.add_link(EVALUATION_LINK, pred, input)->getTruthValue();
	return tv->getMean() > 0.5;
}
