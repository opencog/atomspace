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

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atomspaceutils/AtomSpaceUtils.h>
#include <opencog/query/BindLinkAPI.h>

using namespace std;
using namespace opencog;

const std::string UREConfigReader::top_rbs_name = "URE";

// Parameters
const std::string UREConfigReader::attention_alloc_name = "URE:attention-allocation";
const std::string UREConfigReader::max_iter_name = "URE:maximum-iterations";
const std::string UREConfigReader::bc_complexity_penalty_name = "URE:BC:complexity-penalty";
const std::string UREConfigReader::bc_max_bit_size_name = "URE:BC:maximum-bit-size";

UREConfigReader::UREConfigReader(AtomSpace& as, const Handle& rbs) : _as(as)
{
	//////////////////////////
	// Common parameters    //
	//////////////////////////

	if (Handle::UNDEFINED == rbs)
		throw RuntimeException(TRACE_INFO,
			"UREConfigReader - invalid rulebase specified!");

	// Retrieve the rules (MemberLinks) and instantiate them
	for (const Handle& rule_name : fetch_rule_names(rbs))
		_common_params.rules.emplace(rule_name, rbs);

	// Fetch maximum number of iterations
	_common_params.max_iter = fetch_num_param(max_iter_name, rbs);

	// Fetch attention allocation parameter
	_common_params.attention_alloc = fetch_bool_param(attention_alloc_name, rbs);

	//////////////////////
	// FC parameters    //
	//////////////////////

	//////////////////////
	// BC parameters    //
	//////////////////////

	// Fetch BC complexity penalty parameter
	_bc_params.complexity_penalty = fetch_num_param(bc_complexity_penalty_name, rbs);

	// Fetch BC BIT maximum size parameter
	_bc_params.max_bit_size = fetch_num_param(bc_max_bit_size_name, rbs, -1);
}

const RuleSet& UREConfigReader::get_rules() const
{
	return _common_params.rules;
}

RuleSet& UREConfigReader::get_rules()
{
	return _common_params.rules;
}

bool UREConfigReader::get_attention_allocation() const
{
	return _common_params.attention_alloc;
}

int UREConfigReader::get_maximum_iterations() const
{
	return _common_params.max_iter;
}

double UREConfigReader::get_complexity_penalty() const
{
	return _bc_params.complexity_penalty;
}

double UREConfigReader::get_max_bit_size() const
{
	return _bc_params.max_bit_size;
}

void UREConfigReader::set_attention_allocation(bool aa)
{
	_common_params.attention_alloc = aa;
}

void UREConfigReader::set_maximum_iterations(int mi)
{
	_common_params.max_iter = mi;
}

void UREConfigReader::set_complexity_penalty(double cp)
{
	_bc_params.complexity_penalty = cp;
}

HandleSeq UREConfigReader::fetch_rule_names(const Handle& rbs)
{
	// Retrieve rules
	Handle rule_var = _as.add_node(VARIABLE_NODE, "__URE_RULE__"),
		rule_pat = _as.add_link(MEMBER_LINK, rule_var, rbs),
		gl = _as.add_link(GET_LINK, rule_pat),
		results = satisfying_set(&_as, gl);
	HandleSeq rule_names = results->getOutgoingSet();

	// Remove the GetLink pattern and other no longer useful atoms
	// from the AtomSpace
	extract_hypergraph(_as, gl);
	_as.extract_atom(results);

	return rule_names;
}

HandleSeq UREConfigReader::fetch_execution_outputs(const Handle& schema,
                                                   const Handle& input,
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
		results = satisfying_set(&_as, gl);
	HandleSeq outputs = results->getOutgoingSet();

	// Remove the GetLink pattern and other no longer useful atoms
	// from the AtomSpace
	extract_hypergraph(_as, gl);
	_as.extract_atom(results);

	return outputs;
}

double UREConfigReader::fetch_num_param(const string& schema_name,
                                        const Handle& input,
                                        double default_value)
{
	Handle param_schema = _as.add_node(SCHEMA_NODE, schema_name);
	HandleSeq outputs = fetch_execution_outputs(param_schema, input, NUMBER_NODE);
	{
		string input_name = input->get_name();
		Type input_type = input->get_type();
		string input_str =
			classserver().getTypeName(input_type) + " \"" + input_name + "\"";
		if (outputs.size() == 0) {
			logger().warn() << "Could not retrieve parameter " << schema_name
			                << " for rule-based system " << input_name
			                << ". Use default value " << default_value
			                << " instead.";
			return default_value;
		} else {
			OC_ASSERT(outputs.size() == 1,
		          "Could not retrieve parameter %s for rule-based system %s. "
		          "There should be only one output for\n"
		          "ExecutionLink\n"
		          "   SchemaNode \"%s\"\n"
		          "   %s\n"
		          "   <N>\n"
		          "instead there are %u",
		          schema_name.c_str(), input_name.c_str(),
		          schema_name.c_str(), input_str.c_str(), outputs.size());
			return NumberNodeCast(outputs.front())->get_value();
		}
	}
}

bool UREConfigReader::fetch_bool_param(const string& pred_name,
                                       const Handle& input)
{
	Handle pred = _as.add_node(PREDICATE_NODE, pred_name);
	TruthValuePtr tv =
		_as.add_link(EVALUATION_LINK, pred, input)->getTruthValue();
	return tv->get_mean() > 0.5;
}
