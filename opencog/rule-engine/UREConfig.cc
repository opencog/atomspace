/*
 * UREConfig.cc
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

#include "UREConfig.h"

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atomspaceutils/AtomSpaceUtils.h>
#include <opencog/query/BindLinkAPI.h>

using namespace std;
using namespace opencog;

const std::string UREConfig::top_rbs_name = "URE";

// Parameters
const std::string UREConfig::attention_alloc_name = "URE:attention-allocation";
const std::string UREConfig::max_iter_name = "URE:maximum-iterations";
const std::string UREConfig::fc_retry_sources_name = "URE:FC:retry-sources";
const std::string UREConfig::bc_complexity_penalty_name = "URE:BC:complexity-penalty";
const std::string UREConfig::bc_max_bit_size_name = "URE:BC:maximum-bit-size";
const std::string UREConfig::bc_mm_complexity_penalty_name = "URE:BC:MM:complexity-penalty";
const std::string UREConfig::bc_mm_compressiveness_name = "URE:BC:MM:compressiveness";

UREConfig::UREConfig(AtomSpace& as, const Handle& rbs) : _as(as)
{
	if (Handle::UNDEFINED == rbs)
		throw RuntimeException(TRACE_INFO,
			"UREConfig - invalid rulebase specified!");

	fetch_common_parameters(rbs);
	fetch_fc_parameters(rbs);
	fetch_bc_parameters(rbs);
}

const RuleSet& UREConfig::get_rules() const
{
	return _common_params.rules;
}

RuleSet& UREConfig::get_rules()
{
	return _common_params.rules;
}

bool UREConfig::get_attention_allocation() const
{
	return _common_params.attention_alloc;
}

int UREConfig::get_maximum_iterations() const
{
	return _common_params.max_iter;
}

bool UREConfig::get_retry_sources() const
{
	return _fc_params.retry_sources;
}

double UREConfig::get_complexity_penalty() const
{
	return _bc_params.complexity_penalty;
}

double UREConfig::get_max_bit_size() const
{
	return _bc_params.max_bit_size;
}

double UREConfig::get_mm_complexity_penalty() const
{
	return _bc_params.mm_complexity_penalty;
}

double UREConfig::get_mm_compressiveness() const
{
	return _bc_params.mm_compressiveness;
}

std::string UREConfig::get_maximum_iterations_str() const
{
	if (_common_params.max_iter < 0)
		return "+inf";
	return std::to_string(_common_params.max_iter);
}

void UREConfig::set_attention_allocation(bool aa)
{
	_common_params.attention_alloc = aa;
}

void UREConfig::set_maximum_iterations(int mi)
{
	_common_params.max_iter = mi;
}

void UREConfig::set_retry_sources(bool rs)
{
	_fc_params.retry_sources = rs;
}

void UREConfig::set_complexity_penalty(double cp)
{
	_bc_params.complexity_penalty = cp;
}

void UREConfig::set_mm_complexity_penalty(double mm_cp)
{
	_bc_params.mm_complexity_penalty = mm_cp;
}

void UREConfig::set_mm_compressiveness(double mm_cpr)
{
	_bc_params.mm_complexity_penalty = mm_cpr;
}

HandleSeq UREConfig::fetch_rule_names(const Handle& rbs)
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

void UREConfig::fetch_common_parameters(const Handle& rbs)
{
	// Retrieve the rules (MemberLinks) and instantiate them
	for (const Handle& rule_name : fetch_rule_names(rbs))
    {
        OC_ASSERT((rule_name->get_type() == DEFINED_SCHEMA_NODE),
              "The rule: \n%s \n is not a DefinedSchemaNode."
              "A rule needs an alias and to be defined a DefinedSchemaNode.\n"
              "Please check rules in /atomspace/examples/rule-engine for example.\n\n",
              rule_name->to_short_string().c_str());

		_common_params.rules.emplace(rule_name, rbs);
    }

	// Fetch maximum number of iterations
	_common_params.max_iter = fetch_num_param(max_iter_name, rbs, -1);

	// Fetch attention allocation parameter
	_common_params.attention_alloc = fetch_bool_param(attention_alloc_name, rbs);
}

void UREConfig::fetch_fc_parameters(const Handle& rbs)
{
	// Fetch retry sources parameter
	_fc_params.retry_sources =
		fetch_bool_param(fc_retry_sources_name, rbs, true);
}

void UREConfig::fetch_bc_parameters(const Handle& rbs)
{
	// Fetch BC complexity penalty parameter
	_bc_params.complexity_penalty =
		fetch_num_param(bc_complexity_penalty_name, rbs);

	// Fetch BC BIT maximum size parameter
	_bc_params.max_bit_size = fetch_num_param(bc_max_bit_size_name, rbs, -1);

	// Fetch BC Mixture Model complexity penalty parameter
	_bc_params.mm_complexity_penalty =
		fetch_num_param(bc_mm_complexity_penalty_name, rbs, 0);

	// Fetch BC Mixture Model complexity penalty parameter
	_bc_params.mm_compressiveness =
		fetch_num_param(bc_mm_compressiveness_name, rbs, 1);
}

HandleSeq UREConfig::fetch_execution_outputs(const Handle& schema,
                                             const Handle& input,
                                             Type type)
{
	// Retrieve rules
	Handle var_node = _as.add_node(VARIABLE_NODE, "__EXECUTION_OUTPUT_VAR__"),
		type_node = _as.add_node(TYPE_NODE, nameserver().getTypeName(type)),
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

double UREConfig::fetch_num_param(const string& schema_name,
                                  const Handle& input,
                                  double default_value)
{
	Handle param_schema = _as.add_node(SCHEMA_NODE, schema_name);
	HandleSeq outputs = fetch_execution_outputs(param_schema, input, NUMBER_NODE);

	if (outputs.size() == 0) {
		log_param_value(input, schema_name, default_value, true);
		return default_value;
	}

	string input_name = input->get_name(),
		input_type_str = nameserver().getTypeName(input->get_type()),
		input_str = input_type_str + " \"" + input_name + "\"";
	OC_ASSERT(outputs.size() == 1,
	          "Could not retrieve parameter %s for rule-based system %s. "
	          "There should be only one output for\n"
	          "ExecutionLink\n"
	          "   SchemaNode \"%s\"\n"
	          "   %s\n"
	          "   <value>\n"
	          "instead there are %u",
	          schema_name.c_str(), input_name.c_str(),
	          schema_name.c_str(), input_str.c_str(), outputs.size());

	double value = NumberNodeCast(outputs.front())->get_value();
	log_param_value(input, schema_name, value);
	return value;
}

bool UREConfig::fetch_bool_param(const string& pred_name,
                                 const Handle& input,
                                 bool default_value)
{
	string input_name = input->get_name();
	Handle pred = _as.get_node(PREDICATE_NODE, pred_name);
	if (pred) {
		Handle eval = _as.get_link(EVALUATION_LINK, pred, input);
		if (eval) {
			bool value = eval->getTruthValue()->get_mean() > 0.5;
			log_param_value(input, pred_name, value);
			return value;
		}
	}

	log_param_value(input, pred_name, default_value, true);
	return default_value;
}
