/*
 * UREConfig.h
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

#ifndef _OPENCOG_URE_CONFIG_H
#define _OPENCOG_URE_CONFIG_H

#include "Rule.h"

#include <opencog/atomspace/AtomSpace.h>

#include "URELogger.h"

namespace opencog {

/**
 * Read the URE configuration from the AtomSpace as described in
 * http://wiki.opencog.org/w/URE_Configuration_Format, and provide
 * parameter accessors for all rule-based systems.
 *
 * @todo: It doesn't support the hierarchical configuration structure
 * described in
 * http://wiki.opencog.org/w/URE_Configuration_Format#Rule-Based_System_Hierarchical_Structure,
 * instead it assumes all parameters are duplicated for all systems
 * and subsystems, for now.
 */
class UREConfig
{
public:
	/////////////
	// Ctor    //
	/////////////

	// rbs is a Handle pointing to a rule-based system is as
	UREConfig(AtomSpace& as, const Handle& rbs);

	///////////////
	// Accessors //
	///////////////

	// Common
	const RuleSet& get_rules() const;
	RuleSet& get_rules();
	bool get_attention_allocation() const;
	int get_maximum_iterations() const;
	// FC
	bool get_retry_sources() const;
	// BC
	double get_complexity_penalty() const;
	double get_max_bit_size() const;
	double get_mm_complexity_penalty() const;
	double get_mm_compressiveness() const;

	// Display
	std::string get_maximum_iterations_str() const; // "+inf" if negative

	///////////////////////////////////////////////////////////////////
	// Modifiers. WARNING: Those changes are not reflected in the    //
	// AtomSpace, only in the UREConfig object.                      //
	///////////////////////////////////////////////////////////////////

	// Common
	void set_attention_allocation(bool);
	void set_maximum_iterations(int);
	// FC
	void set_retry_sources(bool);
	// BC
	void set_complexity_penalty(double);
	void set_mm_complexity_penalty(double);
	void set_mm_compressiveness(double);

	//////////////////
	// Constants    //
	//////////////////

	// Name of the top rule base from which all rule-based systems
	// inherit. It should corresponds to a ConceptNode in the
	// AtomSpace.
	static const std::string top_rbs_name;

	// Name of the PredicateNode outputting whether attention
	// allocation is enabled or not
	static const std::string attention_alloc_name;

	// Name of the SchemaNode outputing the maximum iterations
	// parameter
	static const std::string max_iter_name;

	// Name of the PredicateNode outputting whether sources should be
	// retried after exhaustion
	static const std::string fc_retry_sources_name;

	// Name of the complexity penalty parameter for the Backward
	// Chainer
	static const std::string bc_complexity_penalty_name;

	// Name of the maximum number of and-BITs in the BIT parameter
	static const std::string bc_max_bit_size_name;

	// Name of the parameter of the Mixture Model controlling how
	// complexity affects model prior.
	static const std::string bc_mm_complexity_penalty_name;

	// Name of the parameter of the Mixture Model controlling how
	// much unexplained data are compressed
	static const std::string bc_mm_compressiveness_name;

private:
	AtomSpace& _as;

	// Parameter common to the forward and backward chainer.
	struct CommonParameters {
		RuleSet rules;
		bool attention_alloc;
		int max_iter;           // If negative then disabled
	};
	CommonParameters _common_params;

	// Parameter specific to the forward chainer.
	struct FCParameters {
		// Retry all sources even if they have all been tried
		bool retry_sources;
	};
	FCParameters _fc_params;

	// Parameter specific to the backward chainer.
	struct BCParameters {
		// This parameter biases select_expansion_andbit towards
		// simpler FCS. Range from 0 to +inf. 0 means there is no
		// complexity penalty, the greater value the greater the
		// complexity penalty.
		double complexity_penalty;

		// This put an upper boundary on the maximum number of
		// and-BITs the BIT can hold. Negative means unlimited.
		int max_bit_size;

		// Parameter of the Mixture Model controlling how complexity
		// affects model prior. The prior exponentially decreases
		// w.r.t. to the complexity. Specifically
		//
		// prior = exp(-mm_complexity_penalty * complexity)
		double mm_complexity_penalty;

		// Parameter of the Mixture Model controlling how much
		// unexplained data are compressed. The compressed unexplained
		// data are added to the model complexity.
		double mm_compressiveness;
	};
	BCParameters _bc_params;

	// Fetch from the AtomSpace all rules of a given rube-based
	// system. Specifically fetches patterns
	//
	// MemberLink <TV>
	//    <rule name>
	//    <rbs>
	HandleSeq fetch_rule_names(const Handle& rbs);

	// Fetch from the atomspace all parameters common to the forward
	// and backward chainer
	void fetch_common_parameters(const Handle& rbs);

	// Fetch from the atomspace all forward chainer parameters
	void fetch_fc_parameters(const Handle& rbs);

	// Fetch from the atomspace all backward chainer parameters
	void fetch_bc_parameters(const Handle& rbs);

	// Given <schema>, an <input> and optionally an output <type> (or
	// subtype), return the <output>s in
	//
	// ExecutionLink
	//    <schema>
	//    <input>
	//    <output>
	//
	// inheriting <type>.
	//
	// The type (or subtype) can be used to avoid fetching patterns
	// (if <type> is choosen not to have VARIABLE_NODE inherit from
	// it).
	HandleSeq fetch_execution_outputs(const Handle& schema,
	                                  const Handle& input,
	                                  Type type = ATOM);

	// Similar to above but takes instead the schema name instead of
	// the schema Handle, assumes that output value is a NumberNode,
	// and return directly that number.
	//
	// That is, given <schema_name> and <input> in
	//
	// ExecutionLink
	//    SchemaNode <schema_name>
	//    <input>
	//    NumberNode <num>
	//
	// Return the number associated to <num> or default_value in case
	// no such ExecutionLink exists.
	double fetch_num_param(const std::string& schema_name,
	                       const Handle& input,
	                       double default_value = 0.0);

	// Given <pred_name> and <input> in
	//
	// EvaluationLink TV
	//    PredicateNode <pred_name>
	//    <input>
	//
	// Return TV.mean > 0.5 or default_value in case no such
	// EvaluationLink exists.
	bool fetch_bool_param(const std::string& pred_name, const Handle& input, bool default_value = false);

	// Log debug message about the value of parameter, fetched or default
	template<typename T>
	void log_param_value(const Handle& rbs_input,
	                     const std::string& param_name,
	                     const T& value, bool is_default=false) const
	{
		ure_logger().debug() << "Rule-base " << rbs_input->get_name()
		                     << ", set parameter " << param_name
		                     << " to " << (is_default ? "(default) " : "") << value;
	}
};

} // ~namespace opencog


#endif /* _OPENCOG_URE_CONFIG_H_ */
