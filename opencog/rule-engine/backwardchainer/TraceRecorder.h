/*
 * TraceRecorder.h
 *
 * Copyright (C) 2017 OpenCog Foundation
 *
 * Authors: Nil Geisweiller
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
#ifndef TRACERECORDER_H_
#define TRACERECORDER_H_

#include <opencog/atomspace/AtomSpace.h>

#include "BIT.h"
#include "../Rule.h"

namespace opencog
{

class TraceRecorder
{
public:
	TraceRecorder(AtomSpace* tr_as);

	const std::string target_predicate_name = "URE:BC:target";
	const std::string andbit_predicate_name = "URE:BC:and-BIT";
	const std::string expand_andbit_predicate_name = "URE:BC:expand-and-BIT";
	const std::string proof_predicate_name = "URE:BC:proof";

	// Record that an atom is a target
	//
	// Evaluation (stv 1 1)
	//   Predicate "URE:BC:target"
	//   <target>
	void target(const Handle& target);

	// Record that an atom is an and-BIT
	//
	// Evaluation (stv 1 1)
	//   Predicate "URE:BC:and-BIT"
	//   <and-BIT>
	void andbit(const AndBIT& andbit);

	// Record and-BIT expansion to _trace_as
	//
	// ExecutionLink (stv 1 1)
	//   SchemaNode "URE:BC:expand-and-BIT"
	//   List
	//     <andbit_fcs>
	//     <bitleaf_body>
	//     <rule>
	//   <new_andbit>
	//
	// The andbit and its bitleaf are passed as Handles because by the
	// time this called they have gotten corrupted (but not their
	// handles).
	void expansion(const Handle& andbit_fcs, const Handle& bitleaf_body,
	               const Rule& rule, const AndBIT& new_andbit);

	// Record whether a certain and-BIT is a proof of a certain target result
	//
	// EvaluationLink <TV>
	//   PredicateNode "URE:BC:proof"
	//   List
	//     <andbit_fcs>
	//     <target_result> <TV>
	//
	// If the TV on the target has a greater than zero confidence it
	// is reported to the EvaluationLink, otherwise it is not
	// recorded.
	//
	// TODO: the TV on the evaluation link should be more carefully
	// thought. For instance maybe it was already proved to begin
	// with.
	void proof(const Handle& andbit_fcs, const Handle& target_result);

private:
	AtomSpace* _trace_as;

	// Add
	//
	// Execution <tv>
	//   Schema <schema_name>
	//   <input>
	//   <output>
	Handle add_execution(const std::string& schema_name,
	                     const Handle& input, const Handle& output,
	                     TruthValuePtr tv);

	// Add
	//
	// Execution <tv>
	//   Schema <schema_name>
	//   List
	//     <input1>
	//     <input2>
	//     <input3>
	//   <output>
	Handle add_execution(const std::string& schema_name,
	                     const Handle& input1,
	                     const Handle& input2,
	                     const Handle& input3,
	                     const Handle& output,
	                     TruthValuePtr tv);

	// Add
	//
	// Evaluation <tv>
	//   Predicate <predicate_name>
	//   <argument>
	Handle add_evaluation(const std::string& predicate_name,
	                      const Handle& argument,
	                      TruthValuePtr tv);

	// Add
	//
	// Evaluation <tv>
	//   Predicate <predicate_name>
	//   List
	//     <arg1>
	//     <arg2>
	Handle add_evaluation(const std::string& predicate_name,
	                      const Handle& arg1, const Handle& arg2,
	                      TruthValuePtr tv);
};


} // namespace opencog

#endif /* BACKWARDCHAINER_H_ */
