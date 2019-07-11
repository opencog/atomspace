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
#ifndef _OPENCOG_TRACERECORDER_H_
#define _OPENCOG_TRACERECORDER_H_

#include <opencog/atomspace/AtomSpace.h>

#include "BIT.h"
#include "../Rule.h"

namespace opencog
{

class TraceRecorder
{
public:
	static const std::string target_predicate_name;
	static const std::string andbit_predicate_name;
	static const std::string expand_andbit_schema_name;
	static const std::string proof_predicate_name;

	TraceRecorder(AtomSpace* tr_as);

	// Return the traces of fcs leading to the recorded proofs
	HandleSeqSet traces();

	// Return traces leading to the given FCS. A trace is a sequence
	// of Handles, each representing a FCS and chained by
	// ExecutionLinks as recorded in the expansion method.
	HandleSeqSet traces(const Handle& fcs);

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
	//   PredicateNode "URE:BC:proof-of"
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

	Handle _target_predicate, _andbit_predicate, _expand_andbit_schema,
		_proof_predicate;

	// Wrap a DontExecLink around h
	//
	// DontExecLink
	//   h
	Handle dont_exec(const Handle& h);

	// Add
	//
	// Execution <tv>
	//   <schema>
	//   <input>
	//   <output>
	Handle add_execution(const Handle& schema,
	                     const Handle& input, const Handle& output,
	                     TruthValuePtr tv);

	// Add
	//
	// Execution <tv>
	//   <schema>
	//   List
	//     <input1>
	//     <input2>
	//     <input3>
	//   <output>
	Handle add_execution(const Handle& schema,
	                     const Handle& input1,
	                     const Handle& input2,
	                     const Handle& input3,
	                     const Handle& output,
	                     TruthValuePtr tv);

	// Add
	//
	// Evaluation <tv>
	//   <predicate>
	//   <argument>
	Handle add_evaluation(const Handle& predicate,
	                      const Handle& argument,
	                      TruthValuePtr tv);

	// Add
	//
	// Evaluation <tv>
	//   <predicate>
	//   List
	//     <arg1>
	//     <arg2>
	Handle add_evaluation(const Handle& predicate,
	                      const Handle& arg1, const Handle& arg2,
	                      TruthValuePtr tv);

	// Given a fcs, return all fcs that expands to this fcs target.
	HandleSet get_expansion_sources(const Handle& fcs_target);

	// Return the set of fcs corresponding to proofs
	HandleSet get_fcs_proofs();
};


} // namespace opencog

#endif /* _OPENCOG_TRACERECORDER_H_ */
