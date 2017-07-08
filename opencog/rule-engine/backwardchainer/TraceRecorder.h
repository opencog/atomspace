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

	// Record and-BIT expansion to _trace_as
	//
	// ExecutionLink (stv 1 1)
	//   SchemaNode "URE:BC:expand-and-BIT"
	//   List
	//     <andbit_fcs>
	//     <bitleaf_body>
	//     <rule>
	//   <new_andbit>
	void record_expansion(const Handle& andbit_fcs, const Handle& bitleaf_body,
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
	void record_proof(const Handle& andbit_fcs, const Handle& target_result);

private:
	AtomSpace* _trace_as;
};


} // namespace opencog

#endif /* BACKWARDCHAINER_H_ */
