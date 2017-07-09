/*
 * TraceRecorder.cc
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

#include "TraceRecorder.h"

using namespace opencog;

TraceRecorder::TraceRecorder(AtomSpace* tr_as) : _trace_as(tr_as) {}

void TraceRecorder::target(const Handle& target)
{
	add_evaluation(target_predicate_name, target, TruthValue::TRUE_TV());
}

void TraceRecorder::andbit(const AndBIT& andbit)
{
	add_evaluation(andbit_predicate_name, andbit.fcs, TruthValue::TRUE_TV());
}

void TraceRecorder::expansion(const Handle& andbit_fcs, const Handle& bitleaf_body,
                              const Rule& rule, const AndBIT& new_andbit)
{
	add_execution(expand_andbit_predicate_name,
	              andbit_fcs, bitleaf_body, rule.get_definition(),
	              new_andbit.fcs, TruthValue::TRUE_TV());
}

void TraceRecorder::proof(const Handle& andbit_fcs, const Handle& target_result)
{
	add_evaluation(proof_predicate_name,
	               andbit_fcs, target_result,
	               target_result->getTruthValue());
}

Handle TraceRecorder::add_execution(const std::string& schema_name,
                                    const Handle& input, const Handle& output,
                                    TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle schema = _trace_as->add_node(SCHEMA_NODE, schema_name);
	Handle execution = _trace_as->add_link(EXECUTION_LINK, schema, input, output);
	execution->setTruthValue(tv);
	return execution;
}

Handle TraceRecorder::add_execution(const std::string& schema_name,
                                    const Handle& input1,
                                    const Handle& input2,
                                    const Handle& input3,
                                    const Handle& output,
                                    TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle inputs = _trace_as->add_link(LIST_LINK, input1, input2, input3);
	return add_execution(schema_name, inputs, output, tv);
}

Handle TraceRecorder::add_evaluation(const std::string& predicate_name,
                                     const Handle& argument,
                                     TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;
	
	Handle predicate = _trace_as->add_node(PREDICATE_NODE, predicate_name);
	Handle evaluation = _trace_as->add_link(EVALUATION_LINK, predicate, argument);
	evaluation->setTruthValue(tv);
	return evaluation;
}

Handle TraceRecorder::add_evaluation(const std::string& predicate_name,
                                     const Handle& arg1, const Handle& arg2,
                                     TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle arguments = _trace_as->add_link(LIST_LINK, arg1, arg2);
	return add_evaluation(predicate_name, arguments, tv);
}
