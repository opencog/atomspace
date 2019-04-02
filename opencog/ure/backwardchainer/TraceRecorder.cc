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
#include <opencog/util/algorithm.h>

using namespace opencog;

const std::string TraceRecorder::target_predicate_name = "URE:BC:target";
const std::string TraceRecorder::andbit_predicate_name = "URE:BC:and-BIT";
const std::string TraceRecorder::expand_andbit_schema_name = "URE:BC:expand-and-BIT";
const std::string TraceRecorder::proof_predicate_name = "URE:BC:proof-of";

TraceRecorder::TraceRecorder(AtomSpace* tr_as) : _trace_as(tr_as) {
	if (_trace_as) {
		_target_predicate =
			_trace_as->add_node(PREDICATE_NODE, target_predicate_name);
		_andbit_predicate =
			_trace_as->add_node(PREDICATE_NODE, andbit_predicate_name);
		_expand_andbit_schema =
			_trace_as->add_node(SCHEMA_NODE, expand_andbit_schema_name);
		_proof_predicate =
			_trace_as->add_node(PREDICATE_NODE, proof_predicate_name);
	}
}

HandleSeqSet TraceRecorder::traces()
{
	HandleSeqSet trs;
	for (const Handle& fcs_proof : get_fcs_proofs())
		set_union_modify(trs, traces(fcs_proof));
	return trs;
}

HandleSeqSet TraceRecorder::traces(const Handle& fcs)
{
	HandleSet expansion_sources(get_expansion_sources(fcs));

	// Unwrap DontExecLink around the fcs
	Handle exec_fcs = fcs->getOutgoingAtom(0);

	// Base case
	if (expansion_sources.empty())
		return HandleSeqSet{{exec_fcs}};

	// Recursive case
	HandleSeqSet trs;
	for (const Handle& h : expansion_sources) {
		for (HandleSeq hs : traces(h)) {
			hs.push_back(exec_fcs);
			trs.insert(hs);
		}
	}
	return trs;
}

void TraceRecorder::target(const Handle& target)
{
	add_evaluation(_target_predicate, target, TruthValue::TRUE_TV());
}

void TraceRecorder::andbit(const AndBIT& andbit)
{
	add_evaluation(_andbit_predicate,
	               dont_exec(andbit.fcs),
	               TruthValue::TRUE_TV());
}

void TraceRecorder::expansion(const Handle& andbit_fcs, const Handle& bitleaf_body,
                              const Rule& rule, const AndBIT& new_andbit)
{
	add_execution(_expand_andbit_schema,
	              dont_exec(andbit_fcs), bitleaf_body,
	              dont_exec(rule.get_alias()),
	              dont_exec(new_andbit.fcs), TruthValue::TRUE_TV());
}

void TraceRecorder::proof(const Handle& andbit_fcs, const Handle& target_result)
{
	add_evaluation(_proof_predicate,
	               dont_exec(andbit_fcs), target_result,
	               target_result->getTruthValue());
}

Handle TraceRecorder::dont_exec(const Handle& h)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	return _trace_as->add_link(DONT_EXEC_LINK, h);
}

Handle TraceRecorder::add_execution(const Handle& schema,
                                    const Handle& input, const Handle& output,
                                    TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle execution = _trace_as->add_link(EXECUTION_LINK, schema, input, output);
	execution->setTruthValue(tv);
	return execution;
}

Handle TraceRecorder::add_execution(const Handle& schema,
                                    const Handle& input1,
                                    const Handle& input2,
                                    const Handle& input3,
                                    const Handle& output,
                                    TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle inputs = _trace_as->add_link(LIST_LINK, input1, input2, input3);
	return add_execution(schema, inputs, output, tv);
}

Handle TraceRecorder::add_evaluation(const Handle& predicate,
                                     const Handle& argument,
                                     TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;
	
	Handle evaluation = _trace_as->add_link(EVALUATION_LINK, predicate, argument);
	evaluation->setTruthValue(tv);
	return evaluation;
}

Handle TraceRecorder::add_evaluation(const Handle& predicate,
                                     const Handle& arg1, const Handle& arg2,
                                     TruthValuePtr tv)
{
	if (not _trace_as)
		return Handle::UNDEFINED;

	Handle arguments = _trace_as->add_link(LIST_LINK, arg1, arg2);
	return add_evaluation(predicate, arguments, tv);
}

HandleSet TraceRecorder::get_expansion_sources(const Handle& fcs_target)
{
	HandleSet sources;
	for (auto& exec_link : fcs_target->getIncomingSetByType(EXECUTION_LINK))
		if (exec_link->getOutgoingAtom(0) == _expand_andbit_schema)
			sources.insert(exec_link->getOutgoingAtom(1)->getOutgoingAtom(0));
	return sources;
}

HandleSet TraceRecorder::get_fcs_proofs()
{
	HandleSet fcs_proofs;
	for (auto& eval_link : _proof_predicate->getIncomingSetByType(EVALUATION_LINK))
		fcs_proofs.insert(eval_link->getOutgoingAtom(1)->getOutgoingAtom(0));
	return fcs_proofs;
}
