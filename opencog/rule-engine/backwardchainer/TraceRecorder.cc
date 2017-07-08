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

void TraceRecorder::record_expansion(const Handle& andbit_fcs,
                                     const Handle& bitleaf_body,
                                     const Rule& rule,
                                     const AndBIT& new_andbit)
{
	if (_trace_as) {
		Handle schema = _trace_as->add_node(SCHEMA_NODE, "URE:BC:expand-and-BIT");
		Handle input = _trace_as->add_link(LIST_LINK,
		                                  andbit_fcs,
		                                  bitleaf_body,
		                                  rule.get_definition());
		Handle output = new_andbit.fcs;
		_trace_as->add_link(EXECUTION_LINK, schema, input, output);
	}
}

void TraceRecorder::record_proof(const Handle& andbit_fcs,
                                 const Handle& target_result)
{
	if (_trace_as) {
		TruthValuePtr tv = target_result->getTruthValue();
		if (0 < tv->getConfidence()) {
			Handle proof_pred = _trace_as->add_node(PREDICATE_NODE, "URE:BC:proof");
			Handle proof = _trace_as->add_link(PREDICATE_NODE, proof_pred,
			                                   _trace_as->add_link(LIST_LINK,
			                                                       andbit_fcs,
			                                                       target_result));
			proof->setTruthValue(tv);
		}
	}
}
