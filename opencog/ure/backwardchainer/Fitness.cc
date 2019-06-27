/*
 * Fitness.cc
 *
 * Copyright (C) 2016-2017 OpenCog Foundation
 * Author: Nil Geisweiller
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

#include "Fitness.h"
#include "BIT.h"
#include "../URELogger.h"

#include <opencog/util/algorithm.h>

using namespace opencog;

BITNodeFitness::BITNodeFitness(FitnessType ft) : type(ft)
{
	switch(type) {
	case (MaximizeConfidence):
		function = [](const BITNode& bitnode) {
			return bitnode.body->getTruthValue()->get_confidence();
		};
		lower = 0;
		upper = 1;
		break;
	default:
		ure_logger().error() << "Not implemented";
	}
}

double BITNodeFitness::operator()(const BITNode& bitnode) const
{
	return function(bitnode);
}

AndBITFitness::AndBITFitness(FitnessType ft, const std::set<ContentHash>& tr)
	: type(ft), _trace(tr)
{
	switch(type) {
	case (Uniform):
		function = [](const AndBIT&) { return 1.0; };
		lower = 1.0;
		upper = 1.0;
		break;
	case (Trace):
		function = [&](const AndBIT& andbit) {
			return is_in(andbit.fcs.value(), _trace) ? 1.0 : 0.0; };
		lower = 0.0;
		upper = 1.0;
		break;
	default:
		ure_logger().error() << "Not implemented";
	}
}

double AndBITFitness::operator()(const AndBIT& andbit) const
{
	return function(andbit);
}
