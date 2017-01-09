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
#include "BCLogger.h"

using namespace opencog;

BITNodeFitness::BITNodeFitness(FitnessType ft) : type(ft)
{
	switch(type) {
	case (MaximizeConfidence):
		function = [](const BITNode& bitnode) {
			return bitnode.body->getTruthValue()->getConfidence();
		};
		upper = 1;
		lower = 0;
		break;
	default:
		bc_logger().error() << "Not implemented";
	}
}

double BITNodeFitness::operator()(const BITNode& bitnode) const
{
	return function(bitnode);
};
