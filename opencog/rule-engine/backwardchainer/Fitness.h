/*
 * Fitness.h
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

#ifndef _OPENCOG_URE_FITNESS_H
#define _OPENCOG_URE_FITNESS_H

#include <functional>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

class BITNode;
class AndBIT;

/**
 * BIT-node fitness type.
 */
class BITNodeFitness
{
public:
	enum FitnessType {
		MaximizeConfidence
	};

	BITNodeFitness(FitnessType ft=MaximizeConfidence);

	// Fitness type
	const FitnessType type;

	// Fitness attributes
	std::function<double(const BITNode&)> function;
	double lower;       // Co-domain lower bound
	double upper;       // Co-domain upper bound

	// Evaluate the fitness of a given BIT-node.
	double operator()(const BITNode& bitnode) const;
};

/**
 * And-BIT fitness base class.
 */
class AndBITFitness
{
public:
	enum FitnessType {
		// Return 1.0 no matter what
		Uniform,

		// Given a trace, return 1.0 if the AndBIT FCS is in the
		// trace, 0.0 otherwise. This is handy for running inferences
		// without the computational cost of searching the trace.
		Trace
	};

	// TODO: we may want to move the arguments in its own class if it
	// groses bigger.
	AndBITFitness(FitnessType ft=Uniform,
	              const std::set<ContentHash>& tr=std::set<ContentHash>());

	// Fitness type
	const FitnessType type;

	// Fitness attributes
	std::function<double(const AndBIT&)> function;
	double lower;       // Co-domain lower bound
	double upper;       // Co-domain upper bound

	// Fitness evaluation function
	double operator()(const AndBIT& andbit) const;

private:
	// TODO: replace by class dedicated to hold the parameters
	std::set<ContentHash> _trace;
};

} // ~namespace opencog

#endif // _OPENCOG_URE_FITNESS_H
