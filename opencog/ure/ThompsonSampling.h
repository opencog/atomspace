/*
 * ThompsonSampling.h
 *
 * Copyright (C) 2018 SingularityNET Foundation
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
#ifndef _OPENCOG_THOMPSON_SAMPLING_H_
#define _OPENCOG_THOMPSON_SAMPLING_H_

#include <opencog/util/empty_string.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

#include "BetaDistribution.h"

namespace opencog
{

/**
 * Class containing methods to calculate distribution over TV indices
 * given the TruthValue that each index corresponds to an action that
 * fulfills the objective of interest. It may also directly sample the
 * index (see operator()).
 */
class ThompsonSampling
{
public:
	/**
	 * CTor
	 *
	 * @param tvs Sequence of TruthValues denoting the probability that the
	 *            corresponding index is associated with fulfilling the objective.
	 *
	 * @paran bins Number of bins to discretize the second order
	 *             distributions associated to each TV.
	 */
	ThompsonSampling(const std::vector<TruthValuePtr>& tvs, unsigned bins=100);

	/**
	 * Return the index distribution, a probability for each index to
	 * be used as sampling distribution. The distribution attempts to
	 * reflect the optimal balance between exploration and exploitation
	 * (Thompson sampling).
	 *
	 * Pi = I_0^1 pdfi(x) Prod_j!=i cdfj(x) dx / nt
	 *
	 * where `Prod_j!=i fi` is the product of all fi with j from 1 to
	 * n, except i, nt is a normalizing factor and Pi is the
	 * probability that action i is the best.
	 *
	 * See Section Inference Rule Selection in the README.md of the
	 * pln inference-control-learning for more explanations.
	 */
	std::vector<double> distribution();

	/**
	 * Perform random action selection according to the action
	 * distribution.
	 *
	 * TODO: for now it builds the entire selection distribution, then
	 * select the index accordingly. This could be greatly optimized by
	 * sampling on the fly without building it.
	 */
	size_t operator()();

	std::string to_string(const std::string& indent=empty_string) const;

private:
	/**
	 * Helper for distribution(). Given a vector of cdf vector, one
	 * cdf vector per action, calculate the unnormalized Pi (see the
	 * comment of distribution()) for action i.
	 */
	double Pi(size_t i, const std::vector<std::vector<double>>& cdfs) const;

	// Sequence of TruthValues denoting the probability that the
	// corresponding index is associated with fulfilling the objective
	const TruthValueSeq& _tvs;

	// Number of bins used for discretization
	unsigned _bins;
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string	oc_to_string(const ThompsonSampling& asel,
                         const std::string& indent=empty_string);

} // namespace opencog

#endif /* _OPENCOG_ACTIONSELECTION_H_ */
