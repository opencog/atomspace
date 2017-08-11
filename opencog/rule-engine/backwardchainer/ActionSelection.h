/*
 * ActionSelection.h
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
#ifndef OPENCOG_ACTIONSELECTION_H_
#define OPENCOG_ACTIONSELECTION_H_

#include <boost/math/distributions/beta.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{

//! a map from handles to truth values
typedef std::map<Handle, TruthValuePtr> HandleTVMap;

// Beta Distribution
typedef boost::math::beta_distribution<double> BetaDistribution;

/**
 * Class containing methods to calculate distribution over actions
 * given the TruthValue that each action fulfills the objective of
 * interest.
 */
class ActionSelection
{
public:
	const HandleTVMap& action2tv;

	/**
	 * CTor
	 */
	ActionSelection(const HandleTVMap& action2tv);

	/**
	 * Return the action distribution, a probability for each action
	 * to be used as sampling distribution for picking the next
	 * action. The distribution attempts to reflect the optimal
	 * balance between exploration and exploitation (Thomson
	 * sampling).
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
	HandleCounter distribution();

	/**
	 * Perform random action selection according to the action
	 * distribution.
	 */
	Handle operator()();

private:
	/**
	 * Return the beta distribution associated to a TV
	 */
	static BetaDistribution tv2beta(const TruthValuePtr& tv);

	/**
	 * Calculate the cdf of a beta distribution, given the number of
	 * bins for discretization.
	 */
	static std::vector<double> beta2cdf(const BetaDistribution& beta, int bins);
};

} // namespace opencog

#endif /* OPENCOG_ACTIONSELECTION_H_ */
