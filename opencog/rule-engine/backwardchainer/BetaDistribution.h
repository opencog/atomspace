/*
 * BetaDistribution.h
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
#ifndef OPENCOG_BETADISTRIBUTION_H_
#define OPENCOG_BETADISTRIBUTION_H_

#include <boost/math/distributions/beta.hpp>

#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{

/**
 * Class containing methods to calculate distribution over actions
 * given the TruthValue that each action fulfills the objective of
 * interest.
 */
class BetaDistribution
{
public:
	/**
	 * Construct a BetaDistribution given a TV and a beta-distribution
	 * prior with parameters (alpha, beta).
	 */
	BetaDistribution(const TruthValuePtr& tv,
	                 double prior_alpha=1.0, double prior_beta=1.0);
	BetaDistribution(double pos_count, double count,
	                 double prior_alpha=1.0, double prior_beta=1.0);

	/**
	 * Return the mean of the beta distribution
	 */
	double mean() const;

	/**
	 * Return the variance of the beta distribution
	 */
	double variance() const;

	/**
	 * Calculate the cdf of a beta distribution, given the number of
	 * bins for discretization.
	 */
	std::vector<double> cdf(int bins) const;

	/**
	 * Calculate the probability density at x
	 */
	double pd(double x) const;

private:
	boost::math::beta_distribution<double> _beta_distribution;
};

// Helpers
BetaDistribution mk_beta_distribution(const TruthValuePtr& tv);
	TruthValuePtr mk_stv(double mean, double variance,
	                     double prior_alpha=1.0, double prior_beta=1.0);

} // namespace opencog

#endif /* OPENCOG_ACTIONSELECTION_H_ */
