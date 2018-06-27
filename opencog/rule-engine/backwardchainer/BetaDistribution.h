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
#ifndef _OPENCOG_BETADISTRIBUTION_H_
#define _OPENCOG_BETADISTRIBUTION_H_

#include <boost/math/distributions/beta.hpp>

#include <opencog/util/empty_string.h>
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
	 * Return the alpha parameter of the distribution
	 */
	double alpha() const;

	/**
	 * Return the beta of the beta parameter of the distribution
	 */
	double beta() const;

	/**
	 * Return the mean of the distribution
	 */
	double mean() const;

	/**
	 * Return the variance of the distribution
	 */
	double variance() const;

	/**
	 * Generate a vector of the cdf of regularly spaced right-end
	 * points, specifically
	 *
	 * [beta_distribution_cdf(1/bins),
	 *  beta_distribution_cdf(2/bins),
	 *  ...
	 *  beta_distribution_cdf(1)]
	 *
	 * The cdf at the origin is ignored because it is always 0. The
	 * last one is always 1 but is included for completeness.
	 */
	std::vector<double> cdf(int bins) const;

	/**
	 * Calculate the probability density at x
	 */
	double pd(double x) const;

	std::string to_string(const std::string& indent) const;

private:
	boost::math::beta_distribution<double> _beta_distribution;
};

// Helpers
BetaDistribution mk_beta_distribution(const TruthValuePtr& tv);

TruthValuePtr mk_stv(double mean, double variance,
                     double prior_alpha=1.0, double prior_beta=1.0);

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const BetaDistribution& bd,
                         const std::string& indent=empty_string);

} // namespace opencog

#endif /* _OPENCOG_BETADISTRIBUTION_H_ */
