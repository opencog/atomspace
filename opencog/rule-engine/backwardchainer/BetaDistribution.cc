/*
 * ActionSelection.cc
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

#include "BetaDistribution.h"
#include "../URELogger.h"

#include <opencog/truthvalue/SimpleTruthValue.h>

namespace opencog {

BetaDistribution::BetaDistribution(const TruthValuePtr& tv,
                                   double p_alpha, double p_beta)
	// TODO should be replaced by tv->get_mode() once implemented
	: BetaDistribution(tv->get_mean() * tv->get_count(),
	                   tv->get_count(), p_alpha, p_beta) {}

BetaDistribution::BetaDistribution(double pos_count, double count,
                                   double p_alpha, double p_beta)
	: _beta_distribution(p_alpha + pos_count, p_beta + count - pos_count) {}

double BetaDistribution::alpha() const
{
	return _beta_distribution.alpha();
}

double BetaDistribution::beta() const
{
	return _beta_distribution.beta();
}

double BetaDistribution::mean() const
{
	return boost::math::mean(_beta_distribution);
}

double BetaDistribution::variance() const
{
	return boost::math::variance(_beta_distribution);
}

std::vector<double> BetaDistribution::cdf(int bins) const
{
	std::vector<double> cdf;
	for (int x_idx = 0; x_idx < bins; x_idx++) {
		double x = (x_idx + 1.0) / bins,
			r = boost::math::cdf(_beta_distribution, std::min(1.0, x));
		cdf.push_back(r);
	}
	return cdf;
}

double BetaDistribution::pd(double x) const
{
	return boost::math::pdf(_beta_distribution, x);
}

std::string BetaDistribution::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "alpha = " << alpha()
	   << ", beta = " << beta()
	   << ", mean = " << mean()
	   << ", variance = " << variance() << std::endl;
	return ss.str();
}

BetaDistribution mk_beta_distribution(const TruthValuePtr& tv) {
	return BetaDistribution(tv);
}

TruthValuePtr mk_stv(double mean, double variance,
                     double prior_alpha, double prior_beta)
{
	using boost::math::beta_distribution;
	double alpha = beta_distribution<double>::find_alpha(mean, variance),
		beta = beta_distribution<double>::find_beta(mean, variance);

	// Inferred from
	// alpha == prior_alpha + pos_count
	// beta == prior_beta + count - pos_count
	double count = alpha + beta - prior_alpha - prior_beta;
	count = std::max(0.1, count); // Hack to avoid non-sensical TV
	double confidence = count / (count + SimpleTruthValue::DEFAULT_K),
		mode = 1;               // default strength if confidence is null

	if (1 < alpha and 1 < beta)
		mode = boost::math::mode(beta_distribution<double>(alpha, beta));

	if (alpha < 1 and 1 <= beta)
		mode = 0;

	if (beta < 1 and 1 <= alpha)
		mode = 1;

	// This is mathematically wrong, but for now we don't try to have
	// a bimodal TV, rather a unimodal one with very low confidence.
	if (alpha < 1 and beta < 1)
		mode = mean;

	LAZY_URE_LOG_FINE << "mk_stv alpha alpha = " << alpha
	                  << ", beta = " << beta << ", count = " << count
	                  << ", confidence = " << confidence << ", mode = " << mode;

	// The strength is in fact the mode, this should be corrected once
	// TruthValue is reworked
	return SimpleTruthValue::createTV(mode, confidence);
}

std::string oc_to_string(const BetaDistribution& bd, const std::string& indent)
{
	return bd.to_string(indent);
}

} // ~namespace opencog
