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

using namespace opencog;

BetaDistribution::BetaDistribution(const TruthValuePtr& tv, double alpha, double beta)
	: BetaDistribution(tv->get_mean() * tv->get_count(), tv->get_count(), alpha, beta) {}

BetaDistribution::BetaDistribution(double pos_count, double count,
                                   double alpha, double beta)
	: _beta_distribution(alpha + pos_count, beta + count - pos_count) {}

double BetaDistribution::mean() const
{
	return boost::math::mean(_beta_distribution);
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
