/*
 * MixtureModel.cc
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

#include "ActionSelection.h"

#include <opencog/util/Logger.h>

using namespace opencog;

ActionSelection::ActionSelection(const HandleTVMap& a2tv)
	: action2tv(a2tv) {}

HandleCounter ActionSelection::distribution()
{
	HandleCounter action2prob;
	const int bins = 100;             // Number of bins used for discretization
	const double step = 1.0 / bins;   // Step size

	// Generate all beta distributions of all TVs
	std::vector<BetaDistribution> betas;
	for (const auto& atv : action2tv)
		betas.push_back(tv2beta(atv.second));

	// Calculate cdfs for all TV
	std::vector<std::vector<double>> cdfs;
	for (const auto& beta : betas)
		cdfs.push_back(beta2cdf(beta, bins));

	// Calculate Pi for all actions
	// Pi = I_0^1 fi(x)
	// where fi(x) = pdfi(x) Prod_j!=i cdfj(x) dx
	for (size_t i = 0; i < action2tv.size(); i++) {
		const BetaDistribution& beta = betas[i];
		double Pi = 0;
		for (int x_idx = 1; x_idx <= bins; x_idx++) {
			double x = (double)x_idx / (double)bins;
			double f_x = boost::math::pdf(beta, x) * step;
			for (size_t j = 0; j < action2tv.size(); j++)
				if (j != i)
					f_x *= cdfs[j][x_idx];
			Pi += f_x;
		}
		action2prob[std::next(action2tv.begin(), i)->first] = Pi;
	}
	return action2prob;
}

BetaDistribution ActionSelection::tv2beta(const TruthValuePtr& tv)
{
	double count = tv->getCount(),
		pos_count = tv->getMean() * count; // TODO correct when mean is fixed

	// Assuming the prior is Beta(1, 1)
	double alpha = 1 + pos_count;
	double beta = 1 + count - pos_count;

	return BetaDistribution(alpha, beta);
}

std::vector<double> ActionSelection::beta2cdf(const BetaDistribution& beta,
                                              int bins)
{
	std::vector<double> cdf;
	for (int x_idx = 1; x_idx <= bins; x_idx++) {
		double x = (double)x_idx / (double)bins;
		cdf.push_back(boost::math::cdf(beta, std::min(1.0, x)));
	}
	return cdf;
}
