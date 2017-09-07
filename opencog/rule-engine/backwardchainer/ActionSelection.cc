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

#include "ActionSelection.h"

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

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
		betas.emplace_back(atv.second);

	// Calculate cdfs for all TV
	std::vector<std::vector<double>> cdfs;
	for (const auto& beta : betas)
		cdfs.push_back(beta.cdf(bins));

	// Calculate Pi for all actions
	// Pi = I_0^1 fi(x)
	// where fi(x) = pdfi(x) Prod_j!=i cdfj(x) dx
	double nt = 0.0;
	for (size_t i = 0; i < action2tv.size(); i++) {
		const BetaDistribution& beta = betas[i];
		double Pi = 0;
		for (int x_idx = 0; x_idx < bins; x_idx++) {
			double x = (x_idx + 1.0) / bins;
			double f_x = beta.pd(x) * step;
			// Only bother calculating Prod_j!=i cdfj(x) is pdfi(x) is
			// greater than zero
			if (f_x <= 0.0)
				continue;
			for (size_t j = 0; j < action2tv.size(); j++)
				if (j != i)
					f_x *= cdfs[j][x_idx];
			Pi += f_x;
		}
		action2prob[std::next(action2tv.begin(), i)->first] = Pi;
		nt += Pi;
	}

	// Normalize so that it sums up to 1
	OC_ASSERT(0.0 < nt, "nt = %g, should be greater than zero", nt);
	for (auto& ap : action2prob)
		ap.second /= nt;

	return action2prob;
}
