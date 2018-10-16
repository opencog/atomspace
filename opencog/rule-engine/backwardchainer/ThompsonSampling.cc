/*
 * ThompsonSampling.cc
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

#include "ThompsonSampling.h"

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/random.h>

#include <opencog/atoms/base/Handle.h>

namespace opencog {

ThompsonSampling::ThompsonSampling(const TruthValueSeq& tvs, unsigned bins)
	: _tvs(tvs), _bins(bins) {}

std::vector<double> ThompsonSampling::distribution()
{
	std::vector<double> probs(_tvs.size());

	// Calculate cdfs for all TVs
	std::vector<std::vector<double>> cdfs;
	for (const auto& tv : _tvs)
		cdfs.push_back(BetaDistribution(tv).cdf(_bins));

	// Calculate Pi for all actions
	// where Pi = I_0^1 pdfi(x) Prod_j!=i cdfj(x) dx
	double nt = 0.0;            // normalizing term
	for (size_t i = 0; i < _tvs.size(); i++) {
		probs[i] = Pi(i, cdfs);
		nt += probs[i];
	}

	// Normalize so that it sums up to 1
	OC_ASSERT(0.0 < nt, "nt = %g, should be greater than zero", nt);
	for (auto& p : probs)
		p /= nt;

	return probs;
}

size_t ThompsonSampling::operator()()
{
	std::vector<double> weights = distribution();
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return dist(randGen());
}

double ThompsonSampling::Pi(size_t i,
                            const std::vector<std::vector<double>>& cdfs) const
{
	double result = 0.0;
	size_t bins = cdfs[i].size();
	// Perform right-end point Riemann sum of fi(x)
	for (size_t x_idx = 0; x_idx < bins; x_idx++) {
		// Calculate pdfi(x)*dx, that is the probability of the first
		// order probability being within [(x_idx-1)/bins, x_idx/bins]
		// using the derivative of the cdf
		double f_x = cdfs[i][x_idx] - (x_idx == 0 ? 0.0 : cdfs[i][x_idx - 1]);

		// Only bother calculating Prod_j!=i cdfj(x) if pdfi(x)*dx is
		// greater than zero
		if (f_x <= 0.0)
			continue;

		// Calculate Prod_j!=i cdfj(x)
		for (size_t j = 0; j < _tvs.size(); j++)
			if (j != i)
				f_x *= cdfs[j][x_idx];
		result += f_x;
	}
	return result;
}

std::string ThompsonSampling::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "bins: " << _bins << std::endl
	   << indent << "tvs:" << std::endl
	   << oc_to_string(_tvs, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const ThompsonSampling& tsmp, const std::string& indent)
{
	return tsmp.to_string();
}

} // ~namespace opencog
