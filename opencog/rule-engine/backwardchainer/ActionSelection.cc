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

namespace opencog {

ActionSelection::ActionSelection(const HandleTVMap& a2tv)
	: action2tv(a2tv) {}

HandleCounter ActionSelection::distribution()
{
	HandleCounter action2prob;
	const int bins = 100;             // Number of bins used for discretization

	// Calculate cdfs for all TVs
	std::vector<std::vector<double>> cdfs;
	for (const auto& atv : action2tv)
		cdfs.push_back(BetaDistribution(atv.second).cdf(bins));

	// Calculate Pi for all actions
	// where Pi = I_0^1 pdfi(x) Prod_j!=i cdfj(x) dx
	double nt = 0.0;            // normalizing term
	for (size_t i = 0; i < action2tv.size(); i++) {
		double p = Pi(i, cdfs);
		action2prob[std::next(action2tv.begin(), i)->first] = p;
		nt += p;
	}

	// Normalize so that it sums up to 1
	OC_ASSERT(0.0 < nt, "nt = %g, should be greater than zero", nt);
	for (auto& ap : action2prob)
		ap.second /= nt;

	return action2prob;
}

double ActionSelection::Pi(size_t i,
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
		for (size_t j = 0; j < action2tv.size(); j++)
			if (j != i)
				f_x *= cdfs[j][x_idx];
		result += f_x;
	}
	return result;
}

std::string oc_to_string(const ActionSelection& asel, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "action2tv:" << std::endl
	   << oc_to_string(asel.action2tv, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const HandleTVMap& h2tv, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << h2tv.size() << std::endl;
	int i = 0;
	for (const auto& htv : h2tv) {
		ss << indent << "atom[" << i << "]:" << std::endl
		   << oc_to_string(htv.first, indent + OC_TO_STRING_INDENT);
		ss << indent << "tv[" << i << "]:"
		   << oc_to_string(htv.second) << std::endl;
	}
	return ss.str();
}

} // ~namespace opencog
