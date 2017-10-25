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

#include "MixtureModel.h"

#include <boost/math/special_functions/binomial.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/numeric.hpp>

#include <opencog/atomutils/FindUtils.h>
#include <opencog/truthvalue/SimpleTruthValue.h>

using namespace opencog;

using boost::math::binomial_coefficient;

MixtureModel::MixtureModel(const HandleSet& mds, double cpx, double cmp) :
	models(mds), cpx_penalty(cpx), compressiveness(cmp)
{
	data_set_size = infer_data_set_size();
}

TruthValuePtr MixtureModel::operator()()
{
	std::vector<TruthValuePtr> tvs;
	std::vector<double> weights;
	for (const Handle model : models) {
		// Get model's TV
		TruthValuePtr tv = model->getTruthValue();
		tvs.push_back(tv);

		// Calculate model's weight
		double count = tv->get_count(),
			pos_count = tv->get_mean() * count, // TODO correct when mean is fixed
			binom = binomial_coefficient<double>(count, pos_count);
		weights.push_back(prior_estimate(model) * (count+1) * binom);
	}
	return weighted_average(tvs, weights);
}

TruthValuePtr MixtureModel::weighted_average(const std::vector<TruthValuePtr>& tvs,
                                             const std::vector<double>& weights) const
{
	// Normalize the weights and get the TV means
	double total = boost::accumulate(weights, 0.0);
	std::vector<double> norm_weights, means;
	boost::transform(weights, std::back_inserter(norm_weights),
	                 [total](double w) { return w / total; });
	boost::transform(tvs, std::back_inserter(means),
	                 [](const TruthValuePtr& tv) { return tv->get_mean(); });

	// For now the formula is extremely approximative, instead we
	// could fit the mixture by a beta distribution, either
	// analytically (Extended Beta Distribution in R by Grun et al) or
	// numerically (Fitting Beta Distribution Based on Sample Data by
	// AbouRisk et al). Or, better, this should be moved in the truth
	// value API because a TV might not necessarily be a beta
	// distribution.
	double mean = boost::inner_product(norm_weights, means, 0.0);
	double confidence = 0.2;    // TODO
	return SimpleTruthValue::createSTV(mean, confidence);
}

double MixtureModel::prior_estimate(const Handle& model)
{
	HandleSet all_atoms(get_all_uniq_atoms(model));
	double partial_length = all_atoms.size();
	double remain_data_size = data_set_size - model->getTruthValue()->get_count();
	return prior(partial_length + kolmogorov_estimate(remain_data_size));
}

double MixtureModel::kolmogorov_estimate(double remain_count)
{
	return std::pow(remain_count, 1.0 - compressiveness);
}

double MixtureModel::prior(double length)
{
	return exp(-cpx_penalty*length);
}

double MixtureModel::infer_data_set_size()
{
	double max_count = 0.0;
	for (const Handle& model : models)
		max_count = std::max(max_count, model->getTruthValue()->get_count());
	return max_count;
}
