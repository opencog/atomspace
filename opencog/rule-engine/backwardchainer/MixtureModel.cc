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

#include <opencog/util/numeric.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/truthvalue/SimpleTruthValue.h>

#include "BetaDistribution.h"
#include "../URELogger.h"

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
		tvs.push_back(model->getTruthValue());
		weights.push_back(prior_estimate(model));
	}
	return weighted_average(tvs, weights);
}

TruthValuePtr MixtureModel::weighted_average(const std::vector<TruthValuePtr>& tvs,
                                             const std::vector<double>& weights) const
{
	// Don't bother mixing if there's only one TV
	if (tvs.size () == 1)
		return tvs[0];

	// Normalize the weights
	double total = boost::accumulate(weights, 0.0);
	std::vector<double> norm_weights;
	boost::transform(weights, std::back_inserter(norm_weights),
	                 [total](double w) { return w / total; });

	// Calculate the TV means and variances
	std::vector<BetaDistribution> dists;
	std::vector<double> means, variances;
	boost::transform(tvs, std::back_inserter(dists), mk_beta_distribution);
	boost::transform(dists, std::back_inserter(means),
	                 [](const BetaDistribution& bd) { return bd.mean(); });
	boost::transform(tvs, std::back_inserter(variances),
	                 [](const BetaDistribution& bd) { return bd.variance(); });

	// For now the mixed TV remains a SimpleTV, thus a
	// beta-distribution. The mean and variance is calculated
	// according to
	// https://en.wikipedia.org/wiki/Mixture_distribution#Moments
	double mean = boost::inner_product(norm_weights, means, 0.0);
	std::vector<double> relative_variances(variances);
	for (std::size_t i = 0; i < relative_variances.size(); i++)
		relative_variances[i] += sq(means[i] - mean);
	double variance = boost::inner_product(norm_weights, relative_variances, 0.0);

	return mk_stv(mean, variance);
}

double MixtureModel::prior_estimate(const Handle& model)
{
	HandleSet all_atoms(get_all_uniq_atoms(model));
	double partial_length = all_atoms.size(),
		remain_data_size = data_set_size - model->getTruthValue()->get_count(),
		kestimate = kolmogorov_estimate(remain_data_size);

	ure_logger().fine() << "MixtureModel::prior_estimate "
	                    << "partial_length = " << partial_length
	                    << ", remain_data_size = " << remain_data_size
	                    << ", kestimate = " << kestimate;

	return prior(partial_length + kestimate);
}

double MixtureModel::kolmogorov_estimate(double remain_count)
{
	return std::pow(remain_count, 1.0 - compressiveness);
}

double MixtureModel::prior(double length)
{
	ure_logger().fine() << "MixtureModel::prior length = " << length
	                    << ", prior = " << exp(-cpx_penalty*length);
	return exp(-cpx_penalty*length);
}

double MixtureModel::infer_data_set_size()
{
	double max_count = 0.0;
	for (const Handle& model : models)
		max_count = std::max(max_count, model->getTruthValue()->get_count());
	return max_count;
}
