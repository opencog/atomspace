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

#include <boost/math/special_functions/beta.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/numeric.hpp>

#include <opencog/util/numeric.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/truthvalue/SimpleTruthValue.h>

#include "BetaDistribution.h"
#include "../URELogger.h"

using namespace opencog;

MixtureModel::MixtureModel(const HandleSet& mds, double cpx, double cmp) :
	models(mds), cpx_penalty(cpx), compressiveness(cmp)
{
	data_set_size = infer_data_set_size();
}

TruthValuePtr MixtureModel::operator()() const
{
	// Don't bother mixing if there's only one TV
	if (models.size() == 1)
		return (*models.begin())->getTruthValue();

	std::vector<TruthValuePtr> tvs;
	std::vector<double> weights;
	for (const Handle model : models) {
		double weight = prior_estimate(model) * beta_factor(model);
		LAZY_URE_LOG_FINE << "MixtureModel::operator model = " << model->id_to_string()
		                  << ", weight = " << weight;
		tvs.push_back(model->getTruthValue());
		weights.push_back(weight);
	}
	return weighted_average(tvs, weights);
}

TruthValuePtr MixtureModel::weighted_average(const std::vector<TruthValuePtr>& tvs,
                                             const std::vector<double>& weights) const
{
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

	LAZY_URE_LOG_FINE << "MixtureModel::weighted_average mean = " << mean
	                  << ", variance = " << variance;

	return mk_stv(mean, variance);
}

double MixtureModel::beta_factor(const Handle& model) const
{
	BetaDistribution beta_dist(model->getTruthValue());
	double factor = boost::math::beta(beta_dist.alpha(), beta_dist.beta());
	LAZY_URE_LOG_FINE << "MixtureModel::beta_factor factor = " << factor;
	return factor;
}

double MixtureModel::prior_estimate(const Handle& model) const
{
	LAZY_URE_LOG_FINE << "MixtureModel::prior_estimate model = " << model->id_to_string();

	HandleSet all_atoms(get_all_uniq_atoms(model));
	double partial_length = all_atoms.size(),
		remain_data_size = data_set_size - model->getTruthValue()->get_count(),
		kestimate = kolmogorov_estimate(remain_data_size);

	LAZY_URE_LOG_FINE << "MixtureModel::prior_estimate "
	                  << "partial_length = " << partial_length
	                  << ", remain_data_size = " << remain_data_size
	                  << ", kestimate = " << kestimate;

	return prior(partial_length + kestimate);
}

double MixtureModel::kolmogorov_estimate(double remain_count) const
{
	return std::pow(remain_count, 1.0 - compressiveness);
}

double MixtureModel::prior(double length) const
{
	double pri = exp(-cpx_penalty*length);
	LAZY_URE_LOG_FINE << "MixtureModel::prior length = " << length
	                  << ", pri = " << pri;
	return pri;
}

double MixtureModel::infer_data_set_size() const
{
	double max_count = 0.0;
	for (const Handle& model : models)
		max_count = std::max(max_count, model->getTruthValue()->get_count());
	return max_count;
}
