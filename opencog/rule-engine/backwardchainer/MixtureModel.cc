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

#include <opencog/atomutils/FindUtils.h>

using namespace opencog;

MixtureModel::MixtureModel(const HandleSet& mds, double cpx, double cmp) :
	models(mds), cpx_penalty(cpx), compressiveness(cmp)
{
	data_set_size = infer_data_set_size();
}

TruthValuePtr MixtureModel::operator()()
{
	// TODO
	std::vector<double> weights;
	return TruthValuePtr();
}

TruthValuePtr MixtureModel::weighted_average(const std::vector<TruthValuePtr>& tvs,
                                             const std::vector<double>& weights) const
{
	// TODO
	return TruthValuePtr();
}

double MixtureModel::prior_estimate(const Handle& model)
{
	HandleSet all_atoms(get_all_uniq_atoms(model));
	double partial_length = all_atoms.size();
	double remain_data_size = data_set_size - model->getTruthValue()->getCount();
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
		max_count = std::max(max_count, model->getTruthValue()->getCount());
	return max_count;
}
