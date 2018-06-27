/*
 * MixtureModel.h
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
#ifndef _OPENCOG_MIXTUREMODEL_H_
#define _OPENCOG_MIXTUREMODEL_H_

#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{

/**
 * Class containing methods to calculate the TruthValue of constructed
 * from the mixture of a set of active partial models. Since these
 * models might be active over different training sets, the mixture
 * needs to take into account how complexity is hidden in the part of
 * the model explaining the remaining data.
 */
class MixtureModel
{
public:
	// Set of active models. Active means that they fulfill the
	// preconditions of the data to explain.
	HandleSet models;

	// Parameter to control the complexity penalty over the control
	// rules. Ranges from 0, no penalty to +inf, infinit
	// penalty. Affect the calculation of the control rule prior, see
	// the control_rule_prior method. Used to calculate the TV of
	// success in case multiple control rules are active.
	double cpx_penalty;

	// Parameter to estimate the length of a whole model given a
	// partial model + unexplained data. Ranges from 0 to 1, 0 being
	// no compressiveness at all of the unexplained data, 1 being full
	// compressiveness.
	double compressiveness;

	// Size of the complete data set, including all observations used
	// to build the models. For simplicity we're gonna assume that it
	// is the max of all counts over the models. Meaning that to do
	// well, at least one model has to be complete, however bad this
	// model might be.
	double data_set_size;

	/**
	 * Ctor
	 */
	MixtureModel(const HandleSet& models,
	             double cpx_penalty=1.0,
	             double compressiveness=0.0);

	/**
	 * Calculate the TV of the mixture model. Assuming the ith model,
	 * Mi, with prior Pi, has its TV represented by a probabilistic
	 * density function pdf_i, a beta-distributions with parameters
	 * alpha_i, beta_i, then, an optimal mixture, according to
	 * Universal Operator Induction, may be approached with
	 *
	 * Sum_i Pi * pdf_i * Beta(alpha_i, beta_i)
	 *
	 * up to a normalizing factor so that the resulting pdf integrates
	 * to 1. The reason for that is lengthly explained in Subsection
	 * Combining Inference Control Rules of
	 * <OPENCOG_ROOT>/examples/pln/inference-control-learning/README.md
	 *
	 * Since Mi may be based on a subet of observations, Pi must
	 * account for the complexity of the missing observations. We use
	 * the simplistic heuristic to estimate the Kolmogorov complexity
	 * of the missing observations D
	 *
	 * K(D) = |D|^(1-c)
	 *
	 * where c is a compressiveness parameter, that ranges from 0, no
	 * compression, to 1, full compression.
	 *
	 * So for partial models the pdf of the mixture model can be
	 * defined as followed
	 *
	 * Sum_i P(Li + K(Di)) * pdf_i * Beta(alpha_i, beta_i)
	 *
	 * where
	 * - Li is the length of model Mi
	 * - Di are the missing data of model Mi
	 * - P(Li + K(Di)) is the prior of model Mi + perfect fictive completion
	 */
	TruthValuePtr operator()() const;

	/**
	 * Given a list of TVs and a list of associated weights, that do
	 * not need to sum up to 1, produce a TVs that approximates the
	 * normalized weighted average of the given TVs.
	 *
	 * For now a Simple Truth Value will be produced, meaning that it
	 * will probably badly reflect the actually distribution, but
	 * should at least have the same mean and variance.
	 */
	TruthValuePtr weighted_average(const std::vector<TruthValuePtr>& tvs,
	                               const std::vector<double>& weights) const;

	/**
	 * Calculate the alpha and beta parameters of the model's TV, and
	 * return Beta(alpha, beta), where Beta is the beta function.
	 */
	double beta_factor(const Handle& model) const;

	/**
	 * Given a model, calculate it's prior estimate. In the case of a
	 * partial model, the length is estimated
	 */
	double prior_estimate(const Handle& model) const;

	/**
	 * Given the size of the data set that isn't explained by a model,
	 * estimate the complexity of a model that would explain them
	 * perfectly. The heuristic used here is
	 *
	 * remain_data_size^(1 - compressiveness)
	 *
	 * If compressiveness is null, then no compression occurs, the
	 * model is the data set itself, if compressiveness equals 1, then
	 * it return 1, which is the maximum compression, all data can be
	 * explained with just one bit.
	 */
	double kolmogorov_estimate(double remain_data_size) const;

	/**
	 * Given the length of a model, calculate its prior
	 *
	 * exp(-cpx_penalty*length)
	 *
	 * where cpx_penalty is a complexity penalty parameter (0 for no
	 * penalty, +inf for infinit penalty), and length is the size of
	 * the model, the total number of atoms involved in its
	 * definition.
	 *
	 * The prior doesn't have to sum up to 1 because the probability
	 * estimates are normalized.
	 */
	double prior(double length) const;

private:
	/**
	 * Infer the data set size by taking the max count of all models
	 * (it works assuming that one of them is complete).
	 */
	double infer_data_set_size() const;

};

} // namespace opencog

#endif /* _OPENCOG_MIXTUREMODEL_H_ */
