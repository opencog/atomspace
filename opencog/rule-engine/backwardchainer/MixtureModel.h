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
#ifndef OPENCOG_MIXTUREMODEL_H_
#define OPENCOG_MIXTUREMODEL_H_

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
	 * Calculate the TV of the mixture model. According to Universal
	 * Operator Induction, assuming complete models, the equation
	 * rewritten for TVs is
	 *
	 * TV_MM(D') = Sum_i=0^n TV_Mi(D') * P(Mi) / Sum_i=0^n P(Mi)
	 *
	 * where
	 * - D' is the new data to explain
	 * - TV_MM(D') is the TV of the mixture model explaining D'
	 * - TV_Mi(D') is the TV of model Mi explaining D'
	 * - P(Mi) is the prior probability of model Mi
	 *
	 * TVs already captures the likelihood of the training data (the
	 * binomial part of the beta-binomial distribution underlying a
	 * TV, see Section 4.5.1 of the PLN book) which is why it doesn't
	 * appear in the equation.
	 *
	 * However, most of the time the models are partial, they are only
	 * active on a subset of observations. Ignoring the unexplained
	 * data would give an unfair advantage to partial models. Indeed
	 * the multiplicative factor to have the TV exactly equal to the
	 * likelihood of explaining the historical data could be ignored
	 * in the equation above because it is constant for all complete
	 * models, but it can no longer be ignored for partial
	 * models. Assuming Ni and Xi are respectively the number of total
	 * and positive observations defined in model Mi, this factor is
	 *
	 * 1 / (Ni+1)*(choose Ni Xi)
	 *
	 * which grows quadratically to exponentially as N goes down.
	 *
	 * In principle a way to deal with that would be to complete these
	 * partial models with as many models as possible. As it is
	 * costly, if not impractical, we will attempt to avoid that
	 * entirely and assume instead a fictive completion that perfectly
	 * explains the remaining data, leading to a likelihood of 1. It
	 * seems acceptable as such a fictive completion dominates all
	 * others in terms of fitness (pior*likelihood). However to do
	 * well we need to estimate its size, the Kolmogorov complexity of
	 * the unexplained data. Which almost brings us back to square
	 * one. For now we will use the following simplistic heuristic to
	 * estimate its Kolmogorov complexity
	 *
	 * K(D) = |D|^(1-c)
	 *
	 * where c is a compressiveness parameter, that ranges from 0, no
	 * compression, to 1, full compression.
	 *
	 * So for partial models the TV of the mixture model can be
	 * defined as followed
	 *
	 * TV_MM(D') = Sum_i=0^n TV_Mi(D') * P(Li + K(Di)) * ((Ni+1)*(choose Ni Xi))
	 *           / Sum_i=0^n P(Li + K(Di)) * ((Ni+1)*(choose Ni Xi))
	 *
	 * where
	 * - Li is the length of model Mi
	 * - Di are the unexplained data by model Mi
	 * - P(Li + K(Di)) is the prior of model Mi + perfect fictive completion
	 */
	TruthValuePtr operator()();

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
	 * Given a model, calculate it's prior estimate. In the case of a
	 * partial model, the length is estimated
	 */
	double prior_estimate(const Handle& model);

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
	double kolmogorov_estimate(double remain_data_size);

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
	double prior(double length);

private:
	/**
	 * Infer the data set size by taking the max count of all models
	 * (it works assuming that one of them is complete).
	 */
	double infer_data_set_size();

};

} // namespace opencog

#endif /* OPENCOG_MIXTUREMODEL_H_ */
