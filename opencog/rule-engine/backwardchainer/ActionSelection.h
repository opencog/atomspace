/*
 * ActionSelection.h
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
#ifndef _OPENCOG_ACTIONSELECTION_H_
#define _OPENCOG_ACTIONSELECTION_H_

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>

#include "BetaDistribution.h"

namespace opencog
{

//! a map from handles to truth values
typedef std::map<Handle, TruthValuePtr> HandleTVMap;

/**
 * Class containing methods to calculate distribution over actions
 * given the TruthValue that each action fulfills the objective of
 * interest.
 */
class ActionSelection
{
public:
	const HandleTVMap& action2tv;

	/**
	 * CTor
	 */
	ActionSelection(const HandleTVMap& action2tv);

	/**
	 * Return the action distribution, a probability for each action
	 * to be used as sampling distribution for picking the next
	 * action. The distribution attempts to reflect the optimal
	 * balance between exploration and exploitation (Thompson
	 * sampling).
	 *
	 * Pi = I_0^1 pdfi(x) Prod_j!=i cdfj(x) dx / nt
	 *
	 * where `Prod_j!=i fi` is the product of all fi with j from 1 to
	 * n, except i, nt is a normalizing factor and Pi is the
	 * probability that action i is the best.
	 *
	 * See Section Inference Rule Selection in the README.md of the
	 * pln inference-control-learning for more explanations.
	 */
	HandleCounter distribution();

	/**
	 * Perform random action selection according to the action
	 * distribution.
	 */
	Handle operator()();

private:
	/**
	 * Helper for distribution(). Given a vector of cdf vector, one
	 * cdf vector per action, calculate the unnormalized Pi (see the
	 * comment of distribution()) for action i.
	 */
	double Pi(size_t i, const std::vector<std::vector<double>>& cdfs) const;
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string	oc_to_string(const ActionSelection& asel,
                         const std::string& indent=empty_string);
std::string	oc_to_string(const HandleTVMap& h2tv,
                         const std::string& indent=empty_string);

} // namespace opencog

#endif /* _OPENCOG_ACTIONSELECTION_H_ */
