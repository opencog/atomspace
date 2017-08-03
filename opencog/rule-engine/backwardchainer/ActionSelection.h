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
#ifndef OPENCOG_ACTIONSELECTION_H_
#define OPENCOG_ACTIONSELECTION_H_

#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>

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
	ActionSelection(const HandleTVMap& action_tvs);

	/**
	 * Return the action distribution, a probability over each action
	 * to be used for sampling the next action. The distribution tries
	 * to reflect the optimal balance between exploration and
	 * exploitation.
	 *
	 * TODO: add documentation (taken from the pln
	 * inference-control-learning README.md example).
	 */
	HandleCounter distribution();

	/**
	 * Perform random action selection according to the action
	 * distribution.
	 */
	Handle operator()();
};

} // namespace opencog

#endif /* OPENCOG_ACTIONSELECTION_H_ */
