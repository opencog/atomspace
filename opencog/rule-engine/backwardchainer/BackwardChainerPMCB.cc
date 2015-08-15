/*
 * BackwardChainerPMCB.cc
 *
 * Copyright (C) 2014 Misgana Bayetta
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
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

#include "BackwardChainerPMCB.h"

using namespace opencog;

BackwardChainerPMCB::BackwardChainerPMCB(AtomSpace* as, VariableListPtr int_vars)
    : InitiateSearchCB(as),
      DefaultPatternMatchCB(as),
      _as(as),
      _int_vars(int_vars)
{
}

BackwardChainerPMCB::~BackwardChainerPMCB()
{
}

bool BackwardChainerPMCB::grounding(const std::map<Handle, Handle> &var_soln,
                               const std::map<Handle, Handle> &pred_soln)
{
	std::map<Handle, Handle> true_var_soln;

	// get rid of non-var mapping
	for (auto& p : var_soln)
	{
		if (_int_vars->get_variables().varset.count(p.first) == 1)
			true_var_soln[p.first] = p.second;
	}

	if (true_var_soln.size() == 0)
		return false;

	// store the variable solution
	var_solns_.push_back(true_var_soln);
	pred_solns_.push_back(pred_soln);

	return false;
}

std::vector<std::map<Handle, Handle>> BackwardChainerPMCB::get_var_list()
{
	return var_solns_;
}
std::vector<std::map<Handle, Handle>> BackwardChainerPMCB::get_pred_list()
{
	return pred_solns_;
}
