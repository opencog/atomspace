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

BackwardChainerPMCB::BackwardChainerPMCB(AtomSpace* as,
                                         VariableListPtr int_vars,
                                         bool name_check)
    : InitiateSearchCB(as),
      DefaultPatternMatchCB(as),
      _as(as),
      _int_vars(int_vars),
      _enable_var_name_check(name_check)
{
}

BackwardChainerPMCB::~BackwardChainerPMCB()
{
}

bool BackwardChainerPMCB::grounding(const HandleMap &var_soln,
                                    const HandleMap &pred_soln)
{
	HandleMap true_var_soln;
	HandleMap true_pred_soln;

	// get rid of non-var mapping
	for (auto& p : var_soln)
	{
		if (_int_vars->get_variables().varset.count(p.first) == 1)
			true_var_soln[p.first] = p.second;
	}

	if (true_var_soln.size() == 0)
		return false;

	true_pred_soln = pred_soln;

	// add all the constant clauses into the final pred_soln
	for (auto& h : _pattern->constants)
		true_pred_soln[h] = h;

	// store the variable solution
	var_solns_.push_back(true_var_soln);
	pred_solns_.push_back(true_pred_soln);

	return false;
}

const HandleMapSeq& BackwardChainerPMCB::get_var_list() const
{
	return var_solns_;
}
const HandleMapSeq& BackwardChainerPMCB::get_pred_list() const
{
	return pred_solns_;
}
