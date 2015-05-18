/*
 * UnifyPMCB.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: William Ma <https://github.com/williampma>
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

#include "UnifyPMCB.h"

using namespace opencog;

UnifyPMCB::UnifyPMCB(AtomSpace* as) : BackwardChainerPMCB(as)
{

}

UnifyPMCB::~UnifyPMCB()
{

}

bool UnifyPMCB::variable_match(const Handle& npat_h,
                               const Handle& nsoln_h)
{
	Type soltype = nsoln_h->getType();

	// special case to allow any typed variable to match to a variable
	if (soltype == VARIABLE_NODE) return true;

	return BackwardChainerPMCB::variable_match(npat_h, nsoln_h);
}

bool UnifyPMCB::grounding(const std::map<Handle, Handle> &var_soln,
                          const std::map<Handle, Handle> &pred_soln)
{
	std::map<Handle, Handle> true_var_soln;

	// get rid of non-var mapping
	for (auto& p : var_soln)
	{
		if (p.first->getType() == VARIABLE_NODE)
		{
			// check if any typed variable map to a variable, and if so,
			// store the reverse mapping
			if (DefaultPatternMatchCB::_type_restrictions->count(p.first) == 1
			    && DefaultPatternMatchCB::_type_restrictions->at(p.first).count(p.second->getType()) == 0)
				true_var_soln[p.second] = p.first;
			else
				true_var_soln[p.first] = p.second;
		}
	}

	// store the variable solution
	var_solns_.push_back(true_var_soln);
	pred_solns_.push_back(pred_soln);

	return false;
}
