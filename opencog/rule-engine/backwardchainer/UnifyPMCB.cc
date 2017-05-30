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

#include <opencog/util/exceptions.h>

#include "UnifyPMCB.h"

using namespace opencog;

/**
 * Constructor for the Unify PMCB.
 *
 * @param as        the AtomSpace pointer
 *
 * @param ext_vars  a VariableList of external variables that typed
 *                  variables can map to
 */
UnifyPMCB::UnifyPMCB(AtomSpace* as, VariableListPtr int_vars,
                     VariableListPtr ext_vars)
    : BackwardChainerPMCB(as, int_vars, false), _ext_vars(ext_vars)
{

}

UnifyPMCB::~UnifyPMCB()
{

}

bool UnifyPMCB::node_match(const Handle& npat_h, const Handle& nsoln_h)
{
	if (npat_h == nsoln_h)
		return true;

	// If the pattern is not a variable and the solution is a variable
	// then only check whether the type of the pattern matches the one
	// of the variable (if it has any)
	if (nsoln_h->getType() == VARIABLE_NODE) {
		return _ext_vars->is_type(nsoln_h, npat_h);
	}

	return false;
}

bool UnifyPMCB::variable_match(const Handle& npat_h,
                               const Handle& nsoln_h)
{
	Type soltype = nsoln_h->getType();

	// special case to allow any typed variable to match to a variable
	if (soltype == VARIABLE_NODE
	    and _ext_vars->get_variables().varset.count(nsoln_h) == 1)
	{
		// if the variable is untyped, match immediately
		if (_ext_vars->get_variables()._simple_typemap.count(nsoln_h) == 0
		    || _int_vars->get_variables()._simple_typemap.count(npat_h) == 0)
			return true;

		// otherwise the two variables type need to match
		if (_int_vars->get_variables()._simple_typemap.at(npat_h)
		    == _ext_vars->get_variables()._simple_typemap.at(nsoln_h))
			return true;

		// XXX TODO perform matching on deep types, if any.

		return false;
	}

	return BackwardChainerPMCB::variable_match(npat_h, nsoln_h);
}

bool UnifyPMCB::grounding(const HandleMap &var_soln,
                          const HandleMap &pred_soln)
{
	HandleMap true_var_soln;

	// get rid of non-var mapping
	for (const auto& p : var_soln)
	{
		if (_int_vars->get_variables().varset.count(p.first) == 1)
		{
			// Check if any typed variable maps to a variable, and if so,
			// store the reverse mapping. XXX This is not correct,
			// if there are non-trivial type restrictions (i.e. deep
			// types of fuzzy types). FIXME
			if (DefaultPatternMatchCB::_vars->_simple_typemap.count(p.first) == 1
			    && DefaultPatternMatchCB::_vars->_simple_typemap.at(p.first).count(p.second->getType()) == 0)
				true_var_soln[p.second] = p.first;
			else
				true_var_soln[p.first] = p.second;

			if (0 < DefaultPatternMatchCB::_vars->_deep_typemap.size())
				throw RuntimeException(TRACE_INFO,
					"Not implemented!  XXX FIXME!");

			if (0 < DefaultPatternMatchCB::_vars->_fuzzy_typemap.size())
				throw RuntimeException(TRACE_INFO,
					"Not implemented!  XXX FIXME!");
		}
	}

	// store the variable solution
	var_solns_.push_back(true_var_soln);
	pred_solns_.push_back(pred_soln);

	return false;
}
