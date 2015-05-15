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
