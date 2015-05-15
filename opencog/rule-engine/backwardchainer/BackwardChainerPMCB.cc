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

BackwardChainerPMCB::BackwardChainerPMCB(AtomSpace * as)
    : InitiateSearchCB(as), DefaultPatternMatchCB(as), as_(as)
 //       : Implicator(as), DefaultPatternMatchCB(as), AttentionalFocusCB(as), PLNImplicator(as), as_(as)
{
}

BackwardChainerPMCB::~BackwardChainerPMCB()
{
}

bool BackwardChainerPMCB::node_match(Handle& node1, Handle& node2)
{
	return DefaultPatternMatchCB::node_match(node1, node2);

	//return AttentionalFocusCB::node_match(node1, node2);
}

bool BackwardChainerPMCB::link_match(LinkPtr& lpat, LinkPtr& lsoln)
{
	return DefaultPatternMatchCB::link_match(lpat, lsoln);

	//return AttentionalFocusCB::link_match(lpat, lsoln);
}

bool BackwardChainerPMCB::grounding(const std::map<Handle, Handle> &var_soln,
                               const std::map<Handle, Handle> &pred_soln)
{
	std::map<Handle, Handle> true_var_soln;

	// get rid of non-var mapping
	for (auto& p : var_soln)
	{
		if (p.first->getType() == VARIABLE_NODE)
			true_var_soln[p.first] = p.second;
	}

	// XXX TODO if a variable match to itself, reject?

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
