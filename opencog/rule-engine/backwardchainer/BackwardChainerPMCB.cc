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

BackwardChainerPMCB::BackwardChainerPMCB(AtomSpace* as, VariableListPtr int_vars, bool reverse)
    : InitiateSearchCB(as),
      DefaultPatternMatchCB(as),
      _as(as),
      _int_vars(int_vars),
      _reverse_enabled(reverse)
{
}

BackwardChainerPMCB::~BackwardChainerPMCB()
{
}

/**
 * Override the initiate_search method.
 *
 * Because if _reverse is enabled, but any nodes & links can be
 * matched to a VariableNode, and the default neighbor_search will
 * put too much restriction for such cases.
 *
 * @param pme   same as default
 * @return      true to stop search for more
 */
bool BackwardChainerPMCB::initiate_search(PatternMatchEngine* pme)
{
	// use the default starter if _reverse is not enabled
	if (not _reverse_enabled)
		return InitiateSearchCB::initiate_search(pme);

	// else skip over neighbor_search since that does not apply

	_search_fail = false;
	bool found = link_type_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	_search_fail = false;
	found = variable_search(pme);
	return found;
}

/**
 * Override the node_match method.
 *
 * This is needed so that something like
 *
 *   InheritanceLink
 *     ConceptNode "Fritz"
 *     ConceptNode "green"
 *
 * can be matched to
 *
 *   InheritanceLink
 *     VariableNode "$X"
 *     ConceptNode "green"
 *
 * so that Truth Value Query can generate additional targets that are
 * themselves Variable Fullfillment Query.
 *
 * @param npat_h   same as default
 * @param nsoln_h  same as default
 * @return         true if matched
 */
bool BackwardChainerPMCB::node_match(const Handle& npat_h, const Handle& nsoln_h)
{
	if (npat_h == nsoln_h)
		return true;

	if (not _reverse_enabled)
		return false;

	// if npat_h itself is a VariableNode, then this means it is QuoteLink-ed
	// in this case we don't want other VariableNode to map to it, since
	// the purpose was for npat_h to match to itself
	if (npat_h->getType() == VARIABLE_NODE)
		return false;

	// allows any normal node to map to a VariableNode (assume untyped)
	// XXX will it ever map to a typed VariableNode?  What would that mean?
	if (nsoln_h->getType() == VARIABLE_NODE)
		return true;

	return false;
}

/**
 * Override the fuzzy_match callback.
 *
 * This is for matching any link to an external VariableNode.
 *
 * @param ph  same as default
 * @param gh  same as default
 * @return    true if matched
 */
bool BackwardChainerPMCB::fuzzy_match(const Handle& ph, const Handle& gh)
{
	if (not _reverse_enabled)
		return false;

	// avoid Handle::UNDEFINED
	if (ph == Handle::UNDEFINED || gh == Handle::UNDEFINED)
		return false;

	// if the proposed grounding is not a link but is a VariableNode
	if (gh->getType() == VARIABLE_NODE)
		return true;

	return false;
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
		else if (p.second->getType() == VARIABLE_NODE)
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
