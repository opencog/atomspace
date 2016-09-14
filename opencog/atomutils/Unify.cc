/**
 * Unify.cc
 *
 * Utilities for unifying atoms.
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 * Author: Nil Geisweiller
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
 *
 * Created by Linas Vepstas February 2008
 */

#include "Unify.h"
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomutils/FindUtils.h>

namespace opencog {

BoolHandleMapSetPair unify(const Handle& lhs, const Handle& rhs,
                           const Handle& lhs_vardecl,
                           const Handle& rhs_vardecl)
{
	return unify_rec(lhs, rhs, lhs_vardecl, rhs_vardecl);
}

BoolHandleMapSetPair unify_rec(const Handle& lhs, const Handle& rhs,
                               const Handle& lhs_vardecl,
                               const Handle& rhs_vardecl)
{
	// Make sure both handles are defined
	if (lhs == Handle::UNDEFINED or rhs == Handle::UNDEFINED)
		return {false, HandleMapSet()};

	Type lhs_type(lhs->getType());
	Type rhs_type(rhs->getType());
	Arity lhs_arity(lhs->isNode() ? 0 : lhs->getArity());
	Arity rhs_arity(rhs->isNode() ? 0 : rhs->getArity());

	// Base cases
	if (lhs_arity == 0 or rhs_arity == 0) {
		if (lhs_type == VARIABLE_NODE or rhs_type == VARIABLE_NODE) {
			BoolHandleMapPair vares =
				var_merge(lhs, rhs, lhs_vardecl, rhs_vardecl);
			return {vares.first, {vares.second}};
		} else
			return {lhs == rhs, HandleMapSet()};
	}
	if (lhs_arity != rhs_arity)
		return {false, HandleMapSet()};
	
	// Recursive cases
	BoolHandleMapSetPair result;
	result.first = true;
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto res = unify_rec(lhs->getOutgoingAtom(i), rhs->getOutgoingAtom(i),
		                     lhs_vardecl, rhs_vardecl);
		result = mapset_merge(res, result);
		if (not result.first)     // If unification has failed stop now
			return result;
	}
	return result;
}

BoolHandleMapSetPair mapset_merge(const BoolHandleMapSetPair& lhs,
                                  const BoolHandleMapSetPair& rhs)
{
	// Don't bother merging is one has failed or one is empty
	if (not lhs.first or rhs.second.empty())
		return lhs;
	if (not rhs.first or lhs.second.empty())
		return rhs;
	
	// Merge
	BoolHandleMapSetPair result;
	for (const HandleMap& hm : rhs.second) {
		HandleMapSet hms(mapset_merge(lhs.second, hm));
		result.second.insert(hms.begin(), hms.end());
	}

	// If we get an empty merge whereas the inputs where not empty
	// then the merge has failed
	result.first = (not result.second.empty()) or
		(lhs.second.empty() and rhs.second.empty());
		
	return result;
}

HandleMapSet mapset_merge(const HandleMapSet& lhs, const HandleMap& rhs)
{
	HandleMapSet hms;
	if (lhs.empty())
		hms.insert(rhs);
	else {
		for (const auto& hm : lhs)
			hms.insert(map_merge(hm, rhs));
	}
	return hms;
}
	
HandleMap map_merge(const HandleMap& lhs, const HandleMap& rhs)
{
	HandleMap hm(lhs);
	for (const auto& el : rhs) {
		auto it = hm.find(el.first);
		if (it != hm.end()) {
			// If inconsistent then erase incompatible mapping
			if (it->second != el.second)
				hm.erase(it);
		} else {
			// Insert unexciting mapping for this key 
			hm.insert(el);
		}
	}
	return hm;
}

BoolHandleMapPair map_merge(const BoolHandleMapPair& lhs,
                            const BoolHandleMapPair& rhs)
{
	// If one of them is invalid then don't bother merging
	if (not lhs.first or not rhs.first)
		return {false, HandleMap()};

	// Both are valid
	BoolHandleMapPair bhmp(lhs);
	for (const auto& el : rhs.second) {
		auto it = bhmp.second.find(el.first);
		if (it != bhmp.second.end()) {
			// If inconsistent set the flag as invalid
			if (it->second != el.second) {
				bhmp.first = false;
				break;
			}
		} else {
			// Insert unexciting mapping for this key 
			bhmp.second.insert(el);
		}
	}
	return bhmp;
}

BoolHandleMapPair var_merge(const Handle& lhs, const Handle& rhs,
                            const Handle& lhs_vardecl,
                            const Handle& rhs_vardecl)
{
	return map_merge(oneway_var_merge(lhs, rhs, lhs_vardecl),
	                 oneway_var_merge(rhs, lhs, rhs_vardecl));
}

BoolHandleMapPair oneway_var_merge(const Handle& var, const Handle& val,
                                   const Handle& vardecl)
{
	Type var_type(var->getType());
	VariableListPtr vardecl_vlp(gen_varlist(var, vardecl));

	if (var_type == VARIABLE_NODE)
		return {vardecl_vlp->is_type(var, val), {{{var, val}}}};
	else
		return {true, HandleMap()};
}

/**
 * Generate a VariableList of the free variables of a given atom h.
 */
VariableListPtr gen_varlist(const Handle& h)
{
	OrderedHandleSet vars = get_free_variables(h);
	return createVariableList(HandleSeq(vars.begin(), vars.end()));
}

/**
 * Given an atom h and its variable declaration vardecl, turn the
 * vardecl into a VariableList if not already, and if undefined,
 * generate a VariableList of the free variables of h.
 */
VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl)
{
	if (vardecl == Handle::UNDEFINED)
		return gen_varlist(h);
	else {
		Type vardecl_t = vardecl->getType();
		if (vardecl_t == VARIABLE_LIST)
			return VariableListCast(vardecl);
		else {
			OC_ASSERT(vardecl_t == VARIABLE_NODE
			          or vardecl_t == TYPED_VARIABLE_LINK);
			return createVariableList(HandleSeq(1, vardecl));
		}
	}
}

std::string oc_to_string(const BoolHandleMapSetPair& bhmsp)
{
	std::stringstream ss;
	ss << "success: " << bhmsp.first << std::endl
	   << "mappings: " << oc_to_string(bhmsp.second);
	return ss.str();
}

} // namespace opencog
