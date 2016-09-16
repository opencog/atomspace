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

#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomutils/FindUtils.h>

namespace opencog {

UnificationSolutionSet::UnificationSolutionSet(bool s,
                                               const UnificationPartitions& p)
	: satisfiable(s), partitions(p)
{
}

UnificationSolutionSet unify(const Handle& lhs, const Handle& rhs,
                             const Handle& lhs_vardecl,
                             const Handle& rhs_vardecl)
{
	// Make sure both handles are defined
	if (lhs == Handle::UNDEFINED or rhs == Handle::UNDEFINED)
		return UnificationSolutionSet(false);

	Type lhs_type(lhs->getType());
	Type rhs_type(rhs->getType());

	// Base cases
	if (lhs->isNode() or rhs->isNode()) {
		if (lhs_type == VARIABLE_NODE or rhs_type == VARIABLE_NODE) {
			return mkvarsol(lhs, rhs, lhs_vardecl, rhs_vardecl);
		} else
			return UnificationSolutionSet(lhs == rhs);
	}

	// At least one of them is a link, check if they have the same
	// type (e.i. do they match so far)
	if (lhs_type != rhs_type)
		return UnificationSolutionSet(false);

	// At this point they are both links, check that they have the
	// same arity
	Arity lhs_arity(lhs->getArity());
	Arity rhs_arity(rhs->getArity());
	if (lhs_arity != rhs_arity)
		return UnificationSolutionSet(false);

	// Recursive cases
	UnificationSolutionSet sol;
	for (Arity i = 0; i < lhs_arity; ++i) {
		auto rs = unify(lhs->getOutgoingAtom(i), rhs->getOutgoingAtom(i),
		                lhs_vardecl, rhs_vardecl);
		sol = merge(sol, rs);
		if (not sol.satisfiable)     // Stop if unification has failed
			break;
	}
	return sol;
}

UnificationSolutionSet mkvarsol(const Handle& lhs, const Handle& rhs,
                                const Handle& lhs_vardecl,
                                const Handle& rhs_vardecl)
{
	Handle inter = type_intersection(lhs, rhs, lhs_vardecl, rhs_vardecl);
	if (inter == Handle::UNDEFINED)
		return UnificationSolutionSet(false);
	else {
		OrderedHandleSet hset{lhs, rhs};
		UnificationPartitions par{{{hset, inter}}};
		return UnificationSolutionSet(true, par);
	}
}

// TODO: very limited type intersection, should support structural
// types, etc.
Handle type_intersection(const Handle& lhs, const Handle& rhs,
                         const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	if (inherit(lhs, rhs, lhs_vardecl, rhs_vardecl))
		return lhs;
	if (inherit(rhs, lhs, rhs_vardecl, lhs_vardecl))
		return rhs;
	return Handle::UNDEFINED;
}

UnificationSolutionSet merge(const UnificationSolutionSet& lhs,
                             const UnificationSolutionSet& rhs)
{
	// Don't bother merging if one of them is invalid or empty
	if (not lhs.satisfiable or rhs.partitions.empty())
		return lhs;
	if (not rhs.satisfiable or lhs.partitions.empty())
		return rhs;

	// Merge
	UnificationSolutionSet result;
	for (const UnificationPartition& rp : rhs.partitions) {
		UnificationPartitions sol(merge(lhs.partitions, rp));
		result.partitions.insert(sol.begin(), sol.end());
	}

	// If we get an empty merge whereas the inputs where not empty
	// then the merge has failed
	result.satisfiable = (not result.partitions.empty()) or
		(lhs.partitions.empty() and rhs.partitions.empty());

	return result;
}

UnificationPartitions merge(const UnificationPartitions& lhs,
                            const UnificationPartition& rhs)
{
	UnificationPartitions result;
	if (lhs.empty())
		result.insert(rhs);
	else {
		for (const auto& par : lhs) {
			result.insert(merge(par, rhs));
		}
	}
	return result;
}

UnificationPartition merge(const UnificationPartition& lhs,
                           const UnificationPartition& rhs)
{
	// Don't bother merging if one of them is empty
	if (lhs.empty())
		return rhs;
	if (rhs.empty())
		return lhs;

	// Do the actual merging
	UnificationPartition result(lhs);
	for (const auto& typed_block : rhs) {
		for (auto it = lhs.begin(); it != lhs.end(); ++it) {
			if (has_empty_intersection(typed_block.first, it->first)) {
				// Merely insert this independent block
				result.insert(typed_block);
			} else {
				// Merge the 2 equality related blocks
				UnificationBlock m_typed_block = merge(typed_block, *it);
				// If the resulting block is valid then replace *it
				if (is_valid(m_typed_block)) {
					result.erase(it);
					result.insert(m_typed_block);
				}
				// If the resulting block is invalid then the partition is
				// invalid as well, thus an empty partition is returned
				else {
					return UnificationPartition();
				}
			}
		}
	}
	return result;
}

UnificationBlock merge(const UnificationBlock& lhs, const UnificationBlock& rhs)
{
	return {set_union(lhs.first, rhs.first),
			type_intersection(lhs.second, rhs.second)};
}

bool is_valid(const UnificationBlock& block)
{
	return block.second == Handle::UNDEFINED;
}

bool inherit(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	VariableListPtr rhs_vardecl_vlp(gen_varlist(rhs, rhs_vardecl));
	const Variables& rhs_variables(rhs_vardecl_vlp->get_variables());
	const VariableTypeMap& rhs_vtm = rhs_variables._simple_typemap;
	if (lhs->getType() == VARIABLE_NODE) {
		VariableListPtr lhs_vardecl_vlp(gen_varlist(lhs, lhs_vardecl));
		// Compare the 2 variables types
		const Variables& lhs_variables(lhs_vardecl_vlp->get_variables());
		const VariableTypeMap& lhs_vtm = lhs_variables._simple_typemap;
		auto lhs_it = lhs_vtm.find(rhs);
		return rhs->getType() == VARIABLE_NODE; // TODO
	}
	else return rhs_vardecl_vlp->is_type(rhs, lhs);
}

bool inherit(const std::set<Type>& lhs, const std::set<Type>& rhs)
{
	return true; // TODO
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

std::string oc_to_string(const UnificationPartition& up)
{
	std::stringstream ss;
	ss << "size = " << up.size() << std::endl;
	int i = 0;
	for (const auto& p : up) {
		ss << "block[" << i << "]:" << std::endl << oc_to_string(p.first)
		   << "type[" << i << "]:" << std::endl << oc_to_string(p.second);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const UnificationPartitions& par)
{
	std::stringstream ss;
	ss << "size = " << par.size() << std::endl;
	int i = 0;
	for (const auto& el : par) {
		ss << "typed partition[" << i << "]:" << std::endl << oc_to_string(el);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const UnificationSolutionSet& sol)
{
	std::stringstream ss;
	ss << "satisfiable: " << sol.satisfiable << std::endl
	   << "partitions: " << oc_to_string(sol.partitions);
	return ss.str();
}

} // namespace opencog
