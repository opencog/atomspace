/*
 * atoms/core/FreeVariables.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *               2019 SingularityNET Foundation
 *
 * Authors: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *          Nil Geisweiller <ngeiswei@gmail.com> Oct 2019
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/util/algorithm.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/Context.h>
#include <opencog/atoms/core/FindUtils.h>
#include "FreeVariables.h"

namespace opencog {

/// The VarScraper struct provides the functions to find and sort free
/// variables in a scope link that does not contain variable
/// declaration or contains an unordered one (using VariableSet). The
/// sorting of free variables only depends on the semantics of the
/// variables, that is their positions in the scope, taking into
/// account the commutativity property of unordered links.
struct VarScraper
{
	/**
	 * Mapping variables to their paths, where a path is a list of
	 * pairs of atom type and outgoing index, with the particularity
	 * that when the type is a subtype of unordered link, the index is
	 * zero, because it is semantically meaningless.
	 */
	// The _paths holds paths between variables and root. A path is a
	// sequence of pairs (Type, Index), where Type is the type of a link
	// and index is the index of the outgoing of that link. If the type
	// unordered however, then index is zero, because in such case the
	// index has no meaning. The collection of paths is stored in a
	// multiset because some paths can be identical looking while in
	// fact different in reality, due to the index of a unordered link
	// being always zero. We do that to be able to use std::operator<
	// rather than provide our own.
	//
	// Note: a notion of path is already implemented in PatternTerm,
	// It might be a good idea to unify this mechanism with that one.
	HandlePathsMap _paths;

	/**
	 * Find variable declaration in such order that is consistent with
	 * alpha-equivalence.
	 */
	HandleSeq operator()(const HandleSeq& outgoing, bool ordered_link);

	/**
	 * Return a mapping between variables (free and non free) and their
	 * paths. The reason we need the path of non free variables is
	 * because they are subject to semantic comparison just like the
	 * rest.
	 *
	 * Note: context is passed by copy for implementation convenience.
	 */
	static HandlePathsMap variables_paths(const Handle& h);
	static HandlePathsMap variables_paths(Type incoming_type,
	                                      const HandleSeq& outgoing);

	/**
	 * Find all free variables in body and return them in a canonical
	 * order, so that if the variables were renamed and rerun this
	 * function we would obtain the same order, or an order that does
	 * not affect the semantics.
	 *
	 * Note: context is passed by copy for implementation convenience.
	 */
	HandleSeq sorted_free_variables(const Handle& body, Context ctx=Context()) const;

	/**
	 * Like above but operates on the outgoing set of an ordered
	 * (resp. unordered) link.
	 */
	HandleSeq sorted_free_variables_outgoing(
		bool ordered, const HandleSeq& outgoing, const Context& ctx=Context()) const;
	HandleSeq sorted_free_variables_ordered_outgoing(
		const HandleSeq& outs, const Context& ctx=Context()) const;
	HandleSeq sorted_free_variables_unordered_outgoing(
		const HandleSeq& outs, const Context& ctx=Context()) const;

	/**
	 * Return a sorted outgoing set according to a canonical order.
	 */
	HandleSeq sorted(HandleSeq hs) const;

	/**
	 * Canonical order, so that variables get sorted according to their
	 * semantics. This allows to have the order be consistent with
	 * alpha-equivalent.
	 */
	bool less_than(const Handle& lh, const Handle& rh) const;

	/**
	 * Like less_than but over the outgoings of an ordered
	 * (resp. unordered) link.
	 */
	bool less_than_ordered_outgoing(const HandleSeq& lhs,
	                                const HandleSeq& rhs) const;
	bool less_than_unordered_outgoing(const HandleSeq& lhs,
	                                  const HandleSeq& rhs) const;

	/**
	 * Return true iff the two handle are alpha-equivalent, according
	 * to the following rules:
	 *
	 * - constant nodes: atom equality
	 * - variable nodes: path equality
	 * - links: type equality and equivalence of their outgoings
	 */
	bool is_equivalent(const Handle& rh, const Handle& lh) const;
	bool is_equivalent_ordered_outgoing(const HandleSeq& rhs,
	                                    const HandleSeq& lhs) const;
	bool is_equivalent_unordered_outgoing(const HandleSeq& rhs,
	                                      const HandleSeq& lhs) const;

	/**
	 * Return true iff h is a Variable or Glob node.
	 */
	static bool is_variable(const Handle& h);

	/**
	 * Return true iff is an unordered link type or subtype.
	 */
	static bool is_unordered(Type t);
};

HandleSeq VarScraper::operator()(const HandleSeq& hs, bool ordered_link)
{
	_paths = variables_paths(ORDERED_LINK, hs);
	HandleSeq result = sorted_free_variables_outgoing(ordered_link, hs);
	_paths.clear();
	return result;
}

HandlePathsMap VarScraper::variables_paths(const Handle& h)
{
	// Base cases
	if (is_variable(h))
		return HandlePathsMap{{h, {{}}}};
	if (h->is_node())
		return HandlePathsMap{};

	// Recursive case
	return variables_paths(h->get_type(), h->getOutgoingSet());
}

HandlePathsMap VarScraper::variables_paths(Type itype, const HandleSeq& hs)
{
	HandlePathsMap paths;
	for (Arity i = 0; i < hs.size(); i++)
	{
		// Append (itype, i) to each path; i is zero if itype is unordered.
		for (const auto& vpp : variables_paths(hs[i]))
		{
			for (auto path : vpp.second)
			{
				path.emplace_back(TypeArityPair{itype, is_unordered(itype) ? 0 : i});
				paths[vpp.first].insert(path);
			}
		}
	}

	return paths;
}

HandleSeq VarScraper::sorted_free_variables(const Handle& body, Context ctx) const
{
	// Base cases
	if (ctx.is_free_variable(body))
		return {body};
	if (body->is_node())
		return {};

	// Recursive cases
	OC_ASSERT(body->is_link());
	ctx.update(body);
	const HandleSeq& outs = body->getOutgoingSet();
	return sorted_free_variables_outgoing(not body->is_unordered_link(), outs, ctx);
}

HandleSeq VarScraper::sorted_free_variables_outgoing(bool ordered,
                                                     const HandleSeq& outgoing,
                                                     const Context& ctx) const
{
	return ordered ? sorted_free_variables_ordered_outgoing(outgoing, ctx)
		: sorted_free_variables_unordered_outgoing(outgoing, ctx);
}

HandleSeq VarScraper::sorted_free_variables_ordered_outgoing(
	const HandleSeq& outgoing, const Context& ctx) const
{
	HandleSeq res;
	for (const Handle& h : outgoing)
	{
		// Recursive call
		HandleSeq fvs = sorted_free_variables(h, ctx);

		// Only retain variables that are not in res
		HandleSeq fvs_n;
		std::copy_if(fvs.begin(), fvs.end(), std::back_inserter(fvs_n),
		             [&res](const Handle& var) {
			             return not content_contains(res, var);
		             });

		// Concatenate
		res.insert(res.end(), fvs_n.begin(), fvs_n.end());
	}
	return res;
}

HandleSeq VarScraper::sorted_free_variables_unordered_outgoing(
	const HandleSeq& outgoing, const Context& ctx) const
{
	return sorted_free_variables_ordered_outgoing(sorted(outgoing), ctx);
}

HandleSeq VarScraper::sorted(HandleSeq outs) const
{
	std::sort(outs.begin(), outs.end(), [&](const Handle& lh, const Handle& rh) {
			return less_than(lh, rh); });
	return outs;
}

bool VarScraper::less_than(const Handle& lh, const Handle& rh) const
{
	// Sort by atom type
	Type lt = lh->get_type();
	Type rt = rh->get_type();
	if (lt != rt)
		return lt < rt;

	// Both atoms are links of the same type
	if (lh->is_link())
	{
		// Sort by arity
		Arity lty = lh->get_arity();
		Arity rty = rh->get_arity();
		if (lty != rty)
			return lty < rty;

		// Both atoms have same arity, sort by outgoings
		const HandleSeq& lout = lh->getOutgoingSet();
		const HandleSeq& rout = rh->getOutgoingSet();
		return lh->is_unordered_link() ? less_than_unordered_outgoing(lout, rout)
			: less_than_ordered_outgoing(lout, rout);
	}

	// None are variables, sort by node content.
	if (not is_variable(lh))
		return lh < rh;

	const auto& lps = _paths.at(lh);
	const auto& rps = _paths.at(rh);
	if (lps != rps)
		return lps < rps;

	// No way to establish the order, they are likely equivalent
	return false;
}

bool VarScraper::less_than_ordered_outgoing(const HandleSeq& lhs,
                                            const HandleSeq& rhs) const
{
	OC_ASSERT(lhs.size() == rhs.size());
	for (std::size_t i = 0; i < lhs.size(); i++)
		if (not is_equivalent(lhs[i], rhs[i]))
			return less_than(lhs[i], rhs[i]);
	return false;
}

bool VarScraper::less_than_unordered_outgoing(const HandleSeq& lhs,
                                              const HandleSeq& rhs) const
{
	return less_than_ordered_outgoing(sorted(lhs), sorted(rhs));
}

bool VarScraper::is_variable(const Handle& h)
{
	Type t = h->get_type();
	return VARIABLE_NODE == t or GLOB_NODE == t;
}

bool VarScraper::is_unordered(Type t)
{
	return nameserver().isA(t, UNORDERED_LINK);
}

bool VarScraper::is_equivalent(const Handle& rh, const Handle& lh) const
{
	//////////////////
   // Base cases   //
   //////////////////

	Type rt = rh->get_type();
	Type lt = lh->get_type();
	if (rt != lt)
		return false;

	// Variable node equivalence is path equality
	if (is_variable(rh))
		return _paths.at(rh) == _paths.at(lh);

	// Constant node equivalence is atom equality
	if (rh->is_node())
		return content_eq(rh, lh);

	// Now rh and lh are links, make sure they have the same arity
	Arity lty = lh->get_arity();
	Arity rty = rh->get_arity();
	if (lty != rty)
		return false;

	///////////////////////
   // Recursive cases   //
   ///////////////////////

	// Now rh and lh are links with same type and arity, make sure
	// their outgoings are equivalent
	const HandleSeq& ro = rh->getOutgoingSet();
	const HandleSeq& lo = lh->getOutgoingSet();
	return is_unordered(rt) ? is_equivalent_unordered_outgoing(ro, lo)
		: is_equivalent_ordered_outgoing(ro, lo);
}

bool VarScraper::is_equivalent_ordered_outgoing(const HandleSeq& rhs,
                                                const HandleSeq& lhs) const
{
	OC_ASSERT(lhs.size() == rhs.size());
	for (std::size_t i = 0; i < lhs.size(); i++)
		if (not is_equivalent(lhs[i], rhs[i]))
			return false;
	return true;
}

bool VarScraper::is_equivalent_unordered_outgoing(const HandleSeq& rhs,
                                                  const HandleSeq& lhs) const
{
	return is_equivalent_ordered_outgoing(sorted(rhs), sorted(lhs));
}

/* ================================================================= */

FreeVariables::FreeVariables(const std::initializer_list<Handle>& variables)
	: varseq(variables), varset(variables)
{
	init_index();
}

void FreeVariables::init_index()
{
	index.clear();
	for (unsigned i = 0; i < varseq.size(); i++)
		index[varseq[i]] = i;
}

bool FreeVariables::is_identical(const FreeVariables& other) const
{
	Arity ary = varseq.size();
	if (ary != other.varseq.size()) return false;
	for (Arity i=0; i< ary; i++)
	{
		if (*((AtomPtr) varseq[i]) != *((AtomPtr) other.varseq[i]))
			return false;
	}
	return true;
}

bool FreeVariables::varset_contains(const Handle& v) const
{
	return varset.end() != varset.find(v);
}

void FreeVariables::find_variables(const Handle& body)
{
	find_variables(HandleSeq{body});
}

void FreeVariables::find_variables(const HandleSeq& oset, bool ordered_link)
{
	varseq = VarScraper()(oset, ordered_link);
	varset = HandleSet(varseq.begin(), varseq.end());
	init_index();
}

void FreeVariables::canonical_sort(const HandleSeq& hs)
{
	// Get free variables
	HandleSet fv = get_free_variables(hs);

	// Ignore free variables in the body that are not in this object.
	HandleSet ignored_vars = set_symmetric_difference(fv, varset);
	Context ctx(Quotation(), ignored_vars, false);

	VarScraper vsc;
	vsc._paths = vsc.variables_paths(ORDERED_LINK, hs);
	varseq = vsc.sorted_free_variables_ordered_outgoing(hs, ctx);

	// Rebuild index to reflect the new order
	init_index();
}

HandleSeq FreeVariables::make_sequence(const HandleMap& varmap) const
{
	HandleSeq arguments;
	for (const Handle& var : varseq)
	{
		HandleMap::const_iterator it = varmap.find(var);
		arguments.push_back(it == varmap.end() ? var : it->second);
	}
	return arguments;
}

void FreeVariables::erase(const Handle& var)
{
	// Remove from varset
	varset.erase(var);

	// Remove from index
	index.erase(var);

	// Remove from varseq and update all arguments in the subsequent
	// index as they have changed.
	auto content_eq_var = [&var](const Handle& h) {
		return content_eq(var, h);
	};
	auto it = std::find_if(varseq.begin(), varseq.end(), content_eq_var);
	if (it != varseq.end()) {
		it = varseq.erase(it);
		for (; it != varseq.end(); ++it)
			index.find(*it)->second--;
	}
}

/* ================================================================= */

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleSeq& args,
                                         bool silent,
                                         bool do_exec) const
{
	return substitute_scoped(term, args, index, do_exec);
}

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleMap& vm,
                                         bool silent,
                                         bool do_exec) const
{
	return substitute_scoped(term, make_sequence(vm), index, do_exec);
}

bool FreeVariables::operator<(const FreeVariables& other) const
{
	return varseq < other.varseq;
}

std::size_t FreeVariables::size() const
{
	return varseq.size();
}

bool FreeVariables::empty() const
{
	return varseq.empty();
}

std::string FreeVariables::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// Varseq
	ss << indent << "varseq:" << std::endl
	   << oc_to_string(varseq, indent + OC_TO_STRING_INDENT) << std::endl;

	// Varset
	ss << indent << "varset:" << std::endl
	   << oc_to_string(varset, indent + OC_TO_STRING_INDENT) << std::endl;

	// index
	ss << indent << "index:" << std::endl
	   << oc_to_string(index, indent + OC_TO_STRING_INDENT);

	return ss.str();
}

/* ================================================================= */

std::string oc_to_string(const TypeArityPair& tap, const std::string& indent)
{
	std::stringstream ss;
	ss << indent
	   << "(" << oc_to_string(tap.first)
	   << "," << tap.second << ")";
	return ss.str();
}

std::string oc_to_string(const Path& path, const std::string& indent)
{
	std::stringstream ss;
	ss << indent;
	for (const auto& tap : path)
		ss << oc_to_string(tap);
	return ss.str();
}

std::string oc_to_string(const PathMultiset& paths, const std::string& indent)
{
	std::stringstream ss;
	size_t i = 0;
	for (const auto& path : paths)
	{
		ss << indent << "path[" << i++ << "]: " << oc_to_string(path);
		if (i < path.size())
			ss << std::endl;
	}
	return ss.str();
}

std::string oc_to_string(const HandlePathsMap& hpsm, const std::string& indent)
{
	std::stringstream ss;
	for (const auto& hpsp : hpsm)
	{
		ss << indent << "paths[" << hpsp.first->to_short_string() << "]:"
		   << std::endl
		   << oc_to_string(hpsp.second, indent + OC_TO_STRING_INDENT);
	}
	return ss.str();
}

std::string oc_to_string(const VarScraper& vsc, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "_paths:" << std::endl
	   << oc_to_string(vsc._paths, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const FreeVariables::IndexMap& imap,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << imap.size();
	for (const auto& el : imap) {
		ss << std::endl << indent << "index[" << el.second << "]: "
		   << el.first->id_to_string();
	}
	return ss.str();
}

std::string oc_to_string(const FreeVariables& var, const std::string& indent)
{
	return var.to_string(indent);
}

} // ~namespace opencog
