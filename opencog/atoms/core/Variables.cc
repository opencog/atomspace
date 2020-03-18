/*
 * atoms/core/Variables.cc
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

#include <opencog/util/Logger.h>
#include <opencog/util/algorithm.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/Context.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>

#include "ScopeLink.h"
#include "VariableList.h"
#include "VariableSet.h"
#include "Variables.h"

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
		// Append (itype, i) to each path, i is zero if itype is
		// unordered.
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
			             return std::find(res.begin(), res.end(), var) == res.end();
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

bool FreeVariables::is_in_varset(const Handle& v) const
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

	// Ignore free variables in body not in the FreeVariables object
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
	auto it = std::find(varseq.begin(), varseq.end(), var);
	if (it != varseq.end()) {
		it = varseq.erase(it);
		for (; it != varseq.end(); ++it)
			index.find(*it)->second--;
	}
}

/* ================================================================= */

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleSeq& args,
                                         bool silent) const
{
	return substitute_scoped(term, args, silent, index);
}

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleMap& vm,
                                         bool silent) const
{
	return substitute_scoped(term, make_sequence(vm), silent, index);
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

	// index
	ss << indent << "index:" << std::endl
	   << oc_to_string(index, indent + OC_TO_STRING_INDENT);

	return ss.str();
}

/// Perform beta-reduction on the term.  This is more-or-less a purely
/// syntactic beta-reduction, except for two semantic parts:
///
/// 1. The semantics of QuoteLink, UnquoteLink is honoured, so that
///    quoted variables are not reduced.
///
/// 2. The semantics of scoping (alpha-conversion) is honored, so that
///    any scoped variables with the same name as the free variables
///    are alpha-hidden, possibly alpha-converted if the substituting
///    values are variables of the same name.
Handle FreeVariables::substitute_scoped(Handle term,
                                        const HandleSeq& args,
                                        bool silent,
                                        const IndexMap& index_map,
                                        Quotation quotation) const
{
	bool unquoted = quotation.is_unquoted();

	// If we are not in a quote context, and `term` is a variable,
	// then just return the corresponding argument.
	if (unquoted)
	{
		IndexMap::const_iterator idx = index_map.find(term);
		if (idx != index_map.end())
			return args.at(idx->second);
	}

	// If its a node, and its not a variable, then it is a constant,
	// and just return that.
	if (not term->is_link()) return term;

	Type ty = term->get_type();

	// Update for subsequent recursive calls of substitute_scoped
	quotation.update(ty);

	// If the term is a scope the index map might change, to avoid copy
	// we either point to the original map, or the new one.
	const IndexMap* index_map_ptr = &index_map;
	IndexMap hidden_map;

	if (unquoted and nameserver().isA(ty, SCOPE_LINK))
	{
		// Perform alpha-conversion duck-n-cover.

		// If a substituting value is equal to a variable of that scope,
		// then alpha-convert the scope to avoid variable name
		// collision. Loop in the rare case the new names collide.
		while (must_alpha_convert(term, args))
			term = ScopeLinkCast(term)->alpha_convert();

		// Hide any variables of the scope that are to be substituted,
		// that is remove them from the index.
		if (must_alpha_hide(term, index_map))
		{
			hidden_map = alpha_hide(term, index_map);

			// If the hidden map is empty, then there is no more
			// substitution to be done.
			if (hidden_map.empty())
				return term;

			// Otherwise the new map must be passed down below
			index_map_ptr = &hidden_map;
		}
	}

	// Recursively fill out the subtrees.
	HandleSeq oset;
	bool changed = false;
	for (const Handle& h : term->getOutgoingSet())
	{
		// GlobNodes are matched with a list of one or more arguments.
		// Those arguments need to be in-lined, stripping off the list
		// that wraps them up.  See MapLinkUTest for examples.
		if (GLOB_NODE == h->get_type())
		{
			Handle glst(substitute_scoped(h, args, silent, *index_map_ptr, quotation));
			changed = true;

			// Also unwrap any ListLinks that were inserted by
			// `wrap_glob_with_list()` in RewriteLink.cc
			if (glst->get_type() == LIST_LINK)
				for (const Handle &gl : glst->getOutgoingSet())
					oset.emplace_back(gl);
			else
				oset.emplace_back(glst);
		}
		else
		{
			Handle sub(substitute_scoped(h, args, silent, *index_map_ptr, quotation));
			if (sub != h) changed = true;
			oset.emplace_back(sub);
		}
	}

	// Return the original atom, if it was not modified.
	if (not changed) return term;
	return createLink(std::move(oset), term->get_type());
}

bool FreeVariables::must_alpha_convert(const Handle& scope,
                                       const HandleSeq& args) const
{
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& value : args)
		if (vars.find(value) != vars.end())
			return true;
	return false;
}

// Evaluate whether any variable must be hidden/removed from the
// index_map.
bool FreeVariables::must_alpha_hide(const Handle& scope,
                                    const IndexMap& index_map) const
{
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& v : vars)
		if (index_map.find(v) != index_map.end())
			return true;
	return false;
}

/// Remove the variables from the given index map that are present in
/// the given the variables of a scope, as well as non variables
FreeVariables::IndexMap FreeVariables::alpha_hide(const Handle& scope,
                                                  const IndexMap& index_map) const
{
	// Make a copy... this is what's computationally expensive.
	IndexMap hidden_map = index_map;

	// Remove the alpha-hidden variables.
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& v : vars)
	{
		IndexMap::const_iterator idx = hidden_map.find(v);
		if (idx != hidden_map.end())
		{
			hidden_map.erase(idx);
		}
	}

	// Also remove everything that is not a variable.
	// The map will, in general, contain terms that
	// contain alpha-hidden variables; those also have
	// to go, or they will mess up the substitution.
	for (auto it = hidden_map.begin(); it != hidden_map.end();)
	{
		Type tt = it->first->get_type();
		if (tt != VARIABLE_NODE and tt != GLOB_NODE)
		{
			it = hidden_map.erase(it);
		}
		else
		{
			++it;
		}
	}

	return hidden_map;
}

/* ================================================================= */

Variables::Variables(bool ordered)
	: _ordered(ordered)
{
}

Variables::Variables(const Handle& vardecl, bool ordered)
	: _ordered(ordered)
{
	validate_vardecl(vardecl);
	init_index();
}

Variables::Variables(const HandleSeq& vardecls, bool ordered)
	: _ordered(ordered)
{
	validate_vardecl(vardecls);
	init_index();
}

/* ================================================================= */
/**
 * Extract the variable type(s) from a TypedVariableLink
 *
 * The call is expecting htypelink to point to one of the two
 * following structures:
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeNode  "ConceptNode"
 *
 * or
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeChoice
 *          TypeNode  "ConceptNode"
 *          TypeNode  "NumberNode"
 *          TypeNode  "WordNode"
 *
 * or possibly types that are SignatureLink's or FuyzzyLink's or
 * polymorphic combinations thereof: e.g. the following is valid:
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeChoice
 *          TypeNode  "ConceptNode"
 *          SignatureLink
 *              InheritanceLink
 *                 PredicateNode "foobar"
 *                 TypeNode  "ListLink"
 *          FuzzyLink
 *              InheritanceLink
 *                 ConceptNode "animal"
 *                 ConceptNode "tiger"
 *
 * In either case, the variable itself is appended to "vset",
 * and the list of allowed types are associated with the variable
 * via the map "typemap".
 */
void Variables::get_vartype(const Handle& htypelink)
{
	const HandleSeq& oset = htypelink->getOutgoingSet();
	if (2 != oset.size())
	{
		throw InvalidParamException(TRACE_INFO,
			"TypedVariableLink has wrong size, got %lu", oset.size());
	}

	Handle varname(oset[0]);
	Handle vartype(oset[1]);

	Type nt = varname->get_type();
	Type t = vartype->get_type();

	// Specifying how many atoms can be matched to a GlobNode, if any
	HandleSeq intervals;

	// If its a defined type, unbundle it.
	if (DEFINED_TYPE_NODE == t)
	{
		vartype = DefineLink::get_definition(vartype);
		t = vartype->get_type();
	}

	// For GlobNode, we can specify either the interval or the type, e.g.
	//
	// TypedVariableLink
	//   GlobNode  "$foo"
	//   IntervalLink
	//     NumberNode  2
	//     NumberNode  3
	//
	// or both under a TypeSetLink, e.g.
	//
	// TypedVariableLink
	//   GlobNode  "$foo"
	//   TypeSetLink
	//     IntervalLink
	//       NumberNode  2
	//       NumberNode  3
	//     TypeNode "ConceptNode"
	if (GLOB_NODE == nt and TYPE_SET_LINK == t)
	{
		for (const Handle& h : vartype->getOutgoingSet())
		{
			Type th = h->get_type();

			if (INTERVAL_LINK == th)
				intervals = h->getOutgoingSet();

			else if (TYPE_NODE == th or
			         TYPE_INH_NODE == th or
			         TYPE_CO_INH_NODE == th)
			{
				vartype = h;
				t = th;
			}

			else throw SyntaxException(TRACE_INFO,
				"Unexpected contents in TypedSetLink\n"
				"Expected IntervalLink and TypeNode, got %s",
				h->to_string().c_str());
		}
	}

	// The vartype is either a single type name, or a list of typenames.
	if (TYPE_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		if (vt != ATOM)  // Atom type is same as untyped.
		{
			TypeSet ts = {vt};
			_simple_typemap.insert({varname, ts});
		}
	}
	else if (TYPE_INH_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		TypeSet ts = nameserver().getChildrenRecursive(vt);
		_simple_typemap.insert({varname, ts});
	}
	else if (TYPE_CO_INH_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		TypeSet ts = nameserver().getParentsRecursive(vt);
		_simple_typemap.insert({varname, ts});
	}
	else if (TYPE_CHOICE == t)
	{
		TypeSet typeset;
		HandleSet deepset;
		HandleSet fuzzset;

		const HandleSeq& tset = vartype->getOutgoingSet();
		size_t tss = tset.size();
		for (size_t i=0; i<tss; i++)
		{
			Handle ht(tset[i]);
			Type var_type = ht->get_type();
			if (TYPE_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt) typeset.insert(vt);
			}
			else if (TYPE_INH_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt)
				{
					TypeSet ts = nameserver().getChildrenRecursive(vt);
					typeset.insert(ts.begin(), ts.end());
				}
			}
			else if (TYPE_CO_INH_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt)
				{
					TypeSet ts = nameserver().getParentsRecursive(vt);
					typeset.insert(ts.begin(), ts.end());
				}
			}
			else if (SIGNATURE_LINK == var_type)
			{
				const HandleSeq& sig = ht->getOutgoingSet();
				if (1 != sig.size())
					throw SyntaxException(TRACE_INFO,
						"Unexpected contents in SignatureLink\n"
						"Expected arity==1, got %s", vartype->to_string().c_str());

				deepset.insert(ht);
			}
			else if (FUZZY_LINK == var_type)
			{
				const HandleSeq& fuz = ht->getOutgoingSet();
				if (1 != fuz.size())
					throw SyntaxException(TRACE_INFO,
						"Unexpected contents in FuzzyLink\n"
						"Expected arity==1, got %s", vartype->to_string().c_str());

				fuzzset.insert(ht);
			}
			else
			{
				throw InvalidParamException(TRACE_INFO,
					"VariableChoice has unexpected content:\n"
					"Expected TypeNode, got %s",
					    nameserver().getTypeName(ht->get_type()).c_str());
			}
		}

		if (0 < typeset.size())
			_simple_typemap.insert({varname, typeset});
		if (0 < deepset.size())
			_deep_typemap.insert({varname, deepset});
		if (0 < fuzzset.size())
			_fuzzy_typemap.insert({varname, fuzzset});

		// An empty disjunction corresponds to a bottom type.
		if (tset.empty())
			_simple_typemap.insert({varname, {NOTYPE}});

		// Check for (TypeChoice (TypCoInh 'Atom)) which is also bottom.
		if (1 == tset.size() and TYPE_CO_INH_NODE == tset[0]->get_type())
		{
			Type vt = TypeNodeCast(tset[0])->get_kind();
			if (ATOM == vt)
				_simple_typemap.insert({varname, {NOTYPE}});
		}
	}
	else if (SIGNATURE_LINK == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		if (1 != tset.size())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in SignatureLink\n"
				"Expected arity==1, got %s", vartype->to_string().c_str());

		HandleSet ts;
		ts.insert(vartype);
		_deep_typemap.insert({varname, ts});
	}
	else if (FUZZY_LINK == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		if (1 != tset.size())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in FuzzyLink\n"
				"Expected arity==1, got %s", vartype->to_string().c_str());

		HandleSet ts;
		ts.insert(vartype);
		_fuzzy_typemap.insert({varname, ts});
	}
	else if (VARIABLE_NODE == t)
	{
		// This occurs when the variable type is a variable to be
		// matched by the pattern matcher. There's nothing to do
		// except not throwing an exception.
	}
	else if (GLOB_NODE == nt and INTERVAL_LINK == t)
	{
		intervals = vartype->getOutgoingSet();
	}
	else
	{
		throw SyntaxException(TRACE_INFO,
			"Unexpected contents in TypedVariableLink\n"
			"Expected type specifier (e.g. TypeNode, TypeChoice, etc.), got %s",
			nameserver().getTypeName(t).c_str());
	}

	if (0 < intervals.size())
	{
		long lb = std::lround(NumberNodeCast(intervals[0])->get_value());
		long ub = std::lround(NumberNodeCast(intervals[1])->get_value());
		if (lb < 0) lb = 0;
		if (ub < 0) ub = SIZE_MAX;

		_glob_intervalmap.insert({varname, std::make_pair(lb, ub)});
	}

	varset.insert(varname);
	varseq.emplace_back(varname);
}

/* ================================================================= */
/**
 * Validate variable declarations for syntax correctness.
 *
 * This will check to make sure that a set of variable declarations are
 * of a reasonable form. Thus, for example, a structure similar to the
 * below is expected.
 *
 *       VariableList
 *          VariableNode "$var0"
 *          VariableNode "$var1"
 *          TypedVariableLink
 *             VariableNode "$var2"
 *             TypeNode  "ConceptNode"
 *          TypedVariableLink
 *             VariableNode "$var3"
 *             TypeChoice
 *                 TypeNode  "PredicateNode"
 *                 TypeNode  "GroundedPredicateNode"
 *
 * As a side-effect, the variables and type restrictions are unpacked.
 */
void Variables::validate_vardecl(const Handle& hdecls)
{
	// If no variable declaration then create the empty variables
	if (not hdecls)
		return;

	// Expecting the declaration list to be either a single
	// variable, a list or a set of variable declarations.
	Type tdecls = hdecls->get_type();

	// Order matters only if it is a list of variables
	_ordered = VARIABLE_LIST == tdecls;

	if (VARIABLE_NODE == tdecls or GLOB_NODE == tdecls)
	{
		varset.insert(hdecls);
		varseq.emplace_back(hdecls);
	}
	else if (TYPED_VARIABLE_LINK == tdecls)
	{
		get_vartype(hdecls);
	}
	else if (VARIABLE_LIST == tdecls or VARIABLE_SET == tdecls)
	{
		// Extract the list of set of variables and make sure its as
		// expected.
		const HandleSeq& dset = hdecls->getOutgoingSet();
		validate_vardecl(dset);
	}
	else if (UNQUOTE_LINK == tdecls)
	{
		// This indicates that the variable declaration is not in normal
		// form (i.e. requires beta-reductions to be fully formed), thus
		// variables inference is aborted for now.
		return;
	}
	else
	{
		throw InvalidParamException(TRACE_INFO,
			"Expected a VariableList holding variable declarations");
	}
}

bool Variables::is_well_typed() const
{
	for (const auto& vt : _simple_typemap)
		if (not opencog::is_well_typed(vt.second))
			return false;
	return true;
}

/* ================================================================= */

/// Return true if the other Variables struct is equal to this one,
/// up to alpha-conversion. That is, same number of variables, same
/// type restrictions, but possibly different variable names.
///
/// This should give exactly the same answer as performing the tests
///    this->is_type(other->varseq) and other->is_type(this->varseq)
/// That is, the variables in this instance should have the same type
/// restrictions as the variables in the other class.
bool Variables::is_equal(const Variables& other) const
{
	size_t sz = varseq.size();
	if (other.varseq.size() != sz) return false;

	if (other._ordered != _ordered) return false;

	// Side-by-side comparison
	for (size_t i = 0; i < sz; i++)
	{
		if (not is_equal(other, i))
			return false;
	}
	return true;
}

bool Variables::is_equal(const Variables& other, size_t index) const
{
	const Handle& vme(varseq[index]);
	const Handle& voth(other.varseq[index]);

	// If one is a GlobNode, and the other a VariableNode,
	// then its a mismatch.
	if (vme->get_type() != voth->get_type()) return false;

	// If typed, types must match.
	auto sime = _simple_typemap.find(vme);
	auto soth = other._simple_typemap.find(voth);
	if (sime == _simple_typemap.end() and
	    soth != other._simple_typemap.end()) return false;

	if (sime != _simple_typemap.end())
	{
		if (soth == other._simple_typemap.end()) return false;
		if (sime->second != soth->second) return false;
	}

	// If typed, types must match.
	auto dime = _deep_typemap.find(vme);
	auto doth = other._deep_typemap.find(voth);
	if (dime == _deep_typemap.end() and
	    doth != other._deep_typemap.end()) return false;

	if (dime != _deep_typemap.end())
	{
		if (doth == other._deep_typemap.end()) return false;
		if (dime->second != doth->second) return false;
	}

	// XXX TODO fuzzy?

	// If intervals specified, intervals must match.
	auto iime = _glob_intervalmap.find(vme);
	auto ioth = other._glob_intervalmap.find(voth);
	if (iime == _glob_intervalmap.end() and
	    ioth != other._glob_intervalmap.end()) return false;

	if (iime != _glob_intervalmap.end())
	{
		if (ioth == other._glob_intervalmap.end()) return false;
		if (iime->second != ioth->second) return false;
	}

	// If we got to here, everything must be OK.
	return true;
}

/* ================================================================= */

/// Return true if the variable `othervar` in `other` is
/// alpha-convertible to the variable `var` in this. That is,
/// return true if they are the same variable, differing only
/// in name.

bool Variables::is_alpha_convertible(const Handle& var,
                                     const Handle& othervar,
                                     const Variables& other,
                                     bool check_type) const
{
	IndexMap::const_iterator idx = other.index.find(othervar);
	return other.index.end() != idx
		and varseq.at(idx->second) == var
		and (not check_type or is_equal(other, idx->second));
}

/* ================================================================= */
/**
 * Simple type checker.
 *
 * Returns true/false if the indicated handle is of the type that
 * we have memoized.  If this typelist contains more than one type in
 * it, then clearly, there is a mismatch.  If there are no type
 * restrictions, then it is trivially a match.  Otherwise, there must
 * be a TypeChoice, and so the handle must be one of the types in the
 * TypeChoice.
 */
bool Variables::is_type(const Handle& h) const
{
	// The arity must be one for there to be a match.
	if (1 != varset.size()) return false;

	return is_type(varseq[0], h);
}

/**
 * Type checker.
 *
 * Returns true/false if we are holding the variable `var`, and if
 * the `val` satisfies the type restrictions that apply to `var`.
 */
bool Variables::is_type(const Handle& var, const Handle& val) const
{
	if (varset.end() == varset.find(var)) return false;

	VariableTypeMap::const_iterator tit = _simple_typemap.find(var);
	VariableDeepTypeMap::const_iterator dit = _deep_typemap.find(var);
	VariableDeepTypeMap::const_iterator fit = _fuzzy_typemap.find(var);

	const Arity num_args = val->get_type() != LIST_LINK ? 1 : val->get_arity();

	// If one is allowed in interval then there are two alternatives.
	// one: val must satisfy type restriction.
	// two: val must be list_link and its unique outgoing satisfies
	//      type restriction.
	if (is_lower_bound(var, 1) and is_upper_bound(var, 1)
	    and is_type(tit, dit, fit, val))
		return true;
	else if (val->get_type() != LIST_LINK or
	         not is_lower_bound(var, num_args) or
	         not is_upper_bound(var, num_args))
		// If the number of arguments is out of the allowed interval
		// of the variable/glob or val is not List_link, return false.
		return false;

	// Every outgoing atom in list must satisfy type restriction of var.
	for (size_t i = 0; i < num_args; i++)
		if (!is_type(tit, dit, fit, val->getOutgoingAtom(i)))
			return false;

	return true;
}

bool Variables::is_type(VariableTypeMap::const_iterator tit,
                        VariableDeepTypeMap::const_iterator dit,
                        VariableDeepTypeMap::const_iterator fit,
                        const Handle& val) const
{
	bool ret = true;

	// Simple type restrictions?
	if (_simple_typemap.end() != tit)
	{
		const TypeSet &tchoice = tit->second;
		Type htype = val->get_type();
		TypeSet::const_iterator allow = tchoice.find(htype);

		// If the argument has the simple type, then we are good to go;
		// we are done.  Else, fall through, and see if one of the
		// others accept the match.
		if (allow != tchoice.end()) return true;
		ret = false;
	}

	// Deep type restrictions?
	if (_deep_typemap.end() != dit)
	{
		const HandleSet &sigset = dit->second;
		for (const Handle& sig : sigset)
		{
			if (value_is_type(sig, val)) return true;
		}
		ret = false;
	}

	// Fuzzy deep type restrictions?
	if (_fuzzy_typemap.end() != fit)
	{
		// const HandleSet &fuzzset = dit->second;
		throw RuntimeException(TRACE_INFO,
		                       "Not implemented! TODO XXX FIXME");
		ret = false;
	}

	// There appear to be no type restrictions...
	return ret;
}

/* ================================================================= */
/**
 * Simple type checker.
 *
 * Returns true/false if the indicated handles are of the type that
 * we have memoized.
 *
 * XXX TODO this does not currently handle type equations, as outlined
 * on the wiki; We would need the general pattern matcher to do type
 * checking, in that situation.
 */
bool Variables::is_type(const HandleSeq& hseq) const
{
	// The arities must be equal for there to be a match.
	size_t len = hseq.size();
	if (varset.size() != len) return false;

	// Check the type restrictions.
	for (size_t i=0; i<len; i++)
	{
		if (not is_type(varseq[i], hseq[i])) return false;
	}
	return true;
}

/**
 * Return true if we contain just a single variable, and this one
 * variable is of type gtype (or is untyped). A typical use is that
 * gtype==VARIABLE_LIST.
 */
bool Variables::is_type(Type gtype) const
{
	if (1 != varseq.size()) return false;

	// Are there any type restrictions?
	const Handle& var = varseq[0];
	VariableTypeMap::const_iterator tit = _simple_typemap.find(var);
	if (_simple_typemap.end() == tit) return true;
	const TypeSet &tchoice = tit->second;

	// There are type restrictions; do they match?
	TypeSet::const_iterator allow = tchoice.find(gtype);
	if (allow != tchoice.end()) return true;
	return false;
}

/**
 * Interval checker.
 *
 * Returns true/false if the glob satisfies the lower bound
 * interval restriction.
 */
bool Variables::is_lower_bound(const Handle& glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);
	return (n >= intervals.first);
}

/**
 * Interval checker.
 *
 * Returns true/false if the glob satisfies the upper bound
 * interval restriction.
 */
bool Variables::is_upper_bound(const Handle &glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);
	return (n <= intervals.second or intervals.second < 0);
}

static const GlobInterval& default_interval(Type t)
{
	static const GlobInterval var_def_interval =
			GlobInterval(1, 1);
	static const GlobInterval glob_def_interval =
			GlobInterval(1, SIZE_MAX);
	return t == GLOB_NODE ? glob_def_interval :
			 var_def_interval;
}

const GlobInterval& Variables::get_interval(const Handle& var) const
{
	const auto interval = _glob_intervalmap.find(var);

	if (interval == _glob_intervalmap.end())
		return default_interval(var->get_type());

	return interval->second;
}

/* ================================================================= */
/**
 * Substitute the given arguments for the variables occuring in a tree.
 * That is, perform beta-reduction.  This is a lot like applying the
 * function `func` to the argument list `args`, except that no actual
 * evaluation is performed; only substitution.
 *
 * The resulting tree is NOT placed into any atomspace. If you want
 * that, you must do it yourself.  If you want evaluation or execution
 * to happen during substitution, then use either the EvaluationLink,
 * the ExecutionOutputLink, or the Instantiator.
 *
 * So, for example, if this VariableList contains:
 *
 *   VariableList
 *       VariableNode $a
 *       VariableNode $b
 *
 * and `func` is the template:
 *
 *   EvaluationLink
 *      PredicateNode "something"
 *      ListLink
 *         VariableNode $b      ; note the reversed order
 *         VariableNode $a
 *
 * and the `args` is a list
 *
 *      ConceptNode "one"
 *      NumberNode 2.0000
 *
 * then the returned result will be
 *
 *   EvaluationLink
 *      PredicateNode "something"
 *      ListLink
 *          NumberNode 2.0000    ; note reversed order here, also
 *          ConceptNode "one"
 *
 * That is, the arguments `one` and `2.0` were substituted for `$a` and `$b`.
 *
 * The `func` can be, for example, a single variable name(!) In this
 * case, the corresponding `arg` is returned. So, for example, if the
 * `func` was simply `$b`, then `2.0` would be returned.
 *
 * Type checking is performed before substitution; if the args fail to
 * satisfy the type constraints, an exception is thrown. If `silent`
 * is true, then the exception is non-printing, and so this method can
 * be used for "filtering", i.e. for automatically rejecting arguments
 * that fail the type check.
 *
 * The substitution is almost purely syntactic... with one exception:
 * the semantics of QuoteLink and UnquoteLink are honoured.  That is,
 * no variable reduction is performed into any part of the tree which
 * is quoted. (QuoteLink is like scheme's quasi-quote, in that each
 * UnquoteLink undoes one level of quotation.)
 *
 * Again, only a substitution is performed, there is no evaluation.
 * Note also that the resulting tree is NOT placed into any atomspace!
 */
Handle Variables::substitute(const Handle& func,
                             const HandleSeq& args,
                             bool silent) const
{
	if (args.size() != varseq.size())
		throw SyntaxException(TRACE_INFO,
			"Incorrect number of arguments specified, expecting %lu got %lu",
			varseq.size(), args.size());

	// XXX TODO type-checking could be lazy; if the function is not
	// actually using one of the args, it's type should not be checked.
	// Viz., one of the arguments might be undefined, and that's OK,
	// if that argument is never actually used.  Fixing this requires a
	// cut-n-paste of the substitute_nocheck code. I'm too lazy to do
	// this ... no one wants this whizzy-ness just right yet.
	if (not is_type(args))
	{
		if (silent) throw TypeCheckException();
		throw SyntaxException(TRACE_INFO,
			"Arguments fail to match variable declarations");
	}

	return substitute_nocheck(func, args);
}

Handle Variables::substitute(const Handle& func,
                             const HandleMap& map,
                             bool silent) const
{
	return substitute(func, make_sequence(map), silent);
}

/* ================================================================= */
/**
 * Extend a set of variables.
 *
 * That is, merge the given variables into this set.
 *
 * If a variable is both in *this and vset then its type intersection
 * is assigned to it.
 */
void Variables::extend(const Variables& vset)
{
	for (const Handle& h : vset.varseq)
	{
		auto index_it = index.find(h);
		if (index_it != index.end())
		{
			// Merge the two typemaps, if needed.
			auto typemap_it = vset._simple_typemap.find(h);
			if (typemap_it != vset._simple_typemap.end())
			{
				const TypeSet& tms = typemap_it->second;
				auto tti = _simple_typemap.find(h);
				if(tti != _simple_typemap.end())
					tti->second = set_intersection(tti->second, tms);
				else
					_simple_typemap.insert({h, tms});
			}
		}
		else
		{
			// Found a new variable! Insert it.
			index.insert({h, varseq.size()});
			varseq.emplace_back(h);
			varset.insert(h);

			// Install the type constraints, as well.
			auto typemap_it = vset._simple_typemap.find(h);
			if (typemap_it != vset._simple_typemap.end())
			{
				_simple_typemap.insert({h, typemap_it->second});
			}
		}
		// extend _glob_interval_map
		extend_interval(h, vset);
	}

	// If either this or the other are ordered then the result is ordered
	_ordered = _ordered or vset._ordered;
}

inline GlobInterval interval_intersection(const GlobInterval &lhs,
                                          const GlobInterval &rhs)
{
	const auto lb = std::max(lhs.first, rhs.first);
	const auto ub = std::min(lhs.second, rhs.second);
	return lb > ub ? GlobInterval{0, 0} : GlobInterval{lb, ub};
}

void Variables::extend_interval(const Handle &h, const Variables &vset)
{
	auto it = _glob_intervalmap.find(h);
	auto is_in_gim = it != _glob_intervalmap.end();
	const auto intersection = not is_in_gim ? vset.get_interval(h) :
			interval_intersection(vset.get_interval(h), get_interval(h));
	if (intersection != default_interval(h->get_type())) {
		if (is_in_gim) it->second = intersection;
		else _glob_intervalmap.insert({h, intersection});
	}
}

void Variables::erase(const Handle& var)
{
	// Remove from the type maps
	_simple_typemap.erase(var);
	_deep_typemap.erase(var);
	_fuzzy_typemap.erase(var);

	// Remove from the interval map
	_glob_intervalmap.erase(var);

	// Remove FreeVariables
	FreeVariables::erase(var);
}

bool Variables::operator==(const Variables& other) const
{
	return is_equal(other);
}

bool Variables::operator<(const Variables& other) const
{
	return (FreeVariables::operator<(other))
		or ((_simple_typemap == other._simple_typemap
		     and _deep_typemap < other._deep_typemap)
		    or (_deep_typemap == other._deep_typemap
		        and _fuzzy_typemap < other._fuzzy_typemap));
}

/// Look up the type declaration for `var`, but create the actual
/// declaration for `alt`.  This is an alpha-renaming.
Handle Variables::get_type_decl(const Handle& var, const Handle& alt) const
{
	HandleSeq types;
	// Simple type info
	const auto& sit = _simple_typemap.find(var);
	if (sit != _simple_typemap.end())
	{
		for (Type t : sit->second)
			types.push_back(Handle(createTypeNode(t)));
	}

	auto dit = _deep_typemap.find(var);
	if (dit != _deep_typemap.end())
	{
		OC_ASSERT(false, "TODO: support deep type info");
	}

	auto fit = _fuzzy_typemap.find(var);
	if (fit != _fuzzy_typemap.end())
	{
		OC_ASSERT(false, "TODO: support fuzzy type info");
	}

	// Check if ill-typed a.k.a invalid type intersection.
	if(types.empty() and sit != _simple_typemap.end())
	{
		const Handle ill_type = createLink(TYPE_CHOICE);
		return createLink(TYPED_VARIABLE_LINK, alt, ill_type);
	}

	const auto interval = get_interval(var);
	if (interval != default_interval(var->get_type())) {
		Handle il = createLink(INTERVAL_LINK,
		                       Handle(createNumberNode(interval.first)),
		                       Handle(createNumberNode(interval.second)));

		if (types.empty())
			return createLink(TYPED_VARIABLE_LINK, alt, il);

		HandleSeq tcs;
		for (Handle tn : types)
			tcs.push_back(createLink(TYPE_SET_LINK, il, tn));
		return tcs.size() == 1 ?
		       createLink(TYPED_VARIABLE_LINK, alt, tcs[0]) :
		       createLink(TYPED_VARIABLE_LINK, alt,
		                  createLink(tcs, TYPE_CHOICE));
	}
	// No/Default interval found
	if (!types.empty()) {
		Handle types_h = types.size() == 1 ?
		                 types[0] :
		                 createLink(std::move(types), TYPE_CHOICE);
		return createLink(TYPED_VARIABLE_LINK, alt, types_h);
	}
	// No type info
	return alt;
}

Handle Variables::get_vardecl() const
{
	HandleSeq vardecls;
	for (const Handle& var : varseq)
		vardecls.emplace_back(get_type_decl(var, var));
	if (vardecls.size() == 1)
		return vardecls[0];

	if (_ordered)
		return Handle(createVariableList(std::move(vardecls)));

	return Handle(createVariableSet(std::move(vardecls)));
}

void Variables::validate_vardecl(const HandleSeq& oset)
{
	for (const Handle& h: oset)
	{
		Type t = h->get_type();
		if (VARIABLE_NODE == t or GLOB_NODE == t)
		{
			varset.insert(h);
			varseq.emplace_back(h);
		}
		else if (TYPED_VARIABLE_LINK == t)
		{
			get_vartype(h);
		}
		else if (ANCHOR_NODE == t)
		{
			_anchor = h;
		}
		else
		{
			throw InvalidParamException(TRACE_INFO,
				"Expected a Variable or TypedVariable or Anchor, got: %s"
				"\nVariableList is %s",
					nameserver().getTypeName(t).c_str(),
					to_string().c_str());
		}
	}
}

void Variables::find_variables(const Handle& body)
{
	FreeVariables::find_variables(body);
	_ordered = false;
}

void Variables::find_variables(const HandleSeq& oset, bool ordered_link)
{
	FreeVariables::find_variables(oset, ordered_link);
	_ordered = false;
}

std::string Variables::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// FreeVariables
	ss << FreeVariables::to_string(indent) << std::endl;

	// Whether it is ordered
	ss << indent << "_ordered = " << _ordered << std::endl;

	// Simple typemap
	std::string indent_p = indent + OC_TO_STRING_INDENT;
	ss << indent << "_simple_typemap:" << std::endl
	   << oc_to_string(_simple_typemap, indent_p) << std::endl;

	// Glob interval map
	ss << indent << "_glob_intervalmap:" << std::endl
	   << oc_to_string(_glob_intervalmap, indent_p);

	return ss.str();
}

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

std::string oc_to_string(const VariableTypeMap& vtm, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << vtm.size();
	unsigned i = 0;
	for (const auto& v : vtm)
	{
		ss << std::endl << indent << "variable[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "types[" << i << "]:";
		for (auto& t : v.second)
			ss << " " << nameserver().getTypeName(t);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const GlobIntervalMap& gim, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << gim.size();
	unsigned i = 0;
	for (const auto& v : gim)
	{
		ss << std::endl << indent << "glob[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "interval[" << i << "]: ";
		double lo = v.second.first;
		double up = v.second.second;
		ss << ((0 <= lo and std::isfinite(lo)) ? "[" : "(") << lo << ", "
		   << up << ((0 <= up and std::isfinite(up)) ? "]" : ")");
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const FreeVariables& var, const std::string& indent)
{
	return var.to_string(indent);
}

std::string oc_to_string(const Variables& var, const std::string& indent)
{
	return var.to_string(indent);
}

} // ~namespace opencog
