/*
 * Variables.cc
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
#include "Variables.h"

namespace opencog {

struct VarScraper
{
	/**
	 * Mapping between variables and number of occurrences. This allows
	 * to take into account the semantics of the variables, as to be
	 * able to sort unordered links like
	 *
	 * (And
	 *   (Inheritance (Variable "$X") (Variable "$Y"))
	 *   (Inheritance (Variable "$Z") (Variable "$Z")))
	 *
	 * by their semantics (Z occurs twice, thus is sorted after X and
	 * Y) as opposed to solely variable names, which is not robust to
	 * alpha-conversion.
	 */
	HandleUCounter _fvc;

	/**
	 * Find variable declaration in such order that is consistent with
	 * alpha-equivalence.
	 */
	void find_vars(HandleSeq& varseq, HandleSet& varset,
	               const HandleSeq& outs, bool ordered);

	/**
	 * Return a mapping between free variables and the number of times
	 * they occur in the given body.
	 *
	 * Such a mapping is then passed to the various ordering functions
	 * so that semantics is considered prior to variable names.
	 *
	 * Note: context is passed by copy for implementation convenience.
	 */
	static HandleUCounter free_variables_counter(const Handle& h,
	                                             Context ctx=Context());
	static HandleUCounter free_variables_counter(const HandleSeq& hs,
	                                             const Context& ctx=Context());

	/**
	 * Find all free variables in body and return them in a canonical
	 * order, so that if the variables were renamed and rerun this
	 * function we would obtain the same order, or an order that does
	 * not affect the semantics.
	 *
	 * Note: context is passed by copy for implementation convenience.
	 */
	HandleSeq free_variables(const Handle& body, Context ctx=Context()) const;

	/**
	 * Like above but operates on the outgoing set of an ordered
	 * (resp. unordered) link.
	 */
	HandleSeq free_variables_ordered(const HandleSeq& outs,
	                                 const Context& ctx=Context()) const;
	HandleSeq free_variables_unordered(const HandleSeq& outs,
	                                   const Context& ctx=Context()) const;

	/**
	 * Return a sorted outgoing set according to a canonical order.
	 *
	 * Note: argument is passed by copy for implementation convenience.
	 */
	HandleSeq sorted(HandleSeq hs, const Context& ctx=Context()) const;

	/**
	 * Canonical order, so that variables get sorted according to their
	 * semantics first, and only then their names. This allows to have
	 * the order be consistent with alpha-equivalent.
	 */
	bool less_than(const Handle& lh, const Handle& rh, const Context& ctx) const;

	/**
	 * Like less_than but over ordered (resp. unordered) outgoings.
	 */
	bool less_than_ordered(const HandleSeq& lhs, const HandleSeq& rhs,
	                       const Context& ctx) const;
	bool less_than_unordered(const HandleSeq& lhs, const HandleSeq& rhs,
	                         const Context& ctx) const;

	/**
	 * Return true iff h is a Variable or Glob node.
	 */
	static bool is_variable(const Handle& h);

	/**
	 * Return true iff is an ordered link or subtype thereof.
	 */
	static bool is_ordered(const Handle& h);
};

void VarScraper::find_vars(HandleSeq& varseq, HandleSet& varset,
                           const HandleSeq& hs, bool ordered)
{
	_fvc = free_variables_counter(hs);
	varseq = ordered ? free_variables_ordered(hs) : free_variables_unordered(hs);
	varset = HandleSet(varseq.begin(), varseq.end());
}

HandleUCounter VarScraper::free_variables_counter(const Handle& h,
                                                  Context ctx)
{
	// Base cases
	if (ctx.is_free_variable(h))
		return HandleUCounter{{h, 1}};
	if (h->is_node())
		return HandleUCounter{};

	// Recursive case
	OC_ASSERT(h->is_link());
	ctx.update(h);
	return free_variables_counter(h->getOutgoingSet(), ctx);
}

HandleUCounter VarScraper::free_variables_counter(const HandleSeq& hs,
                                                  const Context& ctx)
{
	HandleUCounter fvc;
	for (const Handle& h : hs)
		fvc += free_variables_counter(h, ctx);
	return fvc;
}

HandleSeq VarScraper::free_variables(const Handle& body, Context ctx) const
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
	return is_ordered(body)	? free_variables_ordered(outs, ctx)
		: free_variables_unordered(outs, ctx);
}

HandleSeq VarScraper::free_variables_ordered(const HandleSeq& outs,
                                             const Context& ctx) const
{
	HandleSeq res;
	for (const Handle& out : outs)
	{
		// Recursive call
		HandleSeq fvs = free_variables(out, ctx);

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

HandleSeq VarScraper::free_variables_unordered(const HandleSeq& outs,
                                               const Context& ctx) const
{
	return free_variables_ordered(sorted(outs, ctx), ctx);
}

HandleSeq VarScraper::sorted(HandleSeq outs, const Context& ctx) const
{
	std::sort(outs.begin(), outs.end(), [&](const Handle& lh, const Handle& rh) {
			return less_than(lh, rh, ctx); });
	return outs;
}

bool VarScraper::less_than(const Handle& lh, const Handle& rh,
                           const Context& ctx) const
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
		return is_ordered(lh)? less_than_ordered(lout, rout, ctx)
			: less_than_unordered(lout, rout, ctx);
	}

	// Both atoms are constant nodes of the same type, sort by regular
	// atom order.
	if (not is_variable(lh))
		return lh < rh;

	// Both atoms are variables. If they are have different counts,
	// sort by counts, as that affects their semantics.
	unsigned lc = _fvc.get(lh);
	unsigned rc = _fvc.get(rh);
	if (lc != rc)
		return lc < rc;

	// Both atoms are variables with same count, sort by regular atom
	// order.
	return lh < rh;
}

bool VarScraper::less_than_ordered(const HandleSeq& lhs, const HandleSeq& rhs,
                                   const Context& ctx) const
{
	OC_ASSERT(lhs.size() == rhs.size());
	for (std::size_t i = 0; i < lhs.size(); i++)
		if (not content_eq(lhs[i], rhs[i]))
			return less_than(lhs[i], rhs[i], ctx);
	return false;
}

bool VarScraper::less_than_unordered(const HandleSeq& lhs, const HandleSeq& rhs,
                                     const Context& ctx) const
{
	return less_than_ordered(sorted(lhs, ctx), sorted(rhs, ctx), ctx);
}

bool VarScraper::is_variable(const Handle& h)
{
	Type t = h->get_type();
	return VARIABLE_NODE == t or GLOB_NODE == t;
}

bool VarScraper::is_ordered(const Handle& h)
{
	return nameserver().isA(h->get_type(), ORDERED_LINK);
}

/* ================================================================= */

FreeVariables::FreeVariables(const std::initializer_list<Handle>& variables)
	: varseq(variables), varset(variables)
{
	init_index();
}

void FreeVariables::init_index()
{
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

void FreeVariables::find_variables(const HandleSeq& oset, bool ordered)
{
	VarScraper vsc;
	vsc.find_vars(varseq, varset, oset, ordered);
	init_index();
}

void FreeVariables::find_variables(const Handle& h)
{
	find_variables(HandleSeq{h});
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
	return substitute_scoped(term, args, silent, index, 0);
}

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleMap& vm,
                                         bool silent) const
{
	return substitute_scoped(term, make_sequence(vm), silent, index, 0);
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
	   << oc_to_string(varseq, indent + OC_TO_STRING_INDENT);

	// index
	ss << indent << "index:" << std::endl
	   << oc_to_string(index, indent + OC_TO_STRING_INDENT);

	return ss.str();
}

/// Perform beta-reduction on the term.  This is more-or-less a purely
/// syntactic beta-reduction, except for two "tiny" semantic parts:
/// The semantics of QuoteLink, UnquoteLink is honoured, so that quoted
/// variables are not reduced, and the semantics of scoping
/// (alpha-conversion) is honored, so that any bound variables with the
/// same name as the free variables are alpha-hidden in the region where
/// the bound variable has scope.
Handle FreeVariables::substitute_scoped(const Handle& term,
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

	if (unquoted and nameserver().isA(ty, SCOPE_LINK))
	{
		// Perform alpha-conversion duck-n-cover.  We don't actually need
		// to alpha-convert anything, if we happen to encounter a bound
		// variable that happens to have the same name as a free variable.
		// Instead, the bound variable simply "hides" the free variable
		// for as long as the bound variable is in scope. We hide it by
		// removing it from the index.
		ScopeLinkPtr sco(ScopeLinkCast(term));
		if (nullptr == sco)
			sco = createScopeLink(term->getOutgoingSet());

		const Variables& vees = sco->get_variables();
		bool alpha_hide = false;
		for (const Handle& v : vees.varseq)
		{
			IndexMap::const_iterator idx = index_map.find(v);
			if (idx != index_map.end())
			{
				alpha_hide = true;
				break;
			}
		}

		// Hiding is expensive, so perform it only if we really have to.
		if (alpha_hide)
		{
			// Make a copy... this is what's computationally expensive.
			IndexMap hidden_map = index_map;

			// Remove the alpha-hidden variables.
			for (const Handle& v : vees.varseq)
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


			// If the hidden map is empty, then there is no more
			// substitution to be done.
			if (hidden_map.empty())
				return term;

			// Recursively fill out the subtrees. Same as below, but
			// using the alpha-renamed variable index map.
			HandleSeq oset;
			for (const Handle& h : term->getOutgoingSet())
			{
				oset.emplace_back(substitute_scoped(h, args, silent,
				                                    hidden_map, quotation));
			}
			return createLink(oset, term->get_type());
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
			Handle glst(substitute_scoped(h, args, silent, index_map, quotation));
			if (glst->is_node())
				return glst;

			changed = true;
			for (const Handle& gl : glst->getOutgoingSet())
				oset.emplace_back(gl);
		}
		else
		{
			Handle sub(substitute_scoped(h, args, silent, index_map, quotation));
			if (sub != h) changed = true;
			oset.emplace_back(sub);
		}
	}

	// Return the original atom, if it was not modified.
	if (not changed) return term;
	return createLink(oset, term->get_type());
}

/* ================================================================= */

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

			else if (TYPE_NODE == th)
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
		TypeSet ts;
		TypeSet::iterator it = ts.begin();
		nameserver().getChildren(vt, std::inserter(ts, it));
		_simple_typemap.insert({varname, ts});
	}
	else if (TYPE_CO_INH_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		TypeSet ts;
		TypeSet::iterator it = ts.begin();
		nameserver().getChildren(vt, std::inserter(ts, it));
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
		_glob_intervalmap.insert({varname, std::make_pair(
			std::round(NumberNodeCast(intervals[0])->get_value()),
			std::round(NumberNodeCast(intervals[1])->get_value()))});
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
	// Expecting the declaration list to be either a single
	// variable, or a list of variable declarations
	Type tdecls = hdecls->get_type();
	if (VARIABLE_NODE == tdecls or GLOB_NODE == tdecls)
	{
		varset.insert(hdecls);
		varseq.emplace_back(hdecls);
	}
	else if (TYPED_VARIABLE_LINK == tdecls)
	{
		get_vartype(hdecls);
	}
	else if (VARIABLE_LIST == tdecls)
	{
		// The list of variable declarations should be .. a list of
		// variables! Make sure its as expected.
		const HandleSeq& dset = hdecls->getOutgoingSet();
		validate_vardecl(dset);
	}
	else if (VARIABLE_SET == tdecls)
	{
		// The variable order does not matter
		_ordered = false;

		// The set of variable declarations should be .. a set of
		// variables! Make sure its as expected.
		const HandleSeq& dset = hdecls->getOutgoingSet();
		validate_vardecl(dset);
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
	bool ret = true;

	// Simple type restrictions?
	VariableTypeMap::const_iterator tit = _simple_typemap.find(var);
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
	VariableDeepTypeMap::const_iterator dit =
		_deep_typemap.find(var);
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
	VariableDeepTypeMap::const_iterator fit =
		_fuzzy_typemap.find(var);
	if (_fuzzy_typemap.end() != fit)
	{
		// const HandleSet &fuzzset = dit->second;
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");
		ret = false;
	}

	// Maybe we don't know this variable?
	if (varset.end() == varset.find(var)) return false;

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
	// Are there any interval restrictions?
	GlobIntervalMap::const_iterator iit = _glob_intervalmap.find(glob);

	// If there are no interval restrictions, the default
	// restrictions apply. The default restriction is 1 or more,
	// so return true as long as `n` is larger than 0.
	if (_glob_intervalmap.end() == iit)
		return (n > 0);

	const std::pair<double, double>& intervals = iit->second;
	return (n >= intervals.first);
}

/**
 * Interval checker.
 *
 * Returns true/false if the glob satisfies the upper bound
 * interval restriction.
 */
bool Variables::is_upper_bound(const Handle& glob, size_t n) const
{
	// Are there any interval restrictions?
	GlobIntervalMap::const_iterator iit = _glob_intervalmap.find(glob);

	// If there are no interval restrictions, the default
	// restrictions apply. The default upper bound is
	// "unbounded"; any number of matches are OK.
	if (_glob_intervalmap.end() == iit)
		return true;

	// Negative upper bound means "unbounded" (infinity).
	const std::pair<double, double>& intervals = iit->second;
	return (n <= intervals.second or intervals.second < 0);
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
		try
		{
			index.at(h);

			// Merge the two typemaps, if needed.
			try
			{
				const TypeSet& tms = vset._simple_typemap.at(h);
				TypeSet mytypes =
					type_intersection(_simple_typemap[h], tms);
				_simple_typemap.erase(h);	 // is it safe to erase if
                                             // h not in already?
				_simple_typemap.insert({h, mytypes});
			}
			catch(const std::out_of_range&) {}
		}
		catch(const std::out_of_range&)
		{
			// Found a new variable! Insert it.
			index.insert({h, varseq.size()});
			varseq.emplace_back(h);
			varset.insert(h);

			// Install the type constraints, as well.
			// The at() might throw...
			try
			{
				_simple_typemap.insert({h, vset._simple_typemap.at(h)});
			}
			catch(const std::out_of_range&) {}
		}
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
	// Simple type info
	const auto& sit = _simple_typemap.find(var);
	if (sit != _simple_typemap.end())
	{
		HandleSeq types;
		for (Type t : sit->second)
			types.push_back(Handle(createTypeNode(t)));
		Handle types_h = types.size() == 1 ? types[0]
			: createLink(types, TYPE_CHOICE);
		return Handle(createLink(TYPED_VARIABLE_LINK, alt, types_h));
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

	// TODO: _glob_intervalmap?

	// No type info
	return alt;
}

Handle Variables::get_vardecl() const
{
	HandleSeq vars;
	for (const Handle& var : varseq)
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

		// Sigh. This UnquoteLink check is a bit of hackery. It can
		// happen, during pattern-recognition, that we are *searching*
		// for BindLink's/GetLink's, and need to construct a dummy
		// variable to match a variable list.  This dummy will be
		// unquoted first.  Its not unquoting an actual VariableList,
		// though, its just unquoting a dummy, and so there's nothing
		// there, its empty, there's nothing to do.  So just pass on
		// the whole she-bang.  See the `recog.scm` example for a
		// a real-world example of this happening.
		else if (UNQUOTE_LINK == t)
		{
			return;
		}
		else
		{
			throw InvalidParamException(TRACE_INFO,
				"Expected a VariableNode or a TypedVariableLink, got: %s"
				"\nVariableList is %s",
					nameserver().getTypeName(t).c_str(),
					to_string().c_str());
		}
	}
}

std::string Variables::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// FreeVariables
	ss << FreeVariables::to_string(indent);

	// Simple typemap
	std::string indent_p = indent + OC_TO_STRING_INDENT;
	std::string indent_pp = indent_p + OC_TO_STRING_INDENT;
	ss << indent << "_simple_typemap:" << std::endl;
	ss << indent_p << "size = " << _simple_typemap.size() << std::endl;
	unsigned i = 0;
	for (const auto& v : _simple_typemap)
	{
		ss << indent_p << "variable[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent_pp)
		   << indent_p << "types[" << i << "]:";
		for (auto& t : v.second)
			ss << " " << nameserver().getTypeName(t);
		ss << std::endl;
		i++;
	}

	return ss.str();
}

std::string oc_to_string(const FreeVariables::IndexMap& imap,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << imap.size() << std::endl;
	for (const auto& el : imap) {
		ss << indent << "index[" << el.second << "]: "
		   << el.first->id_to_string() << std::endl;
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
