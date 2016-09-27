/*
 * Variables.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atoms/TypeNode.h>

#include "ScopeLink.h"
#include "VariableList.h"
#include "Variables.h"

namespace opencog {

struct VarScraper
{
	bool _in_quote;
	OrderedHandleSet _bound_vars;
	void find_vars(HandleSeq&, OrderedHandleSet&, const HandleSeq&);
};

/* ================================================================= */
//
// The work-horse that does the actual heavy-lifting.
// See the find_variables() method for the description of what
// this does, and why.
void VarScraper::find_vars(HandleSeq& varseq, OrderedHandleSet& varset,
                           const HandleSeq& oset)
{
	for (const Handle& h : oset)
	{
		Type t = h->getType();

		if ((VARIABLE_NODE == t or GLOB_NODE == t) and
		    not _in_quote and
		    0 == varset.count(h) and
		    0 == _bound_vars.count(h))
		{
			varseq.emplace_back(h);
			varset.insert(h);
		}

		if (not h->isLink()) continue;

		// Save the recursive state on stack.
		bool save_quote = _in_quote;
		if (QUOTE_LINK == t)
			_in_quote = true;
		else
		if (UNQUOTE_LINK == t)
			_in_quote = false;

		bool issco = classserver().isA(t, SCOPE_LINK);
		OrderedHandleSet bsave = _bound_vars;
		if (issco)
		{
			// Save the current set of bound variables...
			bsave = _bound_vars;

			// If we can cast to ScopeLink, then do so; otherwise,
			// take the low road, and let ScopeLink constructor
			// do the bound-variable extraction.
			ScopeLinkPtr sco(ScopeLinkCast(h));
			if (nullptr == sco)
				sco = createScopeLink(h->getOutgoingSet());
			const Variables& vees = sco->get_variables();
			for (const Handle& v : vees.varseq) _bound_vars.insert(v);
		}

		find_vars(varseq, varset, h->getOutgoingSet());

		if (issco)
			_bound_vars = bsave;

		// Restore current state from the stack.
		if (QUOTE_LINK == t or UNQUOTE_LINK == t)
			_in_quote = save_quote;
	}
}

void FreeVariables::find_variables(const HandleSeq& oset)
{
	VarScraper vsc;
	vsc._in_quote = false;
	vsc.find_vars(varseq, varset, oset);

	// Build the index from variable name, to its ordinal number.
	size_t sz = varseq.size();
	for (size_t i=0; i<sz; i++)
		index.insert(std::pair<Handle, unsigned int>(varseq[i], i));
}

void FreeVariables::find_variables(const Handle& h)
{
	if (h->isLink())
	{
		find_variables(h->getOutgoingSet());
	}
	else
	{
		HandleSeq bogus;
		bogus.push_back(h);
		find_variables(bogus);
	}
}

/* ================================================================= */

Handle FreeVariables::substitute_nocheck(const Handle& term,
                                         const HandleSeq& args) const
{
	return substitute_scoped(term, args, index, 0);
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
                                        const IndexMap& index_map,
                                        int quotation_level) const
{
	// If we are not in a quote context, and `term` is a variable,
	// then just return the corresponding value.
	if (0 == quotation_level)
	{
		IndexMap::const_iterator idx = index_map.find(term);
		if (idx != index_map.end())
			return args.at(idx->second);
	}

	// If its a node, and its not a variable, then it is a constant,
	// and just return that.
	if (not term->isLink()) return term;

	// Quotation management
	Type ty = term->getType();
	if (QUOTE_LINK == ty)
	{
		quotation_level++;
	}
	else
	if (UNQUOTE_LINK == ty)
	{
		quotation_level--;
		if (quotation_level < 0)
			throw SyntaxException(TRACE_INFO, "Unbalanced quotes!");
	}
	else
	if (0 == quotation_level and classserver().isA(ty, SCOPE_LINK))
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

			// If the hidden map is empty, then there is no more
			// substitution to be done.
			if (hidden_map.empty())
				return term;

			// Recursively fill out the subtrees. Same as below, but
			// using the alpha-renamed variable index map.
			HandleSeq oset;
			for (const Handle& h : term->getOutgoingSet())
			{
				oset.emplace_back(substitute_scoped(h, args, hidden_map, quotation_level));
			}
			return Handle(createLink(term->getType(), oset));
		}
	}

	// Recursively fill out the subtrees.
	HandleSeq oset;
	for (const Handle& h : term->getOutgoingSet())
	{
		// GlobNodes are matched with a list of one or more values.
		// Those values need to be in-lined, stripping off the list
		// that wraps them up.  See MapLinkUTest for examples.
		if (GLOB_NODE == h->getType())
		{
			Handle glst(substitute_scoped(h, args, index_map, quotation_level));
			for (const Handle& gl : glst->getOutgoingSet())
				oset.emplace_back(gl);
		}
		else
			oset.emplace_back(
				substitute_scoped(h, args, index_map, quotation_level));
	}
	return Handle(createLink(term->getType(), oset));
}

/* ================================================================= */

/// Return true if the other Variables struct is equal to this one,
/// up to alpha-conversion. That is, same number of variables, same
/// type restrictions, but different actual variable names.
bool Variables::is_equal(const Variables& other) const
{
	size_t sz = varseq.size();
	if (other.varseq.size() != sz) return false;

	// Side-by-side comparison
	for (size_t i=0; i<sz; i++)
	{
		const Handle& vme(varseq[i]);
		const Handle& voth(other.varseq[i]);

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
	}

	// If we got toe here, everything must be OK.
	return true;
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
	VariableTypeMap::const_iterator tit =
		_simple_typemap.find(var);
	if (_simple_typemap.end() != tit)
	{
		const std::set<Type> &tchoice = tit->second;
		Type htype = val->getType();
		std::set<Type>::const_iterator allow = tchoice.find(htype);

		// If the value has the simple type, then we are good to go;
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
		const OrderedHandleSet &sigset = dit->second;
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
		// const OrderedHandleSet &fuzzset = dit->second;
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
	// The arity must be one for there to be a match.
	size_t len = hseq.size();
	if (varset.size() != len) return false;

	// Check the type restrictions.
	for (size_t i=0; i<len; i++)
	{
		if (not is_type(varseq[i], hseq[i])) return false;
	}
	return true;
}

/* ================================================================= */
/**
 * Substitute the given values for the variables occuring in a tree.
 * That is, perform beta-reduction.  This is a lot like applying the
 * function `func` to the argument list `args`, except that no actual
 * evaluation is performed; only substitution.
 *
 * The resulting tree is NOT placed into any atomspace. If you want
 * that, you must do it yourself.  If you want evaluation or execution
 * to happen during sustitution, use either the EvaluationLink, the
 * ExecutionOutputLink, or the Instantiator.
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
 * then the returned value will be
 *
 *   EvaluationLink
 *      PredicateNode "something"
 *      ListLink
 *          NumberNode 2.0000    ; note reversed order here, also
 *          ConceptNode "one"
 *
 * That is, the values `one` and `2.0` were substituted for `$a` and `$b`.
 *
 * The `func` can be, for example, a single variable name(!) In this
 * case, the corresponding `arg` is returned. So, for example, if the
 * `func` was simply `$b`, then `2.0` would be returned.
 *
 * Type checking is performed before substitution; if the args fail to
 * satisfy the type constraints, an exception is thrown.
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
                             const HandleSeq& args) const
{
	if (args.size() != varseq.size())
		throw SyntaxException(TRACE_INFO,
			"Incorrect number of arguments specified, expecting %lu got %lu",
			varseq.size(), args.size());

	// XXX TODO type-checking could be lazy; if the function is not
	// actually using one of the args, it's type should not be checked.
	// Viz., one of the values might be undefined, and that's OK, if that
	// value is never actually used.  Fixing this requires a cut-n-paste
	// of the substitute_nocheck code. I'm too lazy to do this ... no one
	// wants this whizzy-ness just right yet.
	if (not is_type(args))
		throw SyntaxException(TRACE_INFO,
			"Arguments fail to match variable declarations");

	return substitute_nocheck(func, args);
}

/* ================================================================= */
/**
 * Extend a set of variables.
 *
 * That is, merge the given variables into this set.
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
				const std::set<Type>& tms = vset._simple_typemap.at(h);
				std::set<Type> mytypes = _simple_typemap[h];
				for (Type t : tms)
					mytypes.insert(t);
				_simple_typemap.erase(h);	 // is it safe to erase if h not in already?
				_simple_typemap.insert({h,mytypes});
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

std::string Variables::to_string() const
{
	std::stringstream ss;
	ss << "size = " << _simple_typemap.size() << std::endl;
	unsigned i = 0;
	for (const auto& v : _simple_typemap)
	{
		ss << "variable[" << i << "]:" << std::endl
			<< h_to_string(v.first)
		   << "types[" << i << "]:";
		for (auto& t : v.second)
			ss << " " << classserver().getTypeName(t);
		ss << std::endl;
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Variables& var)
{
	return var.to_string();
}

std::string oc_to_string(const FreeVariables::IndexMap& imap)
{
	std::stringstream ss;
	ss << "size = " << imap.size() << std::endl;
	for (const auto& vi : imap)
	{
		ss << "at[" << vi.second << "]:" << std::endl
		   << h_to_string(vi.first);
	}
	return ss.str();
}

} // ~namespace opencog
