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

#include <opencog/util/Logger.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/proto/NameServer.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atoms/core/TypeNode.h>

#include "ScopeLink.h"
#include "VariableList.h"
#include "Variables.h"

namespace opencog {

struct VarScraper
{
	Quotation _quotation;
	HandleSet _bound_vars;
	void find_vars(HandleSeq&, HandleSet&, const HandleSeq&);
};

/* ================================================================= */
//
// The work-horse that does the actual heavy-lifting.
// See the find_variables() method for the description of what
// this does, and why.
//
// Note: the algo used here is nearly identical to that in
// ScopeLink::term_hash() -- if you modify this, then modify that
// one too.
void VarScraper::find_vars(HandleSeq& varseq, HandleSet& varset,
                           const HandleSeq& oset)
{
	for (const Handle& h : oset)
	{
		Type t = h->get_type();

		if ((VARIABLE_NODE == t or GLOB_NODE == t) and
		    _quotation.is_unquoted() and
		    0 == varset.count(h) and
		    0 == _bound_vars.count(h))
		{
			varseq.emplace_back(h);
			varset.insert(h);
		}

		if (not h->is_link()) continue;

		bool issco = _quotation.is_unquoted()
			and nameserver().isA(t, SCOPE_LINK);
		HandleSet bsave;
		if (issco)
		{
			// Save the current set of bound variables...
			bsave = _bound_vars;

			// The ScopeLink ctor did the bound-variable extraction.
			ScopeLinkPtr sco(ScopeLinkCast(h));
			const Variables& vees = sco->get_variables();
			for (const Handle& v : vees.varseq) _bound_vars.insert(v);
		}

		// Save quotation on the stack before updating it for the
		// recursive call
		Quotation save_quotation(_quotation);
		_quotation.update(t);

		find_vars(varseq, varset, h->getOutgoingSet());

		if (issco)
			_bound_vars = bsave;

		// Restore current state from the stack
		_quotation = save_quotation;
	}
}

/* ================================================================= */

FreeVariables::FreeVariables(const std::initializer_list<Handle>& variables)
	: varseq(variables), varset(variables)
{
	unsigned i = 0;
	for (const Handle& var : variables)
		index.insert({var, i++});
}

void FreeVariables::find_variables(const HandleSeq& oset)
{
	VarScraper vsc;
	vsc.find_vars(varseq, varset, oset);

	// Build the index from variable name, to its ordinal number.
	size_t sz = varseq.size();
	for (size_t i=0; i<sz; i++)
		index.insert(std::pair<Handle, unsigned int>(varseq[i], i));
}

void FreeVariables::find_variables(const Handle& h)
{
	find_variables(HandleSeq{h});
}

HandleSeq FreeVariables::make_sequence(const HandleMap& varmap) const
{
	HandleSeq values;
	for (const Handle& var : varseq)
	{
		HandleMap::const_iterator it = varmap.find(var);
		values.push_back(it == varmap.end() ? var : it->second);
	}
	return values;
}

void FreeVariables::erase(const Handle& var)
{
	// Remove from varset
	varset.erase(var);

	// Remove from index
	index.erase(var);

	// Remove from varseq and update all values in the subsequent
	// index as they have changed.
	auto it = std::find(varseq.begin(), varseq.end(), var);
	if (it != varseq.end()) {
		it = varseq.erase(it);
		for (; it != varseq.end(); ++it)
			index.find(*it)->second--;
	}
}

bool FreeVariables::operator<(const FreeVariables& other) const
{
	return varseq < other.varseq;
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
	// then just return the corresponding value.
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
	for (const Handle& h : term->getOutgoingSet())
	{
		// GlobNodes are matched with a list of one or more values.
		// Those values need to be in-lined, stripping off the list
		// that wraps them up.  See MapLinkUTest for examples.
		if (GLOB_NODE == h->get_type())
		{
			Handle glst(substitute_scoped(h, args, silent, index_map, quotation));
			if (glst->is_node())
				return glst;
			for (const Handle& gl : glst->getOutgoingSet())
				oset.emplace_back(gl);
		}
		else
			oset.emplace_back(
				substitute_scoped(h, args, silent, index_map, quotation));
	}

	return createLink(oset, term->get_type());
}

/* ================================================================= */

bool FreeVariables::is_identical(const FreeVariables& other) const
{
	Arity ary = varseq.size();
	if (ary != other.varseq.size()) return false;
	for (Arity i=0; i< ary; i++)
	{
		if (*((AtomPtr) varseq[i]) != *((AtomPtr) other.varseq[i])) return false;
	}
	return true;
}

/* ================================================================= */

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
 * Substitute the given values for the variables occuring in a tree.
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
	// Viz., one of the values might be undefined, and that's OK, if that
	// value is never actually used.  Fixing this requires a cut-n-paste
	// of the substitute_nocheck code. I'm too lazy to do this ... no one
	// wants this whizzy-ness just right yet.
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

std::size_t FreeVariables::size() const
{
	return varseq.size();
}

bool FreeVariables::empty() const
{
	return varseq.empty();
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
	{
		vars.emplace_back(get_type_decl(var, var));
	}
	if (vars.size() == 1)
		return vars[0];

	return Handle(createVariableList(vars));
}

std::string Variables::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// Varseq
	ss << indent << "varseq:" << std::endl
	   << oc_to_string(varseq, indent + OC_TO_STRING_INDENT);

	// Simple typemap
	ss << indent << "_simple_typemap:" << std::endl;
	ss << indent << "size = " << _simple_typemap.size() << std::endl;
	unsigned i = 0;
	for (const auto& v : _simple_typemap)
	{
		ss << indent << "variable[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent + OC_TO_STRING_INDENT)
		   << indent << "types[" << i << "]:";
		for (auto& t : v.second)
			ss << " " << nameserver().getTypeName(t);
		ss << std::endl;
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Variables& var, const std::string& indent)
{
	return var.to_string(indent);
}

} // ~namespace opencog
