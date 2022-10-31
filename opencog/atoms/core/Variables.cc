/*
 * opencog/atoms/core/Variables.cc
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

// #include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/TypedVariableLink.h>
#include <opencog/atoms/core/TypeIntersectionLink.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "VariableSet.h"
#include "VariableList.h"
#include "Variables.h"

namespace opencog {

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
 */
void Variables::unpack_vartype(const Handle& htypelink)
{
	if (TYPED_VARIABLE_LINK != htypelink->get_type())
	{
		throw InvalidParamException(TRACE_INFO,
			"Expecting TypedVariableLink, got %s",
			htypelink->to_short_string().c_str());
	}

	TypedVariableLinkPtr tvlp(TypedVariableLinkCast(htypelink));
	const Handle& varname(tvlp->get_variable());
	_typemap.insert({varname, tvlp});

	varset.insert(varname);
	varseq.emplace_back(varname);
}

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
	// XXX FIXME URE calls us with broken handle!!
	if (nullptr == hdecls) return;

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
		unpack_vartype(hdecls);
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
	else if (ANCHOR_NODE == tdecls)
	{
		_anchor = hdecls;
	}
	else
	{
		throw InvalidParamException(TRACE_INFO,
			"Expected a VariableList holding variable declarations");
	}
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
			unpack_vartype(h);
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

/* ================================================================= */

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

bool Variables::is_well_typed() const
{
	for (const auto& vt : _typemap)
		if (not opencog::is_well_typed(vt.second->get_simple_typeset()))
			return false;
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
 * Returns true if we are holding the variable `var`, and if
 * the `val` satisfies the type restrictions that apply to `var`.
 */
bool Variables::is_type(const Handle& var, const Handle& val) const
{
	// If not holding, then fail.
	if (varset.end() == varset.find(var)) return false;

	// If no type restrictions, then success.
	VariableTypeMap::const_iterator tit = _typemap.find(var);
	if (_typemap.end() == tit) return true;

	return tit->second->is_type(val);
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
	VariableTypeMap::const_iterator tit = _typemap.find(var);
	if (_typemap.end() == tit) return true;

	// There are type restrictions; do they match?
	return tit->second->is_type(gtype);
}

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

/* ================================================================= */
/**
 * Interval checker.
 *
 * Returns true if the glob satisfies the lower bound
 * interval restriction.
 */
bool Variables::is_lower_bound(const Handle& glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);

	if (TypeChoice::is_empty(intervals))
		return false;
	return (n >= intervals.first);
}

/**
 * Interval checker.
 *
 * Returns true if the glob satisfies the upper bound
 * interval restriction.
 */
bool Variables::is_upper_bound(const Handle &glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);

	if (TypeChoice::is_empty(intervals))
		return false;
	return (n <= intervals.second or intervals.second < 0);
}

/**
 * Interval checker.
 *
 * Returns true if the glob can match a variable number of items.
 * i.e. if it is NOT an ordinary variable.
 */
bool Variables::is_globby(const Handle &glob) const
{
	const GlobInterval &intervals = get_interval(glob);

	if (TypeChoice::is_empty(intervals))
		return false;
	return (1 != intervals.first or 1 != intervals.second);
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

const GlobInterval Variables::get_interval(const Handle& var) const
{
	const auto& decl = _typemap.find(var);

	if (decl == _typemap.end())
		return default_interval(var->get_type());

	return decl->second->get_glob_interval();
}

/* ================================================================= */
/**
 * Substitute the given arguments for the variables occurring in a tree.
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
 * If a variable is both in *this and vset then its type union
 * is assigned to it.
 */
void Variables::extend(const Variables& vset)
{
	for (const Handle& h : vset.varseq)
	{
		auto typemap_it = vset._typemap.find(h);
		if (typemap_it != vset._typemap.end())
			unpack_vartype(HandleCast(typemap_it->second));
		else
		{
			varseq.emplace_back(h);
			varset.insert(h);
		}
	}

	// If either this or the other are ordered then the result is ordered
	_ordered = _ordered or vset._ordered;
}

/**
 * Extend a set of variables.
 *
 * That is, merge the given variables into this set.
 *
 * If a variable is both in *this and vset then its type intersection
 * is assigned to it.
 */
void Variables::extend_intersect(const Variables& vset)
{
	for (const Handle& h : vset.varseq)
	{
		auto index_it = index.find(h);
		if (index_it != index.end())
		{
			// Merge the two typemaps, if needed.
			auto vit = vset._typemap.find(h);
			if (vit != vset._typemap.end())
			{
				auto tit = _typemap.find(h);
				if (tit != _typemap.end())
				{
					Handle isect = HandleCast(
						createTypeIntersectionLink(HandleSeq{
							HandleCast(tit->second->get_typedecl()),
							HandleCast(vit->second->get_typedecl())}));
					TypedVariableLinkPtr tvp =
						createTypedVariableLink(h, isect);
					_typemap[h] = tvp;
				}
				else
				{
					_typemap.insert({h, vit->second});
				}
			}
		}
		else
		{
			// Found a new variable! Insert it.
			index.insert({h, varseq.size()});

			auto typemap_it = vset._typemap.find(h);
			if (typemap_it != vset._typemap.end())
			{
				unpack_vartype(HandleCast(typemap_it->second));
			}
			else
			{
				varseq.emplace_back(h);
				varset.insert(h);
			}
		}
	}

	// If either this or the other are ordered then the result is ordered
	_ordered = _ordered or vset._ordered;
}

/* ================================================================= */

void Variables::erase(const Handle& var)
{
	// Remove from the type maps
	_typemap.erase(var);

	// Remove FreeVariables
	FreeVariables::erase(var);
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
	auto sime = _typemap.find(vme);
	auto soth = other._typemap.find(voth);
	if (sime == _typemap.end() and
	    soth != other._typemap.end() and
	    not soth->second->is_untyped()) return false;

	if (sime != _typemap.end())
	{
		if (soth == other._typemap.end() and
		    sime->second->is_untyped()) return true;
		if (soth == other._typemap.end()) return false;

		if (not sime->second->is_equal(*soth->second)) return false;
	}

	// If we got to here, everything must be OK.
	return true;
}

bool Variables::operator==(const Variables& other) const
{
	return is_equal(other);
}

bool Variables::operator<(const Variables& other) const
{
	return FreeVariables::operator<(other)
		or _typemap < other._typemap;
}

/* ================================================================= */

/// Look up the type declaration for `var`, but create the actual
/// declaration for `alt`.  This is an alpha-renaming.
Handle Variables::get_type_decl(const Handle& var, const Handle& alt) const
{
	// Get the type info
	const auto& tit = _typemap.find(var);
	if (_typemap.end() == tit) return alt;

	return HandleCast(createTypedVariableLink(alt,
		HandleCast(tit->second->get_typedecl())));
}

Handle Variables::get_vardecl() const
{
	HandleSeq vardecls;
	for (const Handle& var : varseq)
	{
		const auto& tit = _typemap.find(var);
		if (_typemap.end() == tit)
			vardecls.emplace_back(var);
		else
			vardecls.emplace_back(tit->second);
	}

	if (vardecls.size() == 1)
		return vardecls[0];

	if (_ordered)
		return HandleCast(createVariableList(std::move(vardecls)));

	return HandleCast(createVariableSet(std::move(vardecls)));
}

/* ================================================================= */

std::string Variables::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// FreeVariables
	ss << FreeVariables::to_string(indent) << std::endl;

	// Whether it is ordered
	ss << indent << "_ordered = " << _ordered << std::endl;

	// Typemap
	ss << indent << "_typemap:" << std::endl
	   << oc_to_string(_typemap, indent + OC_TO_STRING_INDENT);

	return ss.str();
}

std::string oc_to_string(const VariableTypeMap& hmap, const std::string& indent)
{
	// Cut-n-paste of oc_to_string(const HandleMap& hmap)
   std::stringstream ss;
   ss << indent << "size = " << hmap.size();
   int i = 0;
   for (const auto& p : hmap) {
      ss << std::endl << indent << "key[" << i << "]:" << std::endl
         << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
         << indent << "value[" << i << "]:" << std::endl
         << p.second->to_string(indent + OC_TO_STRING_INDENT);
      i++;
   }
   return ss.str();
}

std::string oc_to_string(const Variables& var, const std::string& indent)
{
	return var.to_string(indent);
}

} // ~namespace opencog
