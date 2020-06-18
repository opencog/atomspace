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

#include <opencog/util/algorithm.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/TypedVariableLink.h>
#include <opencog/atoms/core/TypeNode.h>
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

	const TypeSet& ts = tvlp->get_simple_typeset();
	if (0 < ts.size())
		_simple_typemap.insert({varname, ts});

	const HandleSet& hs = tvlp->get_deep_typeset();
	if (0 < hs.size())
		_deep_typemap.insert({varname, hs});

	const std::pair<size_t, size_t>& gi = tvlp->get_glob_interval();
	if (tvlp->default_interval() != gi)
		_glob_intervalmap.insert({varname, gi});

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
	VariableSimpleTypeMap::const_iterator tit = _simple_typemap.find(var);
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
 * Returns true if the glob satisfies the lower bound
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
 * Returns true if the glob satisfies the upper bound
 * interval restriction.
 */
bool Variables::is_upper_bound(const Handle &glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);
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

const GlobInterval& Variables::get_interval(const Handle& var) const
{
	const auto& interval = _glob_intervalmap.find(var);

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
			auto stypemap_it = vset._simple_typemap.find(h);
			if (stypemap_it != vset._simple_typemap.end())
			{
				const TypeSet& tms = stypemap_it->second;
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
	_typemap.erase(var);
	_simple_typemap.erase(var);
	_deep_typemap.erase(var);

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
	return FreeVariables::operator<(other)
		or (_simple_typemap == other._simple_typemap
		     and _deep_typemap < other._deep_typemap);
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

	const auto& dit = _deep_typemap.find(var);
	if (dit != _deep_typemap.end())
	{
		for (const Handle& sig: dit->second)
			types.push_back(sig);
	}

	// Check if ill-typed a.k.a invalid type intersection.
	if (types.empty() and sit != _simple_typemap.end())
	{
		const Handle ill_type = createLink(TYPE_CHOICE);
		return createLink(TYPED_VARIABLE_LINK, alt, ill_type);
	}

	const auto interval = get_interval(var);
	if (interval != default_interval(var->get_type()))
	{
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
	if (not types.empty())
	{
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
	   << oc_to_string(_glob_intervalmap, indent_p) << std::endl;

	// Deep typemap
	ss << indent << "_deep_typemap:" << std::endl
	   << oc_to_string(_deep_typemap, indent_p);

	return ss.str();
}

std::string oc_to_string(const Variables& var, const std::string& indent)
{
	return var.to_string(indent);
}

std::string oc_to_string(const VariableSimpleTypeMap& vtm,
                         const std::string& indent)
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

} // ~namespace opencog
