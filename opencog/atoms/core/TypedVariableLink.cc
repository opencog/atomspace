/*
 * opencog/atoms/core/TypedVariableLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  June 2020
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

#include <opencog/atoms/base/ClassServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/TypeNode.h>

#include "TypedVariableLink.h"

using namespace opencog;

void TypedVariableLink::init()
{
	// Must have atom and type specification.
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting atom and type specification; got %s",
			to_string().c_str());

	// Type-check. This is ... kind of a pointless restriction,
	// except that pretty much everything else expects variables
	// in this location.
	Type stype = _outgoing[0]->get_type();
	if (VARIABLE_NODE != stype and
	    GLOB_NODE != stype)
		throw SyntaxException(TRACE_INFO,
			"Sorry, we expect type names to be variables!");

	// Allow VARIABLE_NODE, although this is a bug in the URE,
	// which should be using a SignatureLink for this case. XXX FIXME.
	Type dtype = _outgoing[1]->get_type();
	if (not nameserver().isA(dtype, TYPE_NODE) and
	    DEFINED_TYPE_NODE != dtype and
	    TYPE_CHOICE != dtype and
	    TYPE_SET_LINK != dtype and
	    VARIABLE_NODE != dtype and // XXX FIXME this is wrong; URE-bug
	    SIGNATURE_LINK != dtype and
	    INTERVAL_LINK != dtype and
	    ARROW_LINK != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting type defintion, got %s in\n%s",
				nameserver().getTypeName(dtype).c_str(),
				to_short_string().c_str());

	analyze();
}

TypedVariableLink::TypedVariableLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	init();
}

TypedVariableLink::TypedVariableLink(const Handle& name, const Handle& defn)
	: Link({name, defn}, TYPED_VARIABLE_LINK)
{
	init();
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
 * or possibly types that are SignatureLink's or polymorphic
 * combinations thereof: e.g. the following is valid:
 *
 *    TypedVariableLink
 *       VariableNode "$some_var_name"
 *       TypeChoice
 *          TypeNode  "ConceptNode"
 *          SignatureLink
 *              InheritanceLink
 *                 PredicateNode "foobar"
 *                 TypeNode  "ListLink"
 *          SignatureLink
 *              InheritanceLink
 *                 ConceptNode "animal"
 *                 ConceptNode "tiger"
 *
 */
void TypedVariableLink::analyze()
{
	const Handle& varname(_outgoing[0]);
	Handle vartype(_outgoing[1]);

	Type nt = varname->get_type();
	Type t = vartype->get_type();

	// Specifying how many atoms can be matched to a GlobNode, if any
	HandleSeq interval;

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
				interval = h->getOutgoingSet();

			else if (TYPE_NODE == th or
			         TYPE_INH_NODE == th or
			         TYPE_CO_INH_NODE == th)
			{
				vartype = h;
				t = th;
			}

			else throw SyntaxException(TRACE_INFO,
				"Unexpected contents in TypeSetLink\n"
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
			_simple_typeset.insert(vt);
		}
	}
	else if (TYPE_INH_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		const TypeSet& ts = nameserver().getChildrenRecursive(vt);
		_simple_typeset.insert(ts.begin(), ts.end());
	}
	else if (TYPE_CO_INH_NODE == t)
	{
		Type vt = TypeNodeCast(vartype)->get_kind();
		const TypeSet& ts = nameserver().getParentsRecursive(vt);
		_simple_typeset.insert(ts.begin(), ts.end());
	}
	else if (TYPE_CHOICE == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		size_t tss = tset.size();
		for (size_t i=0; i<tss; i++)
		{
			Handle ht(tset[i]);
			Type var_type = ht->get_type();
			if (TYPE_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt) _simple_typeset.insert(vt);
			}
			else if (TYPE_INH_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt)
				{
					TypeSet ts = nameserver().getChildrenRecursive(vt);
					_simple_typeset.insert(ts.begin(), ts.end());
				}
			}
			else if (TYPE_CO_INH_NODE == var_type)
			{
				Type vt = TypeNodeCast(ht)->get_kind();
				if (ATOM != vt)
				{
					TypeSet ts = nameserver().getParentsRecursive(vt);
					_simple_typeset.insert(ts.begin(), ts.end());
				}
			}
			else if (SIGNATURE_LINK == var_type)
			{
				const HandleSeq& sig = ht->getOutgoingSet();
				if (1 != sig.size())
					throw SyntaxException(TRACE_INFO,
						"Unexpected contents in SignatureLink\n"
						"Expected arity==1, got %s", vartype->to_string().c_str());

				_deep_typeset.insert(ht);
			}
			else
			{
				throw InvalidParamException(TRACE_INFO,
					"VariableChoice has unexpected content:\n"
					"Expected TypeNode, got %s",
					    nameserver().getTypeName(ht->get_type()).c_str());
			}
		}

		// An empty disjunction corresponds to a bottom type.
		if (tset.empty())
			_simple_typeset.insert({NOTYPE});

		// Check for (TypeChoice (TypCoInh 'Atom)) which is also bottom.
		if (1 == tset.size() and TYPE_CO_INH_NODE == tset[0]->get_type())
		{
			Type vt = TypeNodeCast(tset[0])->get_kind();
			if (ATOM == vt)
				_simple_typeset.insert({NOTYPE});
		}
	}
	else if (SIGNATURE_LINK == t)
	{
		const HandleSeq& tset = vartype->getOutgoingSet();
		if (1 != tset.size())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in SignatureLink\n"
				"Expected arity==1, got %s", vartype->to_string().c_str());

		_deep_typeset.insert(vartype);
	}
	else if (VARIABLE_NODE == t)
	{
		// This is a work-around to a URE bug. The URE should be
		// using a SignatureLink, but its not. As a result, it
		// gets undefined behavior and incorect results. Too bad.
		// For now, just avoid throwing an exception. XXX FIXME.
	}
	else if (GLOB_NODE == nt and INTERVAL_LINK == t)
	{
		interval = vartype->getOutgoingSet();
	}
	else
	{
		// Instead of throwing here, we could also assume that
		// there is an implied SignatureLink, and just poke the
		// contents into the _deep_typeset. On the other hand,
		// it seems better to throw, so that beginers aren't
		// thrown off the trail for what essentially becomes
		// a silent error with unexpected effects...
		throw SyntaxException(TRACE_INFO,
			"Unexpected contents in TypedVariableLink\n"
			"Expected type specifier (e.g. TypeNode, TypeChoice, etc.), got %s",
			nameserver().getTypeName(t).c_str());
	}

	if (0 < interval.size())
	{
		long lb = std::lround(NumberNodeCast(interval[0])->get_value());
		long ub = std::lround(NumberNodeCast(interval[1])->get_value());
		if (lb < 0) lb = 0;
		if (ub < 0) ub = SIZE_MAX;

		_glob_interval = std::make_pair(lb, ub);
	}
	else
		_glob_interval = default_interval();
}

const std::pair<size_t, size_t>
TypedVariableLink::default_interval(void) const
{
	if (_outgoing[0]->get_type() == VARIABLE_NODE)
		return std::make_pair(1, 1);
	return std::make_pair(1, SIZE_MAX);
}

/* ================================================================= */

/// Return true if the type is completely unconstrained.
/// The can happen in various ways, e.g. if the user wrote
///    (TypedVariable (Variable "x") (Type 'Atom))
/// or
///    (TypedVariable (Variable "x") (TypeChoice (Type 'Atom)))
/// or specified TypeInh/TypeCoInh that intersected/unioned to Atom.
///
/// XXX This is not quite true, since, in principle, we can have types
/// that specify Values ... however, for backwards compatibility, we
/// enforce this; it is checked in AlphaConvertUTest.
///
bool TypedVariableLink::is_untyped(void) const
{
	return 0 == _simple_typeset.size() and 0 == _deep_typeset.size()
		and default_interval() == _glob_interval;
}

/* ================================================================= */

/// Return true if the glob can match a variable number of items.
/// i.e. if it is NOT an ordinary variable.
bool TypedVariableLink::is_globby(void) const
{
	return (1 != _glob_interval.first or 1 != _glob_interval.second);
}

/// Returns true if the glob satisfies the lower bound
/// interval restriction.
bool TypedVariableLink::is_lower_bound(size_t n) const
{
	return n >= _glob_interval.first;
}

/// Returns true if the glob satisfies the upper bound
/// interval restriction.
bool TypedVariableLink::is_upper_bound(size_t n) const
{
	return n <= _glob_interval.second or _glob_interval.second < 0;
}

/* ================================================================= */

/// Return true if the other TypedVariable is equal to this one,
/// up to alpha-conversion. This returns `true` if the other
/// TypedVariable has the same type restrictions, even though it
/// might have a different variable name. That is, return `true`
/// if the two variables are alpha-convertable.
///
/// The compare is a semantic compare, not a syntactic compare. That
/// is, the actual type restrictions are compared, and NOT the Atom
/// used to specify the restriction.
///
bool TypedVariableLink::is_equal(const TypedVariableLink& other) const
{
	// If one is a GlobNode, and the other a VariableNode,
	// then its a mismatch.
	if (get_variable()->get_type() != other.get_variable()->get_type())
		return false;

	// If typed, types must match.
	if (get_simple_typeset() != other.get_simple_typeset())
		return false;

	if (get_deep_typeset() != other.get_deep_typeset())
		return false;

	if (get_glob_interval() != other.get_glob_interval())
		return false;

	// If we got to here, everything must be OK.
	return true;
}

/* ================================================================= */

DEFINE_LINK_FACTORY(TypedVariableLink, TYPED_VARIABLE_LINK);

/* ===================== END OF FILE ===================== */
