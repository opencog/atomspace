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

	Type dtype = _outgoing[1]->get_type();
	if (not nameserver().isA(dtype, TYPE_NODE) and
	    DEFINED_TYPE_NODE != dtype and
	    TYPE_CHOICE != dtype and
	    TYPE_SET_LINK != dtype and
	    SIGNATURE_LINK != dtype and
	    INTERVAL_LINK != dtype and
	    ARROW_LINK != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting type defintion, got %s",
				nameserver().getTypeName(dtype).c_str());

	_glob_interval = std::make_pair(0, SIZE_MAX);
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
	Handle varname(_outgoing[0]);
	Handle vartype(_outgoing[1]);

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

	if (0 < intervals.size())
	{
		long lb = std::lround(NumberNodeCast(intervals[0])->get_value());
		long ub = std::lround(NumberNodeCast(intervals[1])->get_value());
		if (lb < 0) lb = 0;
		if (ub < 0) ub = SIZE_MAX;

		_glob_interval = std::make_pair(lb, ub);
	}
}

/* ================================================================= */

DEFINE_LINK_FACTORY(TypedVariableLink, TYPED_VARIABLE_LINK);

/* ===================== END OF FILE ===================== */
