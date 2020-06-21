/*
 * opencog/atoms/core/TypeIntersectionLink.cc
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

#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/ClassServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "TypeIntersectionLink.h"

using namespace opencog;

void TypeIntersectionLink::init(bool glob)
{
	if (pre_analyze(glob)) return;

	// Start by adding all types...
	const TypeSet& ts = nameserver().getChildrenRecursive(VALUE);
	_simple_typeset.insert(ts.begin(), ts.end());

	for (const Handle& h : _outgoing)
		analyze(h);

	post_analyze(glob);
}

TypeIntersectionLink::TypeIntersectionLink(const HandleSeq&& oset, Type t, bool glob)
	: TypeChoice(std::move(oset), t, glob)
{
	init(glob);
}

/* ================================================================= */
/**
 * Perform static analysis on a TypeIntersectionLink
 *
 * The TypeIntersectionLink encodes the narrowest interpretation of it's
 * contents.
 *
 *       TypeIntersectionLink
 *          TypeNode  "ConceptNode"
 *          Interval
 *             Number 2
 *             Number 3
 *
 * means that the ConceptNode can be matched only two or three times, in
 * a glob match.
 */
void TypeIntersectionLink::analyze(Handle anontype)
{
	_is_untyped = false;
	Type t = anontype->get_type();

	// If its a defined type, unbundle it.
	if (DEFINED_TYPE_NODE == t)
	{
		anontype = DefineLink::get_definition(anontype);
		t = anontype->get_type();
	}

	// The anontype is either a single type name, or a list of typenames.
	if (TYPE_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();

		// Oh no! Empty set!
		if (ATOM == vt or VALUE == vt)
		{
			_simple_typeset.clear();
			return;
		}

		TypeSet unit({vt});
		_simple_typeset = set_intersection(_simple_typeset, unit);
		return;
	}

	if (TYPE_INH_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();
		const TypeSet& ts = nameserver().getChildrenRecursive(vt);
		_simple_typeset = set_intersection(_simple_typeset, ts);
		return;
	}

	if (TYPE_CO_INH_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();
		if (ATOM == vt or VALUE == vt)
		{
			_simple_typeset.clear();
			return;
		}
		const TypeSet& ts = nameserver().getParentsRecursive(vt);
		_simple_typeset = set_intersection(_simple_typeset, ts);
		return;
	}

#if 0
	if (INTERVAL_LINK == t)
	{
		_glob_interval = make_interval(anontype->getOutgoingSet());
		return;
	}

	if (SIGNATURE_LINK == t)
	{
		if (1 != anontype->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Unexpected contents in SignatureLink\n"
				"Expected arity==1, got %s", anontype->to_string().c_str());

		_deep_typeset.insert(anontype);
		return;
	}

	if (TYPE_CHOICE == t)
	{
		for (const Handle& h : anontype->getOutgoingSet())
			analyze(h);
		return;
	}

	if (VARIABLE_NODE == t)
	{
		// This is a work-around to a URE bug. The URE should be
		// using a SignatureLink, but its not. As a result, it
		// gets undefined behavior and incorect results. Too bad.
		// For now, just avoid throwing an exception. XXX FIXME.
		return;
	}

	// We need to assume that what we got is a type constant.
	// The wiki page for SignatureLink is rife with them, and
	// several unit tests use them explcitly.
	_deep_typeset.insert(anontype);
#endif
}

/* ================================================================= */

DEFINE_LINK_FACTORY(TypeIntersectionLink, TYPE_INTERSECTION_LINK);

/* ===================== END OF FILE ===================== */
