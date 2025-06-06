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

#include <opencog/atoms/base/ClassServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "TypeIntersectionLink.h"

using namespace opencog;

/**
 * \return s1 inter s2
 * s1 and s2 must be sorted
 */
template<typename Set>
Set set_intersection(const Set& s1, const Set& s2)
{
	Set res;
	std::set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(),
	                      std::inserter(res, res.end()));
	return res;
}

void TypeIntersectionLink::init(bool glob)
{
	if (pre_analyze(glob)) return;

	// Start by adding all types...
	const TypeSet& ts = nameserver().getChildrenRecursive(VALUE);
	_simple_typeset.insert(ts.begin(), ts.end());
	_glob_interval = GlobInterval{0, SIZE_MAX};
	bool touched = false;

	for (const Handle& h : _outgoing)
		analyze(h, touched);

	if (not touched)
		_glob_interval = default_interval(glob);

	post_analyze(glob);
}

TypeIntersectionLink::TypeIntersectionLink(const HandleSeq&& oset, Type t, bool glob)
	: TypeChoice(std::move(oset), t, glob)
{
	init(glob);
}

/* ================================================================= */

// Compute the intersection of two glob intervals.
// Currently, zero is the same as empty-set.
static inline GlobInterval intersect(const GlobInterval& lhs,
                                     const GlobInterval& rhs)
{
	if (TypeChoice::is_empty(lhs) or TypeChoice::is_empty(rhs))
		return GlobInterval{1, 0};

	const auto lb = std::max(lhs.first, rhs.first);
	const auto ub = std::min(lhs.second, rhs.second);
	return GlobInterval{lb, ub};
}

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
void TypeIntersectionLink::analyze(Handle anontype, bool& touched)
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
		TypeSet ts(nameserver().getChildrenRecursive(vt));
		if (ATOM != vt and VALUE != vt)
			ts.insert(vt);
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
		TypeSet ts(nameserver().getParentsRecursive(vt));
		ts.insert(vt);
		_simple_typeset = set_intersection(_simple_typeset, ts);
		return;
	}

	if (INTERVAL_LINK == t)
	{
		touched = true;
		GlobInterval ivl = make_interval(anontype->getOutgoingSet());
		_glob_interval = intersect(_glob_interval, ivl);
		return;
	}

	if (nameserver().isA(t, TYPE_CHOICE))
	{
		// Oh no! Empty set!
		if (anontype->get_arity() == 0)
		{
			_simple_typeset.clear();
			return;
		}

		TypeChoicePtr tcp(TypeChoiceCast(anontype));
		const TypeSet& ts = tcp->get_simple_typeset();
		_simple_typeset = set_intersection(_simple_typeset, ts);

		if (tcp->get_deep_typeset().size() == 0)
			_deep_typeset.clear();
		else if (0 < _deep_typeset.size())
			throw RuntimeException(TRACE_INFO,
				"Intersection of deep types not implemented!");

		touched = true;
		_glob_interval = intersect(_glob_interval, tcp->get_glob_interval());
		return;
	}

	throw RuntimeException(TRACE_INFO,
		"Intersection of signatures or type constants not implemented!");
}

/* ================================================================= */

DEFINE_LINK_FACTORY(TypeIntersectionLink, TYPE_INTERSECTION_LINK);

/* ===================== END OF FILE ===================== */
