/*
 * opencog/atoms/core/MapLink.cc
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#include "MapLink.h"

using namespace opencog;

void MapLink::init(void)
{
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"MapLink is expected to be arity-2 only!");

	Type tscope = _outgoing[0]->getType();
	if (not classserver().isA(tscope, SCOPE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw SyntaxException(TRACE_INFO,
			"Expecting a ScopeLink, got %s", tname.c_str());
	}

	_pattern = ScopeLinkCast(_outgoing[0]);
	_vars = &_pattern->get_variables();
	_varset = &_vars->varset;
	_is_impl = false;

	if (classserver().isA(tscope, IMPLICATION_LINK))
	{
		_is_impl = true;
		const HandleSeq& impl = _pattern->getOutgoingSet();
		if (impl.size() < 2)
			throw SyntaxException(TRACE_INFO,
				"Expecting ImplicationLink of at least size 2.");

		if (_pattern->get_body() == impl[0])
		{
			_rewrite = impl[1];
		}
		else if (_pattern->get_body() == impl[1])
		{
			if (impl.size() < 3)
				throw SyntaxException(TRACE_INFO,
					"Expecting ImplicationLink of at least size 3.");
			_rewrite = impl[2];
		}
	}

	FunctionLink::init();
}

MapLink::MapLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, oset, tv, av)
{
	init();
}

MapLink::MapLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

MapLink::MapLink(Type t, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, HandleSeq({body}), tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, oset, tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tmap = l.getType();
	if (not classserver().isA(tmap, MAP_LINK))
	{
		const std::string& tname = classserver().getTypeName(tmap);
		throw SyntaxException(TRACE_INFO,
			"Expecting a MapLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (MAP_LINK != tmap) return;
	init();
}

// ===============================================================

/// Recursive tree-compare-and-extract grounding values.
///
/// Compare the pattern tree `termpat` with the grounding tree `ground`.
/// If a variable in `termpat` corresponds with a variable in `ground`,
/// then add that correspondance pair to `valmap`. Type-checking is
/// performed during the match-up, so if the variable type does not
/// match thr ground type, false is returned.  False is also returned
/// if the trees miscompare in other ways (mismatched link arity,
/// mis-matched atom type, two conflicting groundings for the same
/// variable).
///
/// Any executable terms in `ground` are executed prior to comparison.
///
/// If false is returned, the contents of valmap are invalid. If true
/// is returned, valmap contains the extracted values.
///
bool MapLink::extract(const Handle& termpat,
                      const Handle& ground,
                      std::map<Handle,Handle>& valmap,
                      AtomSpace* scratch) const
{
	if (termpat == ground) return true;

	Type t = termpat->getType();
	// If its a variable, then see if we know its value already;
	// If not, then record it.
	if (VARIABLE_NODE == t and 0 < _varset->count(termpat))
	{
		auto val = valmap.find(termpat);
		if (valmap.end() != val)
		{
			// If we already have a value, the value must be identical.
			return (val->second == ground);
		}

		// Check the type of the value.
		if (not _vars->is_type(termpat, ground)) return false;

		// If we are here, everything looks good. Record and return.
		valmap.emplace(std::make_pair(termpat, ground));
		return true;
	}

	// Whever they are, the type must agree.
	if (t != ground->getType()) return false;

	// If they are (non-variable) nodes, they must be identical.
	LinkPtr tlp(LinkCast(termpat));
	if (nullptr == tlp)
		return (termpat == ground);

	LinkPtr glp(LinkCast(ground));
	const HandleSeq& tlo = tlp->getOutgoingSet();
	const HandleSeq& glo = glp->getOutgoingSet();
	size_t sz = tlo.size();
	if (glo.size() != sz) return false;

	// Compare links side-by-side.
	for (size_t i=0; i<sz; i++)
	{
		if (not extract(tlo[i], glo[i], valmap, scratch))
			return false;
	}

	return true;
}

Handle MapLink::rewrite_one(const Handle& term, AtomSpace* scratch) const
{
	std::map<Handle,Handle> valmap;

	// Extract values for variables.
	if (not extract(_pattern->get_body(), term, valmap, scratch))
		return Handle::UNDEFINED;

	// Make sure each variable is grounded.
	HandleSeq valseq;
	for (const Handle& var : _vars->varseq)
	{
		auto valpair = valmap.find(var);
		if (valmap.end() == valpair)
			return Handle::UNDEFINED;
		valseq.emplace_back(valpair->second);
	}

	// Perform substitution, if it's an ImplicationLink
	if (_is_impl)
	{
		return _vars->substitute(_rewrite, valseq);
	}

	// Wrap up the result in a list only if there is more than one
	// variable.
	if (1 < valseq.size())
		return Handle(createLink(LIST_LINK, valseq));

	return valseq[0];
}

Handle MapLink::execute(AtomSpace* scratch) const
{
	// Handle three different cases.
	// If there is a single value, apply the map to the single value.
	// If there is a set of values, apply the map to the set.
	// If there is a link of values, apply the map to the link.
	Type argtype = _outgoing[1]->getType();
	if (SET_LINK == argtype or LIST_LINK == argtype)
	{
		HandleSeq remap;
		LinkPtr lp(LinkCast(_outgoing[1]));
		for (const Handle& h : lp->getOutgoingSet())
		{
			Handle mone = rewrite_one(h, scratch);
			if (nullptr != mone) remap.emplace_back(mone);
		}
		return Handle(createLink(argtype, remap));
	}

	// Its a singleton. Just remap that.
	return rewrite_one(_outgoing[1], scratch);
}

/* ===================== END OF FILE ===================== */
