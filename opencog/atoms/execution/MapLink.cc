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
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atomutils/FindUtils.h>

#include "MapLink.h"

using namespace opencog;

void MapLink::init(void)
{
	// Maps consist of a function, and the data to apply the function to.
	// The function can be explicit (inheriting from ScopeLink) or
	// implicit (we automatically fish out free variables).
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"MapLink is expected to be arity-2 only!");

	// First argument must be a function of some kind.  All functions
	// are specified using a ScopeLink, to bind the input-variables.
	Type tscope = _outgoing[0]->getType();
	if (classserver().isA(tscope, SCOPE_LINK))
	{
		_pattern = ScopeLinkCast(_outgoing[0]);
	}
	else
	{
		const Handle& body = _outgoing[0];
		FreeVariables fv;
		fv.find_variables(body);
		Handle decl(createVariableList(fv.varseq));
		_pattern = createScopeLink(decl, body);
	}
	_mvars = &_pattern->get_variables();
	_varset = &_mvars->varset;

	// ImplicationScopeLinks are a special type of ScopeLink.  They specify
	// a re-write that should be performed.  Viz, ImplicationScopeLinks are
	// of the form P(x)->Q(x).  Here, the `_rewrite` is the Q(x)
	_is_impl = false;
	if (classserver().isA(tscope, IMPLICATION_SCOPE_LINK))
	{
		_is_impl = true;
		const HandleSeq& impl = _pattern->getOutgoingSet();
		if (impl.size() < 2)
			throw SyntaxException(TRACE_INFO,
				"Expecting ImplicationLink of at least size 2.");

		// ImplicationScopeLinks have arity 2 only if they have no type
		// constraints, else they have arity 3.  That is, an
		// ImplicationLink is either P(x)->Q(x) or its T(x) P(x)->Q(x)
		// where T(x) is the type constraints on the variables.
		if (_pattern->get_body() == impl[0])
		{
			_rewrite = impl[1];
		}
		else if (_pattern->get_body() == impl[1])
		{
			if (impl.size() < 3)
				throw SyntaxException(TRACE_INFO,
					"Expecting ImplicationScopeLink of at least size 3.");
			_rewrite = impl[2];
		}
	}

	// Locate all GlobNodes in the pattern
	FindAtoms fgn(GLOB_NODE, true);
	fgn.search_set(_pattern->get_body());
	for (const Handle& sh : fgn.least_holders)
		_globby_terms.insert(sh);

	// FunctionLink::init() will extract free variables. But we don't
	// really need this for anything, so don't run it.
	// FunctionLink::init();
}

MapLink::MapLink(const Handle& vars, const Handle& body)
	: FunctionLink(HandleSeq({vars, body}), MAP_LINK)
{
	init();
}

MapLink::MapLink(Type t, const Handle& body)
	: FunctionLink(HandleSeq({body}), t)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(const HandleSeq& oset, Type t)
	: FunctionLink(oset, t)
{
	if (not classserver().isA(t, MAP_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a MapLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(const Link &l)
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
/// match the ground type, false is returned.  False is also returned
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
                      HandleMap& valmap,
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
		if (not _mvars->is_type(termpat, ground)) return false;

		// If we are here, everything looks good. Record and return.
		valmap.emplace(std::make_pair(termpat, ground));
		return true;
	}

	if (GLOB_NODE == t and 0 < _varset->count(termpat))
	{
		// Check the type of the value.
		if (not _mvars->is_type(termpat, ground)) return false;
		return true;
	}

	// Special-case for ChoiceLinks in the body of the pattern.
	// This dangles one foot over the edge of a slippery slope,
	// of analyzing the body of the map and special-casing. Not
	// sure if this is a good idea, or a bad idea...
	if (CHOICE_LINK == t)
	{
		for (const Handle& choice : termpat->getOutgoingSet())
		{
			if (extract(choice, ground, valmap, scratch))
				return true;
		}
		return false;
	}

	// Whatever they are, the type must agree.
	if (t != ground->getType()) return false;

	// If they are (non-variable) nodes, they must be identical.
	if (not termpat->isLink())
		return (termpat == ground);

	const HandleSeq& tlo = termpat->getOutgoingSet();
	const HandleSeq& glo = ground->getOutgoingSet();
	size_t tsz = tlo.size();
	size_t gsz = glo.size();

	// If no glob nodes, just compare links side-by-side.
	if (0 == _globby_terms.count(termpat))
	{
		// If the sizes are mismatched, should we do a fuzzy match?
		if (gsz != tsz) return false;
		for (size_t i=0; i<tsz; i++)
		{
			if (not extract(tlo[i], glo[i], valmap, scratch))
				return false;
		}

		return true;
	}

	// If we are here, there is a glob node in the pattern.  A glob can
	// match one or more atoms in a row. Thus, we have a more
	// complicated search ...
	size_t ip=0, jg=0;
	for (ip=0, jg=0; ip<tsz and jg<gsz; ip++, jg++)
	{
		Type ptype = tlo[ip]->getType();
		if (GLOB_NODE == ptype)
		{
			HandleSeq glob_seq;
			Handle glob(tlo[ip]);
			// Globs at the end are handled differently than globs
			// which are followed by other stuff. So, is there
			// anything after the glob?
			Handle post_glob;
			bool have_post = false;
			if (ip+1 < tsz)
			{
				have_post = true;
				post_glob = tlo[ip+1];
			}

			// Match at least one.
			bool tc = extract(glob, glo[jg], valmap, scratch);
			if (not tc) return false;

			glob_seq.push_back(glo[jg]);
			jg++;

			// Can we match more?
			while (tc and jg<gsz)
			{
				if (have_post)
				{
					// If the atom after the glob matches, then we are done.
					tc = extract(post_glob, glo[jg], valmap, scratch);
					if (tc) break;
				}
				tc = extract(glob, glo[jg], valmap, scratch);
				if (tc) glob_seq.push_back(glo[jg]);
				jg ++;
			}
			jg --;
			if (not tc)
			{
				return false;
			}

			// If we already have a value, the value must be identical.
			auto val = valmap.find(glob);
			if (valmap.end() != val)
			{
				// Have to have same arity and contents.
				const Handle& already = val->second;
				const HandleSeq& alo = already->getOutgoingSet();
				size_t asz = alo.size();
				if (asz != glob_seq.size()) return false;
				for (size_t i=0; i< asz; i++)
				{
					if (glob_seq[i] != alo[i]) return false;
				}
				return true;
			}

			// If we are here, we've got a match. Record it.
			LinkPtr glp(createLink(glob_seq, LIST_LINK));
			valmap.emplace(std::make_pair(glob, glp->getHandle()));
		}
		else
		{
			// If we are here, we are not comparing to a glob.
			if (not extract(tlo[ip], glo[jg], valmap, scratch))
				return false;
		}
	}
	return (ip == tsz) and (jg == gsz);
}

Handle MapLink::rewrite_one(const Handle& cterm, AtomSpace* scratch) const
{
	Instantiator inst(scratch);
	Handle term(inst.execute(cterm));

	// Extract values for variables.
	HandleMap valmap;
	if (not extract(_pattern->get_body(), term, valmap, scratch))
		return Handle::UNDEFINED;

	// Make sure each variable is grounded. Place the groundings
	// into a sequence, for easy access. Not all variables need to
	// be grounded, because the re-write might not use all variables.
	// If it does use a variable, it must have a grounding.
	bool partial = false;
	HandleSeq valseq;
	for (const Handle& var : _mvars->varseq)
	{
		auto valpair = valmap.find(var);
		if (valmap.end() == valpair)
		{
			partial = true;
			valseq.emplace_back(Handle::UNDEFINED);
		}
		else
			valseq.emplace_back(valpair->second);
	}

	// Perform substitution, if it's an ImplicationScopeLink
	if (_is_impl)
	{
		// Beta reduce, and execute. No type-checking during
		// beta-reduction; we've already done that.
		Handle red(_mvars->substitute_nocheck(_rewrite, valseq));
		return inst.execute(red);
	}

	// Make sure each variable is grounded. (for real, this time)
	if (partial)
		return Handle::UNDEFINED;

	// Wrap up the result in a list only if there is more than one
	// variable.
	size_t nv = valseq.size();
	if (1 < nv)
		return Handle(createLink(valseq, LIST_LINK));
	else if (1 == nv)
		return valseq[0];
	return Handle::UNDEFINED;
}

Handle MapLink::execute(AtomSpace* scratch) const
{
	const Handle& valh = _outgoing[1];

	// Handle three different cases.
	// If there is a single value, apply the map to the single value.
	// If there is a set of values, apply the map to the set.
	// If there is a list of values, apply the map to the list.
	Type argtype = valh->getType();
	if (SET_LINK == argtype or LIST_LINK == argtype)
	{
		HandleSeq remap;
		for (const Handle& h : valh->getOutgoingSet())
		{
			Handle mone = rewrite_one(h, scratch);
			if (nullptr != mone) remap.emplace_back(mone);
		}
		return Handle(createLink(remap, argtype));
	}

	// Its a singleton. Just remap that.
	return rewrite_one(valh, scratch);
}

DEFINE_LINK_FACTORY(MapLink, MAP_LINK)

/* ===================== END OF FILE ===================== */
