/*
 * opencog/atoms/core/TypeChoice.cc
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

#include <opencog/atoms/base/hash.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>

#include "TypeChoice.h"

using namespace opencog;

bool TypeChoice::pre_analyze(bool glob)
{
	_is_untyped = false;
	_glob_interval = default_interval(glob);

	// An empty disjunction corresponds to a bottom type.
	if (_outgoing.empty())
	{
		_simple_typeset.insert({NOTYPE});
		return true;
	}

	// Check for (TypeChoice (TypCoInh 'Atom)) which is also bottom.
	// .. as is (TypeChoice (TypCoInh 'Value))
	if (1 == _outgoing.size() and TYPE_CO_INH_NODE == _outgoing[0]->get_type())
	{
		Type vt = TypeNodeCast(_outgoing[0])->get_kind();
		if (ATOM == vt or VALUE == vt)
		{
			_simple_typeset.insert({NOTYPE});
			return true;
		}
	}

	return false;
}

void TypeChoice::post_analyze(bool glob)
{
	if (default_interval(glob) != _glob_interval and
	    0 == _simple_typeset.size() and
	    0 == _deep_typeset.size()  and
	    0 == _sect_typeset.size())
		_is_untyped = true;

	// And again... recursion in TypeChoice can still leave us empty.
	// e.g. (TypeChoice (TypeChoice (TypeChoice)))
	if (not _is_untyped and
	    default_interval(glob) == _glob_interval and
	    0 == _simple_typeset.size() and
	    0 == _deep_typeset.size()  and
	    0 == _sect_typeset.size())
	{
		_simple_typeset.insert({NOTYPE});
	}
}

void TypeChoice::init(bool glob)
{
	if (pre_analyze(glob)) return;

	_glob_interval = GlobInterval({INT_MAX, 0});

	for (const Handle& h : _outgoing)
		analyze(h);

	if (GlobInterval({INT_MAX, 0}) == _glob_interval)
		_glob_interval = default_interval(glob);

	post_analyze(glob);
}

TypeChoice::TypeChoice(const HandleSeq&& oset, Type t, bool glob)
	: Link(std::move(oset), t)
{
	// Derived classes initialize in a different way.
	if (TYPE_CHOICE != t) return;

	init(glob);
}

/* ================================================================= */

GlobInterval TypeChoice::make_interval(const HandleSeq& ivl)
{

	long lb = std::lround(NumberNodeCast(ivl[0])->get_value());
	long ub = std::lround(NumberNodeCast(ivl[1])->get_value());
	if (lb < 0) lb = 0;
	if (ub < 0) ub = SIZE_MAX;

	return std::make_pair(lb, ub);
}

// Compute the union of two glob intervals.
static inline GlobInterval extend(const GlobInterval& lhs,
                                  const GlobInterval& rhs)
{
	const auto lb = std::min(lhs.first, rhs.first);
	const auto ub = std::max(lhs.second, rhs.second);
	return GlobInterval{lb, ub};
}

/* ================================================================= */
/**
 * Perform static analysis on a TypeChoice
 *
 * The TypeChoice is a polymorphic type; for example:
 *
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
 * means that the type can be any one of those listed (choose one).
 */
void TypeChoice::analyze(Handle anontype)
{
	_is_untyped = false;
	Type t = anontype->get_type();

	// If its a defined type, unbundle it.
	if (DEFINED_TYPE_NODE == t)
	{
		anontype = DefineLink::get_definition(anontype);
		t = anontype->get_type();
	}

	// The a single type name.
	if (TYPE_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();
		if (ATOM != vt and VALUE != vt)
			_simple_typeset.insert(vt);
		else if (0 == _simple_typeset.size() and 0 == _deep_typeset.size())
			_is_untyped = true;
		return;
	}

	// This type, or children.
	if (TYPE_INH_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();
		const TypeSet& ts = nameserver().getChildrenRecursive(vt);
		_simple_typeset.insert(ts.begin(), ts.end());
		return;
	}

	// This type, or parents.
	if (TYPE_CO_INH_NODE == t)
	{
		Type vt = TypeNodeCast(anontype)->get_kind();
		if (ATOM == vt or VALUE == vt) return;
		const TypeSet& ts = nameserver().getParentsRecursive(vt);
		_simple_typeset.insert(ts.begin(), ts.end());
		return;
	}

	if (INTERVAL_LINK == t)
	{
		GlobInterval ivl = make_interval(anontype->getOutgoingSet());
		_glob_interval = extend(ivl, _glob_interval);
		return;
	}

	if (TYPE_INTERSECTION_LINK == t)
	{
		_sect_typeset.insert(TypeChoiceCast(anontype));
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
}

const GlobInterval TypeChoice::default_interval(bool glob)
{
	static GlobInterval gi = std::make_pair(1, SIZE_MAX);
	static GlobInterval vi = std::make_pair(1, 1);

	if (glob) return gi;
	return vi;
}

/* ================================================================= */

/// Return true if the type is completely unconstrained.
/// The can happen in various ways, e.g. if the user wrote
///    (TypedVariable (Variable "x") (Type 'Value))
/// or
///    (TypedVariable (Variable "x") (TypeChoice (Type 'Value)))
/// or specified TypeInh that unioned to Value.
///
/// For backwards compatibility, (TypeChoice (Type 'Atom)) is also
/// considered to be untyped.
///
bool TypeChoice::is_untyped(bool glob) const
{
	return _is_untyped and _glob_interval == default_interval(glob);
}

/* ================================================================= */

/// Return true if the glob can match a variable number of items.
/// i.e. if it is NOT an ordinary variable.
bool TypeChoice::is_globby(void) const
{
	return (1 != _glob_interval.first or 1 != _glob_interval.second);
}

/// Returns true if the glob satisfies the lower bound
/// interval restriction.
bool TypeChoice::is_lower_bound(size_t n) const
{
	return n >= _glob_interval.first;
}

/// Returns true if the glob satisfies the upper bound
/// interval restriction.
bool TypeChoice::is_upper_bound(size_t n) const
{
	return n <= _glob_interval.second or _glob_interval.second < 0;
}

/* ================================================================= */

/// Returns true if `h` satisfies the type restrictions.
bool TypeChoice::is_type(Type t) const
{
	return _is_untyped or _simple_typeset.end() != _simple_typeset.find(t);
}

/// Returns true if `h` satisfies the type restrictions.
bool TypeChoice::is_type(const ValuePtr& vp) const
{
	// If the type is not globby, then the Atom must satisfy
	// type restrictions directly.
	if (not is_globby() and is_nonglob_type(vp))
		return true;

	// Perhaps it can satisfy one of the intersection types...
	// Either as a glob, or just plainly.
	for (const TypeChoicePtr& isect : _sect_typeset)
		if (isect->is_type(vp)) return true;

	// If it's globby, then we expect a List.
	if (LIST_LINK != vp->get_type()) return false;

	const Handle& h(HandleCast(vp));
	// The list must be of the allowed size...
	const Arity num_args = h->get_arity();
	if (not is_lower_bound(num_args)) return false;
	if (not is_upper_bound(num_args)) return false;

	// Every outgoing atom in list must satisfy type restriction of var.
	for (const Handle& oh : h->getOutgoingSet())
		if (not is_nonglob_type(oh)) return false;

	return true;
}

/// Perform typecheck, ignoring possible globbiness.
bool TypeChoice::is_nonglob_type(const ValuePtr& vp) const
{
	// If the argument has the simple type, then we are good to go;
	// we are done.  Else, fall through, and see if one of the
	// others accept the match.
	if (_simple_typeset.find(vp->get_type()) != _simple_typeset.end())
		return true;

	// Deep type restrictions?
	for (const Handle& sig : _deep_typeset)
		if (value_is_type(sig, vp)) return true;

	// True, only if there were no type restrictions...
	return _is_untyped;
}

/* ================================================================= */

/// A specialized hashing function, designed so that all equivalent
/// type specifications get exactly the same hash.  To acheive this,
/// the normalized type specifications are used, rather than the raw
/// user-specified types. (The static analysis is "normalizing").

ContentHash TypeChoice::compute_hash() const
{
	ContentHash hsh = get_fvna_offset<sizeof(ContentHash)>();

	// This, and all inherited types muse use the same type...
	fnv1a_hash(hsh, TYPE_CHOICE);

	for (Type t : _simple_typeset)
		fnv1a_hash(hsh, t);

	for (const Handle& th : _deep_typeset)
		fnv1a_hash(hsh, th->get_hash());

	fnv1a_hash(hsh, _glob_interval.first);
	fnv1a_hash(hsh, _glob_interval.second);

	return hsh;
}

/* ================================================================= */

/// Return true if the other TypeChoice is equal to this one
///
/// The compare is a semantic compare, not a syntactic compare. That
/// is, the actual type restrictions are compared, and NOT the Atom
/// used to specify the restriction.
///
bool TypeChoice::is_equal(const TypeChoice& other) const
{
	// If the hashes are not equal, they can't possibly be equivalent.
	if (get_hash() != other.get_hash()) return false;

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

bool TypeChoice::operator==(const Atom& other) const
{
	if (this == &other) return true;
	if (get_hash() != other.get_hash()) return false;
	return is_equal(*TypeChoiceCast(other.get_handle()));
}

/* ================================================================= */

std::string TypeChoice::to_string(const std::string& indent) const
{
	std::string str = Link::to_string(indent);

	std::string escind = indent + "; ";
	std::string morind = escind + OC_TO_STRING_INDENT;

	str += "\n" + escind;
	str += "simple:\n" + oc_to_string(_simple_typeset, morind);
	str += "\n" + escind ;
	str += "deep:\n" + oc_to_string(_deep_typeset, morind);
	str += "\n" + escind;
	str += "intersect:\n" + oc_to_string(_sect_typeset, morind);
	str += "\n" + escind;
	str += "interval: [" + std::to_string(_glob_interval.first)
		+ ", " + std::to_string((long int) _glob_interval.second) + "]";

	return str;
}

std::string opencog::oc_to_string(const TypeChoiceSet& hset, const std::string& indent)
{
	// Cut-n-paste of oc_to_string(const HandleSet& hset)
	std::stringstream ss;
	ss << indent << "size = " << hset.size();
	size_t i = 0;
	for (const TypeChoicePtr& tcp : hset)
	{
		ss << std::endl << indent << "atom[" << i << "]:" << std::endl
			<< tcp->to_string(indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

/* ================================================================= */

DEFINE_LINK_FACTORY(TypeChoice, TYPE_CHOICE);

/* ===================== END OF FILE ===================== */
