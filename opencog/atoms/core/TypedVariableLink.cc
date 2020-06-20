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
#include <opencog/atoms/core/TypeUtils.h>

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

	_typech = createTypeChoice(HandleSeq({_outgoing[1]}),
		TYPE_CHOICE, GLOB_NODE == _outgoing[0]->get_type());
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

bool TypedVariableLink::is_untyped() const
{
	return _typech->is_untyped(GLOB_NODE == _outgoing[0]->get_type());
}

const GlobInterval TypedVariableLink::default_interval() const
{
	return _typech->default_interval(GLOB_NODE == _outgoing[0]->get_type());
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
