/*
 * opencog/atoms/core/Checkers.cc
 *
 * Copyright (C) 2017 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>

using namespace opencog;

// There is a way for the user to pull a fast one, and crash us here.
// avoid null-ptr deref.
static inline void check_null(const Handle& h)
{
	if (nullptr == h)
		throw InvalidParamException(TRACE_INFO,
		       "Outgoing set of Link contains null handle\n");
}

/// Provide static factory-time type checking.
/// This only performs a very simple kind of type checking;
/// it does not check deep types, nor does it check arity.

/// Check to see if every input atom is of Evaluatable type.
bool check_evaluatable(const Handle& bool_atom)
{
	// Make an exception for AndLink, its used in pattern matcher
	// in an unseemly way.
	if (bool_atom->get_type() == AND_LINK) return true;

	for (const Handle& h: bool_atom->getOutgoingSet())
	{
		check_null(h);
		Type t = h->get_type();
		// PutLinks and GetLinks cannot be type-checked statically.
		// Checking has to be deferred until runtime.
		if (PUT_LINK == t) continue;
		if (GET_LINK == t) continue;
		if (VARIABLE_NODE == t) continue;
		if (GLOB_NODE == t) continue;
		if (DEFINED_PREDICATE_NODE == t) continue;

		// Allow conjunction, disjunction and negation of
		// predicates. Since it cannot inherit from EVALUATABLE_LINK
		// (cause it's a Node) we have to add it here.
		if (h->is_type(PREDICATE_NODE)) continue;

		// Allow conjunction, disjunction and negation of concepts as
		// well, in that case these are interpreted as intersection,
		// union and complement. Since it cannot inherit from
		// EVALUATABLE_LINK (cause it's a Node) we have to add it here.
		// XXX FIXME, this is to be removed, because UnionLink,
		// IntersectionLink takes the place of OrLink, AndLink.
		if (h->is_type(CONCEPT_NODE)) continue;

		// Fucking quote links. I hate those with a passion.
		if (QUOTE_LINK == t) continue;
		if (UNQUOTE_LINK == t) continue;

		// Negation, conjunction or disjunction over Implication,
		// Equivalence, Inheritance, Similarity and their subtypes is
		// currently required by PLN for higher order reasoning. We may
		// want to forbid it in the future by maybe introducing a
		// specialized operator to explicitly map the higher order into
		// the lower order but as of today it is required.
		// XXX FIXME ... Perhaps IntersectionLink, UnionLink will
		// resolve this?
		if (h->is_type(SIMILARITY_LINK) or
		    h->is_type(MEMBER_LINK))
			continue;

		// This is used by PLN to avoid type-checking.
		if (h->is_type(DIRECTLY_EVALUATABLE_LINK)) continue;

		if (not h->is_type(EVALUATABLE_LINK)) return false;
	}
	return true;
}

/// Check to see if every input atom is of Numeric type.
bool check_numeric(const Handle& bool_atom)
{
	for (const Handle& h: bool_atom->getOutgoingSet())
	{
		check_null(h);
		Type t = h->get_type();
		// PutLinks and GetLinks cannot be type-checked statically.
		// Checking has to be deferred until runtime.
		if (PUT_LINK == t) continue;
		if (h->is_type(SATISFYING_LINK)) continue;
		if (EXECUTION_OUTPUT_LINK == t) continue;

		if (VARIABLE_NODE == t) continue;
		if (GLOB_NODE == t) continue;
		if (NUMBER_NODE == t) continue;

		// TODO - look up the schema, and make sure its numeric, also.
		if (DEFINED_SCHEMA_NODE == t) continue;

		// Oddly enough, sets of numbers are allowed.
		if (SET_LINK == t and check_numeric(h)) continue;

		// Allows to add, subtract, etc functions (used by as-moses)
		if (SCHEMA_NODE == t) continue;

		if (QUOTE_LINK == t) continue;
		if (UNQUOTE_LINK == t) continue;

		if (not h->is_type(NUMERIC_OUTPUT_LINK)) return false;
	}
	return true;
}

/// Check the type constructors that expect types as input.
bool check_type_ctors(const Handle& bool_atom)
{
	for (const Handle& h: bool_atom->getOutgoingSet())
	{
		check_null(h);
		Type t = h->get_type();
		if (h->is_type(TYPE_NODE)) continue;

		// Intervals are commonly used with GlobNodes.
		if (INTERVAL_LINK == t) continue;

		if (not h->is_type(TYPE_OUTPUT_LINK)) return false;
	}
	return true;
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init(void)
{
	classserver().addValidator(BOOLEAN_LINK, check_evaluatable);
	classserver().addValidator(NUMERIC_INPUT_LINK, check_numeric);
	classserver().addValidator(TYPE_INPUT_LINK, check_type_ctors);
}
