/*
 * opencog/atoms/reduct/FoldLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include <limits>

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "FoldLink.h"

using namespace opencog;

FoldLink::FoldLink(const HandleSeq& oset, Type t)
    : FunctionLink(oset, t)
{
	init();
}

FoldLink::FoldLink(Type t, const Handle& a, const Handle& b)
    : FunctionLink(t, a, b)
{
	init();
}

FoldLink::FoldLink(const Link& l)
    : FunctionLink(l)
{
	init();
}

void FoldLink::init(void)
{
	Type tscope = get_type();
	if (not classserver().isA(tscope, FOLD_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FoldLink");
}

// ===============================================================

/// reduce() -- reduce the expression by summing constants, etc.
///
/// No actual black-box evaluation or execution is performed. Only
/// clearbox reductions are performed.
///
/// Examples: the reduct of (FoldLink (NumberNode 2) (NumberNode 2))
/// is (NumberNode 4) -- its just a constant.
///
/// The reduct of (FoldLink (VariableNode "$x") (NumberNode 0)) is
/// (VariableNode "$x"), because adding zero to anything yeilds the
/// thing itself.
///
/// This routine is pretending to be more general, though, than simply
/// reducing numeric expressions.  It makes the following assumptions
/// and performs the following actions:
///
/// 1) Two neighboring elements of the same type can always be kons'ed
///    together with each-other.  That is, kons is called on two
///    neighbors that have the same type. That is, this assumes that
///    the list has the associative property, so that neighboring
///    elements can always be cons'ed together.
/// 2) It does not asssume the commutative property.
/// 3) If distributive_type is set, then kons is called when it seems
///    that one neigboring element might distribute into the next.
///    This is vaguely hacky, and is used to implement distributivity
///    of multiplication over addition.
///
/// For something as simple as the above, the code below is
/// annoyingly complicated.  This is certainly not an efficient,
/// effective way to build a computer algebra system.  It works, its
/// just barely good enough for single-variable arithmetic, but that's
/// all.  For general reduction tasks, there are two choices:
///
/// A) Convert atoms to some other CAS format, reduce that, and then
///    convert back to atoms.
///
/// B) Figure out why the atomspace is so ungainly, and fix it so that
///    it is both easy (easier) to use, and also is high-performance.
///
/// Obviously, B) is much harder than A) but is probably more important.
Handle FoldLink::reduce(void) const
{
	Handle expr = knil;

	// Loop over the outgoing set, kons'ing away.
	// This is right to left.
	size_t osz = _outgoing.size();
	for (int i = osz-1; 0 <= i; i--)
	{
		Handle h(_outgoing[i]);

		Type t = h->get_type();
		if (classserver().isA(t, FOLD_LINK))
		{
			auto fact = classserver().getFactory(t);
			FoldLinkPtr fff(FoldLinkCast((*fact)(h)));
			h = fff->reduce();
		}
		expr = kons(h, expr);
	}

	return expr;
}

// ===========================================================
