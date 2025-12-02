/*
 * opencog/atoms/constrain/AlphaEqualLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics LLC
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/scope/Variables.h>

#include "AlphaEqualLink.h"

using namespace opencog;

AlphaEqualLink::AlphaEqualLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, ALPHA_EQUAL_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an AlphaEqualLink");
}

/* ================================================================= */

/// Check for alpha equivalence.
/// Two atoms are alpha-equivalent if they are structurally identical
/// after variable renaming (alpha-conversion).
bool AlphaEqualLink::bevaluate(AtomSpace* as, bool silent)
{
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
		     "AlphaEqualLink expects two arguments");

	Handle h0(_outgoing[0]);
	if (h0->is_executable())
	{
		ValuePtr vp(h0->execute(as, silent));
		if (not vp->is_atom()) return false;
		h0 = HandleCast(vp);
	}

	Handle h1(_outgoing[1]);
	if (h1->is_executable())
	{
		ValuePtr vp(h1->execute(as, silent));
		if (not vp->is_atom()) return false;
		h1 = HandleCast(vp);
	}

	// Are they strictly equal? Good!
	if (h0 == h1)
		return true;

	// Not strictly equal. Are they alpha convertible?
	Variables v0, v1;
	v0.find_variables(h0);
	v1.find_variables(h1);

	// If the variables are not alpha-convertable, then
	// there is no possibility of equality.
	if (not v0.is_equal(v1))
		return false;

	// Actually alpha-convert, and compare.
	Handle h1a = v1.substitute_nocheck(h1, v0.varseq, silent);
	return (*h0 == *h1a);
}

DEFINE_LINK_FACTORY(AlphaEqualLink, ALPHA_EQUAL_LINK);

/* ===================== END OF FILE ===================== */
