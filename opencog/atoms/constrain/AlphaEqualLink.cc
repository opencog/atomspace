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
	// The whack case,
	if (1 == _outgoing.size()) return true;

	Handle hid;
	Variables vid;
	for (Handle h : _outgoing)
	{
		if (h->is_executable())
		{
			ValuePtr vp(h->execute(as, silent));
			if (not vp->is_atom()) return false;
			h = HandleCast(vp);
		}
		if (nullptr == hid)
		{
			hid = h;
			vid.find_variables(h);
			continue;
		}

		// Are they strictly equal? Good!
		if (hid == h) continue;

		// Not strictly equal. Are they alpha convertible?
		Variables v;
		v.find_variables(h);

		// If the variables are not alpha-convertable,
		// then there is no possibility of equality.
		if (not v.is_equal(vid))
			return false;

		// Actually alpha-convert, and compare.
		Handle ha = v.substitute_nocheck(h, vid.varseq, silent);
		if (not (*hid == *ha))
			return false;
	}
	return true;
}

DEFINE_LINK_FACTORY(AlphaEqualLink, ALPHA_EQUAL_LINK);

/* ===================== END OF FILE ===================== */
