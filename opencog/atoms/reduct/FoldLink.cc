/*
 * opencog/atoms/reduct/FoldLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include "FoldLink.h"

using namespace opencog;

FoldLink::FoldLink(const HandleSeq&& oset, Type t)
    : FunctionLink(std::move(oset), t)
{
	init();
}

void FoldLink::init(void)
{
	Type tscope = get_type();
	if (FOLD_LINK == tscope)
		throw InvalidParamException(TRACE_INFO,
			"FoldLinks are private and cannot be instantiated.");

	if (not nameserver().isA(tscope, FOLD_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FoldLink");
}

// ===============================================================

/// delta_reduce() -- delta-reduce a right-fold by recursively
/// calling kons.  Recall the definition of delta-reduction: it is
/// that operation by which a function with arguments is replaced
/// by the value that function would have for these values.
/// For example, the delta-reduction of 2+2 is 4.
///
/// This performs a fold-right, so that (for example)
/// (Plus A B C) is expanded into (Plus A (Plus B (Plus C 0)))
/// Note that srfi-1 default fold is fold-left.
/// Note that the PlusLink, TimesLink etc. depend delciately
/// on this being fold-right; otherwise, they break.
ValuePtr FoldLink::delta_reduce(AtomSpace* as, bool silent) const
{
	ValuePtr expr = knil;
	bool unpack = false;

	// Loop over the outgoing set, kons'ing away.
	// This is right to left; so this is fold-right as explained above.
	size_t osz = _outgoing.size();
	for (int i = osz-1; 0 <= i; i--)
	{
		// Some extra crazy-making for unary Minus.
		// The issue here is (Minus (ListValue ...)) is mis-understood
		// to be a unary Minus, which the MinusLink ctor turns into
		// (Minus (Number 0) (ListValue ...)) which then comes out wrong.
		// So we undo that with this if-statement.
		if (unpack and (0 == i) and content_eq(_outgoing[0], knil)) break;

		ValuePtr vi(FunctionLink::get_value(as, silent,  _outgoing[i]));
		if (not vi->is_type(LINK_VALUE))
		{
			expr = kons(as, silent, vi, expr);
			continue;
		}

		// One more loop.
		const ValueSeq& vseq = LinkValueCast(vi)->value();
		size_t vlen = vseq.size();
		for (int j = vlen-1; 0 <= j; j--)
		{
			expr = kons(as, silent, vseq[j], expr);
			unpack = true;
		}
	}

	return expr;
}

// ===========================================================
