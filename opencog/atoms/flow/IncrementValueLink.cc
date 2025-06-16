/*
 * IncrementValueLink.cc
 *
 * Copyright (C) 2015, 2018, 2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/value/FloatValue.h>

#include "IncrementValueLink.h"

using namespace opencog;

IncrementValueLink::IncrementValueLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, INCREMENT_VALUE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an IncrementValueLink, got %s", tname.c_str());
	}

	size_t ary = _outgoing.size();
	if (INCREMENT_VALUE_LINK == t and 3 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting three atoms!");
}

// ---------------------------------------------------------------

/// When executed, this will execute the third argument to obtain
/// a FloatValue or NumberNode, and then use those numbers to atomically
/// increment the FloatValue at the indicated key on the indicated
/// first argument. The updated value is returned.
///
/// If there is no Value at the indicated key, a new FloatValue is
/// created (assuming an initial value of all zeros.) If there is an
/// existing Value, and its not a Float, then no increment is performed.
ValuePtr IncrementValueLink::execute(AtomSpace* as, bool silent)
{
	// Avoid null-pointer deref due to user error.
	// This can happen with improperly built FilterLinks.
	// See commentary in ValueOfLink::do_execute() about why
	// we want an AtomSpace.
	if (nullptr == as)
		throw RuntimeException(TRACE_INFO,
			"Expecting AtomSpace, got null pointer for %s\n",
			to_string().c_str());

	Handle ah(as->add_atom(_outgoing[0]));
	Handle ak(as->add_atom(_outgoing[1]));

	// Copy-on-write semantics means as->increment_count() might return
	// a different Atom than the current ah.
	ValuePtr vp(NumericFunctionLink::get_value(as, silent, _outgoing[2]));
	if (vp->is_type(FLOAT_VALUE))
	{
		ah = as->increment_count(ah, ak, FloatValueCast(vp)->value());
		return ah->getValue(ak);
	}

	if (vp->is_type(NUMBER_NODE))
	{
		ah = as->increment_count(ah, ak, NumberNodeCast(vp)->value());
		return ah->getValue(ak);
	}

	throw RuntimeException(TRACE_INFO,
		"Expecting numeric value, got %s\n",
		vp->to_string().c_str());
}

DEFINE_LINK_FACTORY(IncrementValueLink, INCREMENT_VALUE_LINK)

/* ===================== END OF FILE ===================== */
