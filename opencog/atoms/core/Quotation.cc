/*
 * opencog/atoms/core/Quotation.cc
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#include <sstream>
#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/Atom.h>
#include "Quotation.h"

namespace opencog {

Quotation::Quotation(int ql, bool lq)
	: _quotation_level(ql), _local_quote(lq) {}

int Quotation::level() const
{
	return _quotation_level;
}

bool Quotation::is_locally_quoted() const
{
	return _local_quote;
}

bool Quotation::is_quoted() const
{
	return _quotation_level != 0 or is_locally_quoted();
}

bool Quotation::is_unquoted() const
{
	return not is_quoted();
}

bool Quotation::is_quotation_type(Type t)
{
	return QUOTE_LINK == t or UNQUOTE_LINK == t or LOCAL_QUOTE_LINK == t;
}

bool Quotation::consumable(Type t) const
{
	return (not is_quoted() and (LOCAL_QUOTE_LINK == t or QUOTE_LINK == t))
		or (level() == 1 and UNQUOTE_LINK == t);
}

void Quotation::update(Type t)
{
	bool is_unquoted = not is_quoted();
	bool is_locally_unquoted = not is_locally_quoted();
	
	// Locally quoted iff it is an unquoted LocalQuote
	_local_quote = is_unquoted and LOCAL_QUOTE_LINK == t;

	// Increment or decrement quotation level if locally unquoted
	if (is_locally_unquoted)
	{
		if (QUOTE_LINK == t) _quotation_level++;
		else if (UNQUOTE_LINK == t)
		{
			// Well, it would make sense to check for unbalanced quotes,
			// in theory. In practice, this does not quite work, because
			// this triggers when quoted FunctionLinks are inserted into
			// the AtomSpace. FunctionLinks are built on FreeLink which
			// searches for free variables, which often sees one unquote
			// too many during atomspace insertion. I don't see any easy
			// fixes at this time.
			if (0 == _quotation_level)
				throw RuntimeException(TRACE_INFO, "Unbalanced quotes!");
			_quotation_level--;
		}
    }
}

// This does what `class Quotation`, but in reverse. Given some
// some Atom `h`, return true if there are more Unquotes below than
// quotes, from which we infer that `h` must be in a quoted context.
static bool unquoted_below_rec(const Handle& h, bool skip, bool skiplo)
{
	Type t = h->get_type();
	if (nameserver().isA(t, NODE)) return false;

	if (UNQUOTE_LINK == t)
	{
		if (not skip and not skiplo) return true;
		skip = false;
	}

	for (const Handle& ho: h->getOutgoingSet())
	{
		bool unq;
		Type to = ho->get_type();
		if (QUOTE_LINK == to)
		{
			unq = unquoted_below_rec(ho, true, false);
		}
		else if (LOCAL_QUOTE_LINK == to)
		{
			unq = unquoted_below_rec(ho, skip, true);
		}
		else
		{
			unq = unquoted_below_rec(ho, skip, false);
		}
		if (unq) return true;
	}
	return false;
}

bool unquoted_below(const Handle& h)
{
	bool skip = false;
	bool skiplo = false;
	return unquoted_below_rec(h, skip, skiplo);
}

bool unquoted_below(const HandleSeq& hs)
{
	for (const Handle& h: hs)
		if (unquoted_below(h)) return true;
	return false;
}

bool Quotation::operator<(const Quotation& quotation) const
{
	return (_quotation_level < quotation._quotation_level)
		or ((_quotation_level == quotation._quotation_level)
		    and (_local_quote < quotation._local_quote));
}

bool Quotation::operator==(const Quotation& quotation) const
{
	return (_quotation_level == quotation._quotation_level)
		and (_local_quote == quotation._local_quote);
}

std::string Quotation::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent
	   << "{level = " << _quotation_level
	   << ", local = " << _local_quote << "}";
	return ss.str();
}

std::string oc_to_string(const Quotation& quotation, const std::string& indent)
{
	return quotation.to_string(indent);
}

} // namespace opencog
