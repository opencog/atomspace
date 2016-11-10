/*
 * opencog/atoms/base/Quotation.h
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

#include "Quotation.h"

using namespace opencog;

Quotation::Quotation(int ql, bool lq)
	: _quotation_level(ql), _local_quote(lq) {}

int Quotation::quotation_level() const
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

void Quotation::update(Type t)
{
	bool is_unquoted = not is_quoted();
	bool is_locally_unquoted = not is_locally_quoted();
	
	// Locally quoted iff it is an unquoted LocalQuote
	_local_quote = is_unquoted and LOCAL_QUOTE_LINK == t;

	// Increment or decrement quotation level if locally unquoted
	if (is_locally_unquoted) {
		if (QUOTE_LINK == t) _quotation_level++;
	    else if (UNQUOTE_LINK == t) _quotation_level--;
    }
}

bool Quotation::consumable_quotation(Type t)
{
	return (not is_quoted() and (LOCAL_QUOTE_LINK == t or QUOTE_LINK == t))
		or (quotation_level() == 1 and UNQUOTE_LINK == t);
}
