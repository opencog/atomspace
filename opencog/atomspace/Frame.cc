/*
 * opencog/atomspace/Frame.cc
 *
 * Copyright (c) 2022 Linas Vepstas
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

#include <opencog/atoms/atom_types/NameServer.h>

#include "Frame.h"

using namespace opencog;

void Frame::init()
{
	if (not nameserver().isA(_type, FRAME))
		throw InvalidParamException(TRACE_INFO, "Not a Frame!");

	// Set up the incoming set.
	keep_incoming_set();
	install();
}

Frame::~Frame()
{
	remove();
}

/// Place `this` into the incoming set of each outgoing frame
///
void Frame::install()
{
	Handle llc(get_handle());
	for (Handle& h : _outgoing)
		h->insert_atom(llc);
}

void Frame::remove()
{
	Handle lll(get_handle());
	for (Handle& h : _outgoing)
		h->remove_atom(lll);
}
