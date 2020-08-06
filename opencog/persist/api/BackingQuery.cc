/*
 * opencog/persist/api/BackingQuery.cc
 *
 * Copyright (C) 2020 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>

using namespace opencog;

// ==========================================================

void BackingStore::getIncomingSet(AtomSpace* as, const Handle& h)
{
	getIncomingSet(as->get_atomtable(), h);
}

void BackingStore::getIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	getIncomingByType(as->get_atomtable(), h, t);
}

void BackingStore::runQuery(const Handle& query, const Handle& key,
                            const Handle& metadata_key, bool fresh)
{
	throw IOException(TRACE_INFO, "Not implemented!");
}
