/*
 * opencog/persist/sql/SQLBackingStore.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Linas Vepstas <linasvepstas@gmail.com>
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
#include <opencog/guile/SchemePrimitive.h>

#include "SQLBackingStore.h"

using namespace opencog;

SQLBackingStore::SQLBackingStore()
{
	_store = NULL;
}

void SQLBackingStore::set_store(AtomStorage *as)
{
	_store = as;
}

NodePtr SQLBackingStore::getNode(Type t, const char *name) const
{
	return _store->getNode(t, name);
}

LinkPtr SQLBackingStore::getLink(Type t, const std::vector<Handle>& oset) const
{
	return _store->getLink(t, oset);
}

AtomPtr SQLBackingStore::getAtom(UUID uuid) const
{
	return _store->getAtom(uuid);
}

HandleSeq SQLBackingStore::getIncomingSet(Handle h) const
{
	return _store->getIncomingSet(h);
}

void SQLBackingStore::storeAtom(Handle h)
{
	_store->storeAtom(h);
}

void SQLBackingStore::loadType(AtomTable& at, Type t)
{
	_store->loadType(at, t);
}

void SQLBackingStore::barrier()
{
	_store->flushStoreQueue();
}

