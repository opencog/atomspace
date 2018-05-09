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

#include "SQLBackingStore.h"

using namespace opencog;

SQLBackingStore::SQLBackingStore()
{
	_store = nullptr;
}

void SQLBackingStore::set_store(AtomStorage *as)
{
	_store = as;
}

Handle SQLBackingStore::getNode(Type t, const char *name) const
{
	if (nullptr == _store) return Handle();
	return _store->getNode(t, name);
}

Handle SQLBackingStore::getLink(Type t, const HandleSeq& hs) const
{
	if (nullptr == _store) return Handle();
	return _store->getLink(t, hs);
}

void SQLBackingStore::getIncomingSet(AtomTable& table, const Handle& h)
{
	if (_store) _store->getIncomingSet(table, h);
}

void SQLBackingStore::getIncomingByType(AtomTable& table, const Handle& h, Type t)
{
	if (_store) _store->getIncomingByType(table, h, t);
}

void SQLBackingStore::getValuations(AtomTable& table, const Handle& key, bool get_all)
{
	if (_store) _store->getValuations(table, key, get_all);
}

void SQLBackingStore::storeAtom(const Handle& h)
{
	if (_store) _store->storeAtom(h);
}

void SQLBackingStore::removeAtom(const Handle& h, bool recursive)
{
	if (_store) _store->removeAtom(h, recursive);
}

void SQLBackingStore::loadType(AtomTable& at, Type t)
{
	if (_store) _store->loadType(at, t);
}

void SQLBackingStore::barrier()
{
	if (_store) _store->flushStoreQueue();
}

void SQLBackingStore::registerWith(AtomSpace* as)
{
	_store->registerWith(as);
	BackingStore::registerWith(as);
}

void SQLBackingStore::unregisterWith(AtomSpace* as)
{
	// First disconnect, and only then shut down.
	AtomStorage* sto = _store;
	_store = nullptr;

	BackingStore::unregisterWith(as);
	sto->unregisterWith(as);
}
