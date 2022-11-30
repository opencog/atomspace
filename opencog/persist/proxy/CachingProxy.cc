/*
 * CachingProxy.cc
 *
 * Copyright (C) 2022 Linas Vepstas
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

#include <opencog/persist/proxy/CachingProxy.h>

using namespace opencog;

CachingProxy::CachingProxy(const std::string&& name)
	: ProxyNode(CACHING_PROXY_NODE, std::move(name))
{
	init();
}

CachingProxy::CachingProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

CachingProxy::~CachingProxy()
{
}

void CachingProxy::init(void)
{
	have_loadType = true;
	have_fetchIncomingByType = true;
	have_fetchIncomingSet = true;
	have_getAtom = true;
	have_loadValue = true;
}

// Get our configuration from the ProxyParameterLink we live in.
// XXX TODO Add support for expriation times, limited AtomSpace
// size and whatever otehr whizzy caching ideas we might want.
void CachingProxy::open(void)
{
	// Can't use this if there are more parameters...
	StorageNodeSeq rdr = setup();
	if (1 != rdr.size())
		throw RuntimeException(TRACE_INFO,
			"Excpecting exactly one StoraeNode");

	_reader = rdr[0];
	_reader->open();
}

void CachingProxy::close(void)
{
	_reader->close();
	_reader = nullptr;
}

#define CHECK_OPEN if (nullptr == _reader) return;

void CachingProxy::getAtom(const Handle& h)
{
	CHECK_OPEN;

	_reader->fetch_atom(h);
	DOWN;
}

void CachingProxy::fetchIncomingSet(AtomSpace* as, const Handle& h)
{
	CHECK_OPEN;
	_reader->fetch_incoming_set(h, false, as);
	DOWN;
}

void CachingProxy::fetchIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	CHECK_OPEN;
	_reader->fetch_incoming_by_type(h, t, as);
	DOWN;
}

void CachingProxy::loadValue(const Handle& atom, const Handle& key)
{
	CHECK_OPEN;
	_reader->fetch_value(atom, key);
	DOWN;
}

void CachingProxy::loadType(AtomSpace* as, Type t)
{
	CHECK_OPEN;
	_reader->fetch_all_atoms_of_type(t, as);
	DOWN;
}

void CachingProxy::barrier(AtomSpace* as)
{
	_reader->barrier(as);
}

DEFINE_NODE_FACTORY(CachingProxy, CACHING_PROXY_NODE)
