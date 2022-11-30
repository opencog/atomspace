/*
 * ReadThruProxy.cc
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

#include <opencog/persist/proxy/ReadThruProxy.h>

using namespace opencog;

ReadThruProxy::ReadThruProxy(const std::string&& name)
	: ProxyNode(READ_THRU_PROXY_NODE, std::move(name)), _round_robin(0)
{
}

ReadThruProxy::ReadThruProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name)), _round_robin(0)
{
}

ReadThruProxy::~ReadThruProxy()
{
}

// Get our configuration from the DefineLink we live in.
void ReadThruProxy::open(void)
{
	_round_robin = 0;

	ProxyNodeSeq rdrs = setup();
	_readers.swap(rdrs);

	for (const StorageNodePtr& stnp :_readers)
		stnp->open();
}

void ReadThruProxy::close(void)
{
	for (const StorageNodePtr& stnp :_readers)
		stnp->close();
}


#define UP \
	size_t nr = _readers.size(); \
	if (0 == nr) return; \
	size_t ir = _round_robin; \
	const StorageNodePtr& stnp = _readers[ir];

#define DOWN \
	stnp->barrier(); \
	ir++; \
	ir %= nr; \
	_round_robin = ir;

// Just get one atom. Round-robin.
void ReadThruProxy::getAtom(const Handle& h)
{
	UP;
	stnp->fetch_atom(h);
	DOWN;
}

void ReadThruProxy::fetchIncomingSet(AtomSpace* as, const Handle& h)
{
	UP;
	stnp->fetch_incoming_set(h, false, as);
	DOWN;
}

void ReadThruProxy::fetchIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	UP;
	stnp->fetch_incoming_by_type(h, t, as);
	DOWN;
}

void ReadThruProxy::loadValue(const Handle& atom, const Handle& key)
{
	UP;
	stnp->fetch_value(atom, key);
	DOWN;
}

void ReadThruProxy::loadType(AtomSpace* as, Type t)
{
	UP;
	stnp->fetch_all_atoms_of_type(t, as);
	DOWN;
}

void ReadThruProxy::barrier(AtomSpace* as)
{
	for (const StorageNodePtr& stnp :_readers)
		stnp->barrier(as);
}

DEFINE_NODE_FACTORY(ReadThruProxy, READ_THRU_PROXY_NODE)
