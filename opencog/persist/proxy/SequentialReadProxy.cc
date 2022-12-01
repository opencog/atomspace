/*
 * SequentialReadProxy.cc
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

#include <opencog/persist/proxy/SequentialReadProxy.h>

using namespace opencog;

SequentialReadProxy::SequentialReadProxy(const std::string&& name)
	: ProxyNode(SEQUENTIAL_READ_PROXY_NODE, std::move(name))
{
	init();
}

SequentialReadProxy::SequentialReadProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

SequentialReadProxy::~SequentialReadProxy()
{
}

void SequentialReadProxy::init(void)
{
	have_loadType = true;
	have_fetchIncomingByType = true;
	have_fetchIncomingSet = true;
	have_getAtom = true;
	have_loadValue = true;
}

// Get our configuration from the DefineLink we live in.
void SequentialReadProxy::open(void)
{
	StorageNodeSeq rdrs = setup();
	_readers.swap(rdrs);

	for (const StorageNodePtr& stnp :_readers)
		stnp->open();
}

void SequentialReadProxy::close(void)
{
	for (const StorageNodePtr& stnp :_readers)
		stnp->close();

	// Get rid of them for good. The `connected()` method needs this.
	_readers.resize(0);
}


#define CHECK_OPEN if (0 == _readers.size()) return;

// Just get one atom.
void SequentialReadProxy::getAtom(const Handle& h)
{
	CHECK_OPEN;
	for (const StorageNodePtr& stnp : _readers)
	{
		stnp->fetch_atom(h);
		stnp->barrier();
		if (h->haveValues()) return;
	}
}

void SequentialReadProxy::fetchIncomingSet(AtomSpace* as, const Handle& h)
{
	CHECK_OPEN;
	for (const StorageNodePtr& stnp : _readers)
	{
		stnp->fetch_incoming_set(h, false, as);
		stnp->barrier();
		if (0 < h->getIncomingSetSize(as)) return;
	}
}

void SequentialReadProxy::fetchIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	CHECK_OPEN;
	for (const StorageNodePtr& stnp : _readers)
	{
		stnp->fetch_incoming_by_type(h, t, as);
		stnp->barrier();
		if (0 < h->getIncomingSetSizeByType(t, as)) return;
	}
}

void SequentialReadProxy::loadValue(const Handle& atom, const Handle& key)
{
	CHECK_OPEN;
	for (const StorageNodePtr& stnp : _readers)
	{
		stnp->fetch_value(atom, key);
		stnp->barrier();
		if (nullptr != atom->getValue(key)) return;
	}
}

void SequentialReadProxy::loadType(AtomSpace* as, Type t)
{
	CHECK_OPEN;
	size_t curnum = as->get_num_atoms_of_type(t);
	for (const StorageNodePtr& stnp : _readers)
	{
		stnp->fetch_all_atoms_of_type(t, as);
		stnp->barrier();

		// If we found more, we're done. If user wants to
		// go deeper, they can call us again, or configure
		// a different kind of proxy.
		if (curnum < as->get_num_atoms_of_type(t)) return;
	}
}

void SequentialReadProxy::barrier(AtomSpace* as)
{
	for (const StorageNodePtr& stnp :_readers)
		stnp->barrier(as);
}

DEFINE_NODE_FACTORY(SequentialReadProxy, SEQUENTIAL_READ_PROXY_NODE)
