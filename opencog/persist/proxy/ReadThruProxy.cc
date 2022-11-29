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
	: StorageNode(READ_THRU_PROXY, std::move(name)), _round_robin(0)
{
}

ReadThruProxy::ReadThruProxy(Type t, const std::string&& name)
	: StorageNode(t, std::move(name)), _round_robin(0)
{
}

ReadThruProxy::~ReadThruProxy()
{
}

void ReadThruProxy::destroy(void) {}
void ReadThruProxy::erase(void) {}

std::string ReadThruProxy::monitor(void)
{
	return "";
}

// Get our configuration from the DefineLink we live in.
void ReadThruProxy::open(void)
{
	_readers.clear();
	_round_robin = 0;

	IncomingSet dli(getIncomingSetByType(DEFINE_LINK));

	// We could throw an error here ... or we can just no-op.
	if (0 == dli.size()) return;

	// If there is only one, grab it.
	Handle params = dli[0]->getOutgoingAtom(1);
	if (params->is_type(PROXY_NODE))
	{
		_readers.emplace_back(StorageNodeCast(params));
		return;
	}

	// Expect the parameters to be wrapped in a ListLink
	if (not params->is_type(LIST_LINK))
		SyntaxException(TRACE_INFO, "Expecting parameters in a ListLink!");

	for (const Handle& h : params->getOutgoingSet())
	{
		StorageNodePtr stnp = StorageNodeCast(h);
		if (nullptr == stnp)
			SyntaxException(TRACE_INFO, "Expecting a list of StorageNodes!");

		_readers.emplace_back(stnp);
	}
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

HandleSeq ReadThruProxy::loadFrameDAG(void)
{
	// XXX FIXME;
	return HandleSeq();
}

Handle ReadThruProxy::getLink(Type t, const HandleSeq& hseq)
{
	// Ugh Copy
	HandleSeq hsc(hseq);
	return _atom_space->get_link(t, std::move(hsc));
}

DEFINE_NODE_FACTORY(ReadThruProxy, READ_THRU_PROXY)
