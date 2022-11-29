/*
 * WriteThruProxy.cc
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

#include <opencog/persist/proxy/WriteThruProxy.h>

using namespace opencog;

WriteThruProxy::WriteThruProxy(const std::string&& name)
	: StorageNode(WRITE_THRU_PROXY, std::move(name))
{
}

WriteThruProxy::WriteThruProxy(Type t, const std::string&& name)
	: StorageNode(t, std::move(name))
{
}

WriteThruProxy::~WriteThruProxy()
{
}

void WriteThruProxy::destroy(void) {}
void WriteThruProxy::erase(void) {}

std::string WriteThruProxy::monitor(void)
{
	return "";
}

// Get our configuration from the DefineLink we live in.
void WriteThruProxy::open(void)
{
	_targets.clear();

	IncomingSet dli(getIncomingSetByType(DEFINE_LINK));

	// We could throw an error here ... or we can just no-op.
	if (0 == dli.size()) return;

	// If there is only one, grab it.
	Handle params = dli[0]->getOutgoingAtom(1);
	if (params->is_type(PROXY_NODE))
	{
		_targets.emplace_back(StorageNodeCast(params));
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

		_targets.emplace_back(stnp);
	}
}

void WriteThruProxy::storeAtom(const Handle& h, bool synchronous)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->store_atom(h);

	if (not synchronous) return;

	for (const StorageNodePtr& stnp : _targets)
		stnp->barrier();
}

void WriteThruProxy::removeAtom(AtomSpace* as, const Handle& h, bool recursive)
{
	// XXX FIXME this is deeply fundamentally broken if there's more
	// than one target; because StorageNode::remove_atom() is broken.
	// See the comments in that code for additional guidance.
	for (const StorageNodePtr& stnp : _targets)
		stnp->remove_atom(as, h, recursive);
}

void WriteThruProxy::storeValue(const Handle& atom, const Handle& key)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->store_value(atom, key);
}

void WriteThruProxy::updateValue(const Handle& atom, const Handle& key,
                            const ValuePtr& delta)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->update_value(atom, key, delta);
}

HandleSeq WriteThruProxy::loadFrameDAG(void)
{
	// XXX FIXME;
	return HandleSeq();
}

Handle WriteThruProxy::getLink(Type t, const HandleSeq& hseq)
{
	// Ugh Copy
	HandleSeq hsc(hseq);
	return _atom_space->get_link(t, std::move(hsc));
}

DEFINE_NODE_FACTORY(WriteThruProxy, WRITE_THRU_PROXY)
