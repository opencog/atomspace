/*
 * ProxyNode.cc
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

#include <opencog/persist/proxy/ProxyNode.h>

using namespace opencog;

ProxyNode::ProxyNode(const std::string&& name)
	: StorageNode(PROXY_NODE, std::move(name))
{
}

ProxyNode::ProxyNode(Type t, const std::string&& name)
	: StorageNode(t, std::move(name))
{
}

ProxyNode::~ProxyNode()
{
}

void ProxyNode::destroy(void) {}
void ProxyNode::erase(void) {}

std::string ProxyNode::monitor(void)
{
	return "";
}

// Get our configuration from the DefineLink we live in.
std::vector<StorageNodePtr>  ProxyNode::setup(void)
{
	std::vector<StorageNodePtr> stolist;

	IncomingSet dli(getIncomingSetByType(DEFINE_LINK));

	// We could throw an error here ... or we can just no-op.
	if (0 == dli.size()) return stolist;

	// If there is only one, grab it.
	Handle params = dli[0]->getOutgoingAtom(1);
	if (params->is_type(PROXY_NODE))
	{
		stolist.emplace_back(StorageNodeCast(params));
		return stolist;
	}

	// Expect the parameters to be wrapped in a ListLink
	if (not params->is_type(LIST_LINK))
		SyntaxException(TRACE_INFO, "Expecting parameters in a ListLink!");

	for (const Handle& h : params->getOutgoingSet())
	{
		StorageNodePtr stnp = StorageNodeCast(h);
		if (nullptr == stnp)
			SyntaxException(TRACE_INFO, "Expecting a list of StorageNodes!");

		stolist.emplace_back(stnp);
	}

	return stolist;
}

HandleSeq ProxyNode::loadFrameDAG(void)
{
	// XXX FIXME;
	return HandleSeq();
}

Handle ProxyNode::getLink(Type t, const HandleSeq& hseq)
{
	// Ugh Copy
	HandleSeq hsc(hseq);
	return _atom_space->get_link(t, std::move(hsc));
}
