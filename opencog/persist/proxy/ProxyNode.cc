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
	init();
}

ProxyNode::ProxyNode(Type t, const std::string&& name)
	: StorageNode(t, std::move(name))
{
	init();
}

ProxyNode::~ProxyNode()
{
}

void ProxyNode::init(void)
{
	have_getAtom = false;
	have_fetchIncomingSet = false;
	have_fetchIncomingByType = false;
	have_storeAtom = false;
	have_removeAtom = false;
	have_storeValue = false;
	have_updateValue = false;
	have_loadValue = false;
	have_loadType = false;
	have_loadAtomSpace = false;
	have_storeAtomSpace = false;
}

void ProxyNode::proxy_open(void)
{
	throw RuntimeException(TRACE_INFO,
		"If you want to open the proxy, just say `cog-open`");
}

void ProxyNode::proxy_close(void)
{
	throw RuntimeException(TRACE_INFO,
		"If you want to close the proxy, just say `cog-close`");
}

void ProxyNode::set_proxy(const Handle&)
{
	throw RuntimeException(TRACE_INFO,
		"Error: `cog-set-proxy!` is not appropriate, here.");
}

std::string ProxyNode::monitor(void)
{
	std::string rpt;
	rpt += "The ";
	rpt += nameserver().getTypeName(_type);
	rpt += " has not implemented monitoring.\n";
	return rpt;
}

// Get our configuration from the DefineLink we live in.
// Hmm, perhaps this should be a StateLink?
//
// XXX FIXME. Using this ProxyParametersLink thing is a kind of
// cheesy hack, to pass parameters to the ProxyNode. It vaguely
// resembles the structure of an ExecutionLink, but instead of
// writing (Execution (Predicate "foo") (List (args...)))
// we write (ProxyParameters (Proxy "foo") (List (params...)))
// Except that we don't have a C++ class for ProxyParameters
// and it is not executable. So ... I dunno. I'm not happy with
// this design.
//
// More generally there is the work in sensory to create a DTD/IDL
// to describe parameters. The design work there is not done, but
// when it is, this should convert to that.
StorageNodeSeq ProxyNode::setup(void)
{
	StorageNodeSeq stolist;

	IncomingSet dli(getIncomingSetByType(PROXY_PARAMETERS_LINK));

	// We could throw an error here ... or we can just no-op.
	if (0 == dli.size()) return stolist;

	// Don't know what to do if there are two or more parameter sets.
	if (1 != dli.size())
	{
		throw SyntaxException(TRACE_INFO,
			"Expecting only one set of parameters for a ProxyNode. Got %lu of them:\n%s\n",
			dli.size(), oc_to_string(dli).c_str());
	}

	// If there is no ListLink, then just grab that.
	const Handle& params = dli[0]->getOutgoingAtom(1);
	if (params->is_type(STORAGE_NODE))
	{
		stolist.emplace_back(StorageNodeCast(params));
		return stolist;
	}

	// Expect the parameters to be wrapped in a ListLink
	if (not params->is_type(LIST_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting parameters in a ListLink! Got\n%s\n",
			dli[0]->to_short_string().c_str());

	for (const Handle& h : params->getOutgoingSet())
	{
		StorageNodePtr stnp = StorageNodeCast(h);
		if (nullptr == stnp)
		{
			// If its a StorageNode but the cast failed, that
			// means the type definition was not loaded. Print a
			// user-friendly error message for this case.
			if (nameserver().isA(h->get_type(), STORAGE_NODE))
			{
				throw SyntaxException(TRACE_INFO,
					"There is no definition for %s.\n"
					"Did you forget to load the module that deffines this?\n"
					"For example: `(use-modules (opencog persist-rocks))`\n"
					"Config was %s\n",
					h->to_short_string().c_str(),
					dli[0]->to_short_string().c_str());
			}
			throw SyntaxException(TRACE_INFO,
				"Expecting a list of Storage or ProxyNodes! Got\n%s\n",
				dli[0]->to_short_string().c_str());
		}

		stolist.emplace_back(stnp);
	}

	return stolist;
}

void ProxyNode::destroy(void) {}
void ProxyNode::erase(void) {}

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

void opencog_persist_proxy_init(void)
{
   // Force shared lib ctors to run
};

/* ===================== END OF FILE ===================== */
