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
	: StorageNode(READ_THRU_PROXY, std::move(name))
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

void ReadThruProxy::getAtom(const Handle&)
{
}

void ReadThruProxy::fetchIncomingSet(AtomSpace*, const Handle&)
{
}

void ReadThruProxy::fetchIncomingByType(AtomSpace*, const Handle&, Type)
{
}

void ReadThruProxy::loadValue(const Handle& atom, const Handle& key)
{
}

void ReadThruProxy::loadType(AtomSpace*, Type)
{
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
