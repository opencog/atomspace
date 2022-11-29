/*
 * PassThruyProxy.cc
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

#include <opencog/persist/proxy/PassThruProxy.h>

using namespace opencog;

PassThruProxy::PassThruProxy(Type t, std::string name)
	: StorageNode(t, name)
{
}

PassThruProxy::~PassThruProxy()
{
}

void PassThruProxy::destroy(void) {}
void PassThruProxy::erase(void) {}

std::string PassThruProxy::monitor(void) { return ""; }

void PassThruProxy::getAtom(const Handle& h) {}
void PassThruProxy::fetchIncomingSet(AtomSpace* as, const Handle& h) {}
void PassThruProxy::fetchIncomingByType(AtomSpace*, const Handle&, Type) {}
void PassThruProxy::storeAtom(const Handle&, bool synchronous) {}
void PassThruProxy::removeAtom(AtomSpace*, const Handle&, bool recursive) {}
void PassThruProxy::storeValue(const Handle& atom, const Handle& key) {}
void PassThruProxy::updateValue(const Handle& atom, const Handle& key,
	                         const ValuePtr& delta) {}
void PassThruProxy::loadValue(const Handle& atom, const Handle& key) {}

void PassThruProxy::loadType(AtomSpace*, Type) {}
void PassThruProxy::loadAtomSpace(AtomSpace*) {}
void PassThruProxy::storeAtomSpace(const AtomSpace*) {}

HandleSeq PassThruProxy::loadFrameDAG(void) { return HandleSeq(); }
void PassThruProxy::storeFrameDAG(AtomSpace*) {}

void PassThruProxy::deleteFrame(AtomSpace*) {}
void PassThruProxy::barrier(AtomSpace*) {}

Handle PassThruProxy::getLink(Type t, const HandleSeq& hseq)
{
	// Ugh Copy
	HandleSeq hsc(hseq);
	return _atom_space->get_link(t, std::move(hsc));
}
