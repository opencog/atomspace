/*
 * ReadWriteProxy.cc
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

#include <opencog/persist/proxy/ReadWriteProxy.h>

using namespace opencog;

ReadWriteProxy::ReadWriteProxy(const std::string&& name)
	: ProxyNode(READ_WRITE_PROXY_NODE, std::move(name))
{
	init();
}

ReadWriteProxy::ReadWriteProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

ReadWriteProxy::~ReadWriteProxy()
{
}

void ReadWriteProxy::init(void)
{
	// We've got the readers
	have_loadType = true;
	have_fetchIncomingByType = true;
	have_fetchIncomingSet = true;
	have_getAtom = true;
	have_loadValue = true;

	// We've got the writers
	have_removeAtom = true;
	have_storeValue = true;
	have_storeAtom = true;
	have_updateValue = true;
}

// Get our configuration from the DefineLink we live in.
void ReadWriteProxy::open(void)
{
	StorageNodeSeq rwpair = setup();

	if (rwpair.size() != 2)
		throw SyntaxException(TRACE_INFO,
			"Expecting two StorageNodes: a reader and a writer!");

	_reader = rwpair[0];
	_writer = rwpair[1];

	_reader->open();
	_writer->open();
}

void ReadWriteProxy::close(void)
{
	_writer->close();
	_reader->close();

	_writer = nullptr;
	_reader = nullptr;
}

// -----------------------------
// The readers

void ReadWriteProxy::getAtom(const Handle& h)
{
	_reader->fetch_atom(h);
}

void ReadWriteProxy::fetchIncomingSet(AtomSpace* as, const Handle& h)
{
	_reader->fetch_incoming_set(h, false, as);
}

void ReadWriteProxy::fetchIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	_reader->fetch_incoming_by_type(h, t, as);
}

void ReadWriteProxy::loadValue(const Handle& atom, const Handle& key)
{
	_reader->fetch_value(atom, key);
}

void ReadWriteProxy::loadType(AtomSpace* as, Type t)
{
	_reader->fetch_all_atoms_of_type(t, as);
}

void ReadWriteProxy::barrier(AtomSpace* as)
{
	_writer->barrier(as);
	_reader->barrier(as);
}

// ----------------------
// The writers

void ReadWriteProxy::storeAtom(const Handle& h, bool synchronous)
{
	_writer->store_atom(h);

	if (not synchronous) return;

	_writer->barrier();
}

void ReadWriteProxy::removeAtom(AtomSpace* as, const Handle& h, bool recursive)
{
	// XXX FIXME this is deeply fundamentally broken if there's more
	// than one target; because StorageNode::remove_atom() is broken.
	// See the comments in that code for additional guidance.
	_writer->remove_atom(as, h, recursive);
}

void ReadWriteProxy::storeValue(const Handle& atom, const Handle& key)
{
	_writer->store_value(atom, key);
}

void ReadWriteProxy::updateValue(const Handle& atom, const Handle& key,
                            const ValuePtr& delta)
{
	_writer->update_value(atom, key, delta);
}

DEFINE_NODE_FACTORY(ReadWriteProxy, READ_WRITE_PROXY_NODE)
