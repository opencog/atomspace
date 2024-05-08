/*
 * WriteBufferProxy.cc
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

#include <opencog/persist/proxy/WriteBufferProxy.h>

using namespace opencog;

WriteBufferProxy::WriteBufferProxy(const std::string&& name)
	: ProxyNode(WRITE_BUFFER_PROXY_NODE, std::move(name))
{
	init();
}

WriteBufferProxy::WriteBufferProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

WriteBufferProxy::~WriteBufferProxy()
{
}

void WriteBufferProxy::init(void)
{
	have_removeAtom = true;
	have_storeValue = true;
	have_storeAtom = true;
	have_updateValue = true;
}

// Get our configuration from the DefineLink we live in.
void WriteBufferProxy::open(void)
{
	StorageNodeSeq sns = setup();
	_targets.swap(sns);

	for (const StorageNodePtr& stnp : _targets)
		stnp->open();
}

void WriteBufferProxy::close(void)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->close();

	// Get rid of them for good. The `connected()` method needs this.
	_targets.resize(0);
}

void WriteBufferProxy::storeAtom(const Handle& h, bool synchronous)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->store_atom(h);

	if (not synchronous) return;

	for (const StorageNodePtr& stnp : _targets)
		stnp->barrier();
}

// Two-step remove. Just pass the two steps down to the children.
void WriteBufferProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                   bool recursive)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->preRemoveAtom(as, h, recursive);
}

void WriteBufferProxy::postRemoveAtom(AtomSpace* as, const Handle& h,
                                    bool recursive, bool extracted_ok)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->postRemoveAtom(as, h, recursive, extracted_ok);
}

void WriteBufferProxy::storeValue(const Handle& atom, const Handle& key)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->store_value(atom, key);
}

void WriteBufferProxy::updateValue(const Handle& atom, const Handle& key,
                            const ValuePtr& delta)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->update_value(atom, key, delta);
}

void WriteBufferProxy::barrier(AtomSpace* as)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->barrier(as);
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
