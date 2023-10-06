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

#include <opencog/util/oc_assert.h>
#include <opencog/persist/proxy/WriteThruProxy.h>

using namespace opencog;

WriteThruProxy::WriteThruProxy(const std::string&& name)
	: ProxyNode(WRITE_THRU_PROXY_NODE, std::move(name))
{
	init();
}

WriteThruProxy::WriteThruProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

WriteThruProxy::~WriteThruProxy()
{
}

void WriteThruProxy::init(void)
{
	have_removeAtom = true;
	have_storeValue = true;
	have_storeAtom = true;
	have_updateValue = true;
}

// Get our configuration from the DefineLink we live in.
void WriteThruProxy::open(void)
{
	StorageNodeSeq sns = setup();
	_targets.swap(sns);

	for (const StorageNodePtr& stnp : _targets)
		stnp->open();
}

void WriteThruProxy::close(void)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->close();

	// Get rid of them for good. The `connected()` method needs this.
	_targets.resize(0);
}

void WriteThruProxy::storeAtom(const Handle& h, bool synchronous)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->store_atom(h);

	if (not synchronous) return;

	for (const StorageNodePtr& stnp : _targets)
		stnp->barrier();
}

void WriteThruProxy::removeAtom(AtomSpace* as, const Handle& h,
                                bool recursive)
{
	OC_ASSERT(false, "Internal Error: Unexpected call to removeAtom()");
}

// Two-step remove. Just pass the two steps down to the children.
void WriteThruProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                   bool recursive)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->preRemoveAtom(as, h, recursive);
}

void WriteThruProxy::postRemoveAtom(AtomSpace* as, const Handle& h,
                                    bool recursive, bool extracted_ok)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->postRemoveAtom(as, h, recursive, extracted_ok);
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

void WriteThruProxy::barrier(AtomSpace* as)
{
	for (const StorageNodePtr& stnp : _targets)
		stnp->barrier(as);
}

DEFINE_NODE_FACTORY(WriteThruProxy, WRITE_THRU_PROXY_NODE)
