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
	: WriteThruProxy(WRITE_BUFFER_PROXY_NODE, std::move(name))
{
	init();
}

WriteBufferProxy::WriteBufferProxy(Type t, const std::string&& name)
	: WriteThruProxy(t, std::move(name))
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
	WriteThruProxy::open();
}

void WriteBufferProxy::storeAtom(const Handle& h, bool synchronous)
{
	WriteThruProxy::storeAtom(h, synchronous);
}

// Two-step remove. Just pass the two steps down to the children.
void WriteBufferProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                     bool recursive)
{
	WriteThruProxy::preRemoveAtom(as, h, recursive);
}

void WriteBufferProxy::postRemoveAtom(AtomSpace* as, const Handle& h,
                                    bool recursive, bool extracted_ok)
{
	WriteThruProxy::postRemoveAtom(as, h, recursive, extracted_ok);
}

void WriteBufferProxy::storeValue(const Handle& atom, const Handle& key)
{
	WriteThruProxy::storeValue(atom, key);
}

void WriteBufferProxy::updateValue(const Handle& atom, const Handle& key,
                            const ValuePtr& delta)
{
	WriteThruProxy::updateValue(atom, key, delta);
}

void WriteBufferProxy::barrier(AtomSpace* as)
{
	WriteThruProxy::barrier(as);
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
