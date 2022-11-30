/*
 * DynamicDataProxy.cc
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

#include <opencog/persist/proxy/DynamicDataProxy.h>

using namespace opencog;

DynamicDataProxy::DynamicDataProxy(const std::string&& name)
	: ProxyNode(DYNAMIC_DATA_PROXY_NODE, std::move(name))
{
	init();
}

DynamicDataProxy::DynamicDataProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

DynamicDataProxy::~DynamicDataProxy()
{
}

void DynamicDataProxy::init(void)
{
	have_getAtom = true;
	have_loadValue = true;
}

void DynamicDataProxy::open(void)
{
}

void DynamicDataProxy::close(void)
{
	_reader->close();
	_reader = nullptr;
}

#define CHECK_OPEN if (nullptr == _reader) return;

void DynamicDataProxy::getAtom(const Handle& h)
{
	CHECK_OPEN;

	_reader->fetch_atom(h);
	_reader->barrier();
}

void DynamicDataProxy::loadValue(const Handle& atom, const Handle& key)
{
	CHECK_OPEN;
	if (nullptr != atom->getValue(key)) return;

	_reader->fetch_value(atom, key);
}

DEFINE_NODE_FACTORY(DynamicDataProxy, DYNAMIC_DATA_PROXY_NODE)
