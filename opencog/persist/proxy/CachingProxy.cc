/*
 * CachingProxy.cc
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

#include <opencog/persist/proxy/CachingProxy.h>

using namespace opencog;

CachingProxy::CachingProxy(const std::string&& name)
	: ReadThruProxy(CACHING_PROXY_NODE, std::move(name))
{
	init();
}

CachingProxy::CachingProxy(Type t, const std::string&& name)
	: ReadThruProxy(t, std::move(name))
{
	init();
}

CachingProxy::~CachingProxy()
{
}

void CachingProxy::init(void)
{
}

// Get our configuration from the ProxyParameterLink we live in.
// XXX TODO Add support for expiration times, limited AtomSpace
// size and whatever other whizzy caching ideas we might want.
void CachingProxy::open(void)
{
	_nhits = 0;
	_nmisses = 0;
	ReadThruProxy::open();
}

void CachingProxy::close(void)
{
	ReadThruProxy::close();
}

#define CHECK_OPEN if (not ReadThruProxy::connected()) return;

// Conceptually, we want to do this (or something like this):
//
//    const Handle& ch = _atom_space->get_atom(h);
//    if (ch) return;
//
// but that won't work, of course, because by this point, the atom has
// already been inserted into the AtomSpace. As a work-around, look to
// see if it looks like its been fetched before: it is decorated with
// Values, or it has an incoming set, etc. It won't have any, if it is
// a fresh atom.
//
// The alternative is to keep an std::set or an std::unordered_set of
// everything we've fetched before. But this eats a little bit of RAM,
// and is slower than the checks done below. But we will need to do
// that, sooner or later, if we want to have time-stamps to expire old
// atoms. Whatever, that's a future design.

void CachingProxy::getAtom(const Handle& h)
{
	CHECK_OPEN;

	if (h->haveValues()) { _nhits++; return; }

	_nmisses ++;
	ReadThruProxy::getAtom(h);
}

void CachingProxy::fetchIncomingSet(AtomSpace* as, const Handle& h)
{
	CHECK_OPEN;
	if (0 < h->getIncomingSetSize(as)) { _nhits++; return; }

	_nmisses ++;
	ReadThruProxy::fetchIncomingSet(as, h);
}

void CachingProxy::fetchIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	CHECK_OPEN;
	if (0 < h->getIncomingSetSizeByType(t, as)) { _nhits++; return; }

	_nmisses ++;
	ReadThruProxy::fetchIncomingByType(as, h, t);
}

void CachingProxy::loadValue(const Handle& atom, const Handle& key)
{
	CHECK_OPEN;
	if (nullptr != atom->getValue(key)) { _nhits++; return; }

	_nmisses ++;
	ReadThruProxy::loadValue(atom, key);
}

// We're just going to be unconditional, here.
void CachingProxy::loadType(AtomSpace* as, Type t)
{
	CHECK_OPEN;
	_nmisses ++;
	ReadThruProxy::loadType(as, t);
}

std::string CachingProxy::monitor(void)
{
	std::string rpt;
	rpt += "Caching Proxy: ";
	rpt += "hits: " + std::to_string(_nhits);
	rpt += "   misses: " + std::to_string(_nmisses);
	rpt += "\n";
	return rpt;
}

DEFINE_NODE_FACTORY(CachingProxy, CACHING_PROXY_NODE)
