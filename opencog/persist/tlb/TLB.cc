/*
 * opencog/persist/tlb/TLB.cc
 *
 * Copyright (C) 2011 OpenCog Foundation
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
#include <opencog/atomspace/AtomSpace.h>
#include "TLB.h"

using namespace opencog;

TLB::TLB(uuid_pool* allocator)
{
	if (nullptr == allocator)
		_uuid_pool = &_local_pool;
	else
		_uuid_pool = allocator;
}

void TLB::clear()
{
    std::lock_guard<std::mutex> lck(_mtx);
    _handle_map.clear();
    _uuid_map.clear();
}

// ===================================================
// Handle resolution stuff.

void TLB::set_resolver(const AtomSpace* tab)
{
    _resolver.push_back(tab);
}

void TLB::clear_resolver(const AtomSpace* tab)
{
    auto it = std::find(_resolver.begin(), _resolver.end(), tab);
    if (it != _resolver.end())
        _resolver.erase(it);
}

// Search several atomspaces, in order.  First one to come up with
// the atom wins.  Seems to work, for now.
Handle TLB::do_res(const Handle& h)
{
    // No-op if it's already in an atomspace.
    if (h->getAtomSpace()) return h->get_handle();

    for (const AtomSpace* at : _resolver) {
        Handle hr(at->get_atom(h));
        if (nullptr != hr) return hr;
    }
    return Handle::UNDEFINED;
}

UUID TLB::addAtom(const Handle& h, UUID uuid)
{
    Handle hr = do_res(h);

    // We need to always use the atomspace version of this handle
    if (nullptr == hr) hr = h;

    // Force a resolution of the outgoing set!
    if (hr != h and h->is_link())
    {
        for (const Handle& ho: h->getOutgoingSet())
            addAtom(ho, TLB::INVALID_UUID);
    }

    std::lock_guard<std::mutex> lck(_mtx);

    // If we hold something that isn't the atomspace's version,
    // then remove it. Only the atomspace's version has the
    // correct values (including the TV) on it.
    if (hr != h)
    {
        auto pr = _handle_map.find(h);
        if (_handle_map.end() != pr)
        {
            UUID oid = pr->second;
            _handle_map.erase(pr);
            _uuid_map.erase(oid);

            OC_ASSERT(uuid == INVALID_UUID or oid == uuid,
                     "Earlier version of atom has mis-matched UUID!");

            // If not given a uuid, now we know what it is.
            uuid = oid;
        }
    }

    auto pr = _handle_map.find(hr);
    if (uuid == INVALID_UUID)
    {
        if (_handle_map.end() != pr) return pr->second;

        while (true)
        {
            // Not found; we need a new uuid.
            uuid = _uuid_pool->get_uuid();

            // Oh wait, is it being used already?
            auto pr = _uuid_map.find(uuid);
            if (_uuid_map.end() == pr) break;
        }
    }
    else
    {
        if (_handle_map.end() != pr)
        {
            OC_ASSERT(uuid == pr->second,
                     "Atom is already in the TLB, and UUID's don't match!");

            // If the atom that we are holding is in the same atomspace
            // as the resolved atom, then we are done. Otherwise, we
            // need to replace it with the version with the indicated
            // atomspace. That is because atoms in different atomspaces
            // will hold different values and TV's.

            AtomSpace* has = hr->getAtomSpace();
            AtomSpace* pas = pr->first->getAtomSpace();
            if (pas and has and pas == has)
                return uuid;

            _handle_map.erase(pr);
            _uuid_map.erase(uuid);
        }
    }

    _uuid_map.emplace(std::make_pair(uuid, hr));
    _handle_map.emplace(std::make_pair(hr, uuid));

    return uuid;
}

Handle TLB::getAtom(UUID uuid)
{
    if (INVALID_UUID == uuid) return Handle::UNDEFINED;
    std::lock_guard<std::mutex> lck(_mtx);
    auto pr = _uuid_map.find(uuid);

    if (_uuid_map.end() == pr) return Handle::UNDEFINED;

    return pr->second;
}

UUID TLB::getUUID(const Handle& h)
{
    std::lock_guard<std::mutex> lck(_mtx);
    auto pr = _handle_map.find(h);
    if (_handle_map.end() != pr)
        return pr->second;

    return INVALID_UUID;
}

/// The two remove functions below erase the uuid from the uuid-to-handle
/// lookup. This means that getAtom() will not be able to find the Atom,
/// which is what we want for something deleted. However, we do keep the
/// handle-to-uuid lookup, so that if we ever see this handle again, we
/// recycle the old uuid for it. This is required to correctly process
/// multi-threaded add-delete races. If the same atom is being repeatedly
/// added and deleted (see MultiDeleteUTest), then there are races where
/// the same atom might be assigned two different uuid's, leading to a
/// throw of either of the above exceptions. Avoid this by just keeping
/// the atom-to-uuid map, so we can recycle the uuid next time we see
/// this atom. (Without this, MultiDeleteUTest hits the race once every
/// dozen runs.)
void TLB::removeAtom(UUID uuid)
{
    if (INVALID_UUID == uuid) return;
    std::lock_guard<std::mutex> lck(_mtx);

    auto pr = _uuid_map.find(uuid);
    if (_uuid_map.end() == pr) return;

    _uuid_map.erase(uuid);
    // Do NOT remove from the handle_map. See note above.
    // _handle_map.erase(pr->second);
}

void TLB::removeAtom(const Handle& h)
{
    std::lock_guard<std::mutex> lck(_mtx);
    auto pr = _handle_map.find(h);
    if (_handle_map.end() != pr)
    {
        _uuid_map.erase(pr->second);
        // Do NOT remove from the handle_map. See note above.
        // _handle_map.erase(pr);
    }
}

/// Much like removeAtom(), except all traces of the atom are removed,
/// allowing corrected UUID assignments to be performed.  This is
/// used in managing races with atom insertion, where two distinct
/// UUID's might get issued for the same atom. This method is guaranteed
/// to clobber the UUID.
void TLB::purgeAtom(UUID uuid)
{
    if (INVALID_UUID == uuid) return;
    std::lock_guard<std::mutex> lck(_mtx);

    auto pr = _uuid_map.find(uuid);
    if (_uuid_map.end() == pr) return;

    _uuid_map.erase(uuid);
    _handle_map.erase(pr->second);
}
