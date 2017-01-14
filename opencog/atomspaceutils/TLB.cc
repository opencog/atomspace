/*
 * opencog/atomspaceutils/TLB.cc
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

#include <opencog/atomspace/AtomTable.h>
#include "TLB.h"

using namespace opencog;

TLB::TLB() : _brk_uuid(1) {}

// ===================================================
// Handle resolution stuff.

void TLB::set_resolver(const AtomTable* tab)
{
    _resolver.push_back(tab);
}

void TLB::clear_resolver(const AtomTable* tab)
{
    auto it = std::find(_resolver.begin(), _resolver.end(), tab);
    if (it != _resolver.end())
        _resolver.erase(it);
}

// Search several atomspaces, in order.  First one to come up with
// the atom wins.  Seems to work, for now.
Handle TLB::do_res(const Handle& h)
{
    for (const AtomTable* at : _resolver) {
        Handle hr(at->getHandle(h));
        if (nullptr != hr) return hr;
    }
    return Handle::UNDEFINED;
}

UUID TLB::addAtom(const Handle& h, UUID uuid)
{
    if (uuid == INVALID_UUID)
    {
        std::lock_guard<std::mutex> lck(_mtx);
        auto pr = _handle_map.find(h);
        if (_handle_map.end() != pr)
        {
            uuid = pr->second;
            Handle hr = do_res(pr->first);
            if (nullptr == hr) return uuid;
            if (hr == pr->first) return uuid;

            // Make sure that we always store the atomspaces
            // version (as its the one with the right TV on it).
            _handle_map.erase(pr);
            _uuid_map.erase(uuid);

            _handle_map.emplace(std::make_pair(hr, uuid));
            _uuid_map.emplace(std::make_pair(uuid, hr));
            return uuid;
        }

        uuid = _brk_uuid.fetch_add(1, std::memory_order_relaxed);
    }
    else
    {
        std::lock_guard<std::mutex> lck(_mtx);
        auto pr = _handle_map.find(h);
        if (_handle_map.end() != pr)
        {
            if (uuid != pr->second)
                throw InvalidParamException(TRACE_INFO,
                     "Atom is already in the TLB, and UUID's don't match!");

            Handle hr = do_res(pr->first);
            if (nullptr == hr) return uuid;
            if (hr == pr->first) return uuid;

            // Make sure that we always store the atomspaces
            // version (as its the one with the right TV on it).
            _handle_map.erase(pr);
            _uuid_map.erase(uuid);

            _handle_map.emplace(std::make_pair(hr, uuid));
            _uuid_map.emplace(std::make_pair(uuid, hr));

            return uuid;
        }

        reserve_upto(uuid);
    }

    // We need to always use the atomspace version of this handle
    Handle hr = do_res(h);
    if (nullptr == hr) hr = h;

    std::lock_guard<std::mutex> lck(_mtx);
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

void TLB::removeAtom(const Handle& h)
{
    std::lock_guard<std::mutex> lck(_mtx);
    auto pr = _handle_map.find(h);
    if (_handle_map.end() != pr)
    {
        _uuid_map.erase(pr->second);
        _handle_map.erase(pr);
    }
}
