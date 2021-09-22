/*
 * opencog/persist/tlb/TLB.h
 *
 * Copyright (C) 2008-2010 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2014, 2015 Linas Vepstas
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

#ifndef _OPENCOG_TLB_H
#define _OPENCOG_TLB_H

#include <atomic>
#include <mutex>
#include <unordered_map>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

class uuid_pool
{
public:
    virtual ~uuid_pool() {}

    virtual UUID get_uuid(void) = 0;
};

/// Default local (non-shared) uuid_pool.
class local_uuid_pool : public uuid_pool
{
private:
    // Thread-safe atomic
    std::atomic<UUID> _brk_uuid;
public:
    local_uuid_pool(void) : _brk_uuid(1) {}

    UUID get_uuid(void)
    {
        return _brk_uuid.fetch_add(1, std::memory_order_relaxed);
    };
};

class AtomTable;

/**
 * Each atom stored in the AtomSpace will have an immutable UUID, which
 * may be used to refer to that atom when a reference to that atom needs
 * to be kept.  This allows atoms to be shared between different
 * atomspaces running in different memory spaces (on different machines
 * in a network cluster): a given atom will have a unique UUID by which
 * it is refered to.
 *
 * Atomspaces are also issued UUID's. This allows atomspaces to be
 * uniquely identified as well.
 */
class TLB
{
private:
    local_uuid_pool _local_pool;
    uuid_pool* _uuid_pool;

    std::mutex _mtx;
    std::unordered_map<UUID, Handle> _uuid_map;
    std::unordered_map<Handle, UUID,
                      std::hash<opencog::Handle>,
                      std::equal_to<opencog::Handle> > _handle_map;

    // Its a vector, not a set, because it's priority ranked.
    std::vector<const AtomTable*> _resolver;
    Handle do_res(const Handle&);

public:

    static const UUID INVALID_UUID = ULONG_MAX;

    TLB(uuid_pool* = nullptr);
    void set_resolver(const AtomTable*);
    void clear_resolver(const AtomTable*);

    size_t size() { return _uuid_map.size(); }
    void clear();

    /**
     * Adds a new atom to the TLB.
     * If the atom has already been added, then an exception is thrown.
     *
     * @param Atom to be added.
     * @return UUID of the newly added atom.
     */
    UUID addAtom(const AtomPtr& a, UUID uuid) {
        return addAtom(a->get_handle(), uuid);
    }
    UUID addAtom(const Handle&, UUID);

    /** Look up atom corresponding to the UUID. */
    Handle getAtom(UUID);

    /** Look up UUID corresponding to the atom. */
    UUID getUUID(const Handle&);

    /** Remove the atom. */
    void removeAtom(const AtomPtr& a) {
        return removeAtom(a->get_handle());
    }
    void removeAtom(const Handle&);
    void removeAtom(UUID);
    void purgeAtom(UUID);
};

} // namespace opencog

#endif // _OPENCOG_TLB_H
