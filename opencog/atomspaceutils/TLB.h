/*
 * opencog/atomspaceutils/TLB.h
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
 *
 * Reserving UUID's is kind of like mallocing them, except that
 * (currently) there is no way to free them.  Use reserve_range()
 * and reserve_extent() to malloc them.
 *
 * Everything in this class is private, mostly because we don't want
 * anyone to mess with it, except our closest friends.
 */
class TLB
{
private:

    // Thread-safe atomic
    std::atomic<UUID> _brk_uuid;

    std::mutex _mtx;
    std::unordered_map<UUID, Handle> _uuid_map;
    std::unordered_map<Handle, UUID,
                      std::hash<opencog::Handle>,
                      std::equal_to<opencog::Handle> > _handle_map;

    // Its a vector, not a set, because its priority ranked.
    std::vector<const AtomTable*> _resolver;
    Handle do_res(const Handle&);

public:

    static const UUID INVALID_UUID = ULONG_MAX;

    TLB();
    void set_resolver(const AtomTable*);
    void clear_resolver(const AtomTable*);

    size_t size() { return _uuid_map.size(); }

    /**
     * Adds a new atom to the TLB.
     * If the atom has already been added, then an exception is thrown.
     *
     * @param Atom to be added.
     * @return UUID of the newly added atom.
     */
    UUID addAtom(const AtomPtr& a, UUID uuid) {
        return addAtom(a->getHandle(), uuid);
    }
    UUID addAtom(const Handle&, UUID);

    /** Look up atom corresponding to the UUID. */
    Handle getAtom(UUID);

    /** Look up UUID corresponding to the atom. */
    UUID getUUID(const Handle&);

    /** Remove the atom. */
    void removeAtom(const AtomPtr& a) {
        return removeAtom(a->getHandle());
    }
    void removeAtom(const Handle&);

    UUID getMaxUUID(void) { return _brk_uuid; }

    /// Reserve a range of UUID's.  The range is inclusive; both `lo`
    /// and `hi` are reserved.  The range must NOT intersect with the
    /// currently issued UUID's.
    inline void reserve_range(UUID lo, UUID hi)
    {
        if (hi <= lo)
            throw InvalidParamException(TRACE_INFO,
                "Bad argument order.");
        UUID extent = hi - lo + 1;

        UUID oldlo = _brk_uuid.fetch_add(extent, std::memory_order_relaxed);

        if (lo < oldlo)
            throw InvalidParamException(TRACE_INFO,
                "Bad range reserve.");
    }

    /// Reserve an extent of UUID's. The lowest reserved ID is returned.
    /// That is, after this call, no one else will be issued UUID's in
    /// the range of [retval, retval+extent-1].
    inline UUID reserve_extent(UUID extent)
    {
        return _brk_uuid.fetch_add(extent, std::memory_order_relaxed);
    }

    /// Make sure that all UUID's up to at least 'hi' have been
    /// reserved.  No error checks are made; its OK if 'hi' has
    /// already been issued.
    inline void reserve_upto(UUID hi)
    {
        if (hi < _brk_uuid) return;
        UUID extent = hi - _brk_uuid + 1;
        _brk_uuid.fetch_add(extent, std::memory_order_relaxed);
    }
};

} // namespace opencog

#endif // _OPENCOG_TLB_H
