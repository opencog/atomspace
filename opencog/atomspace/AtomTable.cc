/*
 * opencog/atomspace/AtomTable.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013-2015 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Previous versions were written by
 *            Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
 *            Welter Silva <welter@vettalabs.com>
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

#include "AtomTable.h"

#include <atomic>
#include <functional>
#include <iterator>
#include <mutex>
#include <set>

#include <stdlib.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/functional.h>
#include <opencog/util/Logger.h>

//#define DPRINTF printf
#define DPRINTF(...)

// Uncomment the following to check at run-time atom hash collisions,
// that is whether 2 different atoms a1 and a2 have the same hash,
// formally
//
// a1 != a2 and a1->get_hash() == a1->get_hash()
//
// Atom hash collision is unavoidable in principle but should be rare,
// this code may be useful for discovering pathological hash
// collisions. If enable, any hash collision should be warn logged.
// #define CHECK_ATOM_HASH_COLLISION

// If CHECK_ATOM_HASH_COLLISION is enabled, uncomment the following to
// that to abort if a collision is detected. This is an extreme yet
// convenient way to check whether a collision has occured.
// #define HALT_ON_COLLISON

using namespace opencog;

// Nothing should ever get the uuid of zero. Zero is reserved for
// "no atomtable" (in the persist code).
static std::atomic<UUID> _id_pool(1);

AtomTable::AtomTable(AtomTable* parent, AtomSpace* holder, bool transient) :
    _nameserver(nameserver())
{
    _as = holder;
    _environ = parent;
    if (_environ) _environ->_num_nested++;
    _num_nested = 0;
    _uuid = _id_pool.fetch_add(1, std::memory_order_relaxed);
    _transient = transient;

    // Connect signal to find out about type additions
    addedTypeConnection =
        _nameserver.typeAddedSignal().connect(
            std::bind(&AtomTable::typeAdded, this, std::placeholders::_1));
}

AtomTable::~AtomTable()
{
    std::unique_lock<std::shared_mutex> lck(_mtx);

    if (_environ) _environ->_num_nested--;
    _nameserver.typeAddedSignal().disconnect(addedTypeConnection);

    clear_all_atoms();

    if (0 != _num_nested)
        throw opencog::RuntimeException(TRACE_INFO,
           "AtomTable - deleteing atomtable %lu which has subtables!",
           _uuid);
}

void AtomTable::ready_transient(AtomTable* parent, AtomSpace* holder)
{
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomTable - ready called on non-transient atom table.");

    // Set the new parent environment and holder atomspace.
    _environ = parent;
    if (_environ) _environ->_num_nested++;
    _as = holder;
}

void AtomTable::clear_transient()
{
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomTable - clear_transient called on non-transient atom table.");

    std::unique_lock<std::shared_mutex> lck(_mtx);

    // Clear all the atoms
    clear_all_atoms();

    // Clear the  parent environment and holder atomspace.
    if (_environ) _environ->_num_nested--;
    _environ = NULL;
    _as = NULL;
}

void AtomTable::clear_all_atoms()
{
    typeIndex.clear();
}

void AtomTable::clear()
{
    std::unique_lock<std::shared_mutex> lck(_mtx);
    clear_all_atoms();
}

Handle AtomTable::getHandle(Type t, const std::string&& n) const
{
    Handle h(createNode(t, std::move(n)));
    return lookupHandle(h);
}

Handle AtomTable::getHandle(Type t, const HandleSeq&& seq) const
{
    Handle h(createLink(std::move(seq), t));
    return lookupHandle(h);
}

/// Find an equivalent atom that is exactly the same as the arg. If
/// such an atom is in the table, it is returned, else return nullptr.
Handle AtomTable::lookupHandle(const Handle& a) const
{
    if (nullptr == a) return Handle::UNDEFINED;

    std::shared_lock<std::shared_mutex> lck(_mtx);
    Handle h(typeIndex.findAtom(a));
    if (h) return h;

    if (_environ)
        return _environ->lookupHandle(a);

    return Handle::UNDEFINED;
}

/// Ask the atom if it belongs to this Atomtable. If so, we're done.
/// Otherwise, search for an equivalent atom that we might be holding.
Handle AtomTable::getHandle(const Handle& a) const
{
    if (nullptr == a) return Handle::UNDEFINED;

    if (in_environ(a))
        return a;

    return lookupHandle(a);
}

Handle AtomTable::add(const Handle& orig, bool force)
{
    // Can be null, if its a Value
    if (nullptr == orig) return Handle::UNDEFINED;

    // Is the atom already in this table, or one of its environments?
    if (not force and in_environ(orig))
        return orig;

    // Force computation of hash external to the locked section.
    orig->get_hash();

    // Lock before checking to see if this kind of atom is already in
    // the atomspace.  Lock, to prevent two different threads from
    // trying to add exactly the same atom.
    std::unique_lock<std::shared_mutex> lck(_mtx);
    if (not force) {
        if (in_environ(orig)) return orig;

        Handle hcheck(typeIndex.findAtom(orig));
        if (nullptr == hcheck and _environ)
            hcheck = _environ->lookupHandle(orig);
        if (hcheck) {
            // Oh we have it already. Update the values, as needed.
            hcheck->copyValues(orig);
            return hcheck;
        }
    } else {
        // If force-adding, we have to be more careful.  We're looking
        // for the atom in this table, and not some other table.
        Handle hcheck(typeIndex.findAtom(orig));
        if (hcheck) {
            hcheck->copyValues(orig);
            return hcheck;
        }
    }

    // Make a copy of the atom, if needed. Otherwise, use what we were
    // given. Not making a copy saves a lot of time, especially by
    // avoiding running the factories a second time. This is, however,
    // potentially buggy, if the user is sneaky and hands us an Atom
    // that should have gone through a factory, but did not.
    Handle atom(orig);
    if (atom->is_link()) {
        bool need_copy = false;
        if (atom->getAtomTable())
            need_copy = true;
        else
            for (const Handle& h : atom->getOutgoingSet())
                if (not in_environ(h)) { need_copy = true; break; }

        if (need_copy) {
            // Insert the outgoing set of this link.
            HandleSeq closet;
            // Reserving space improves emplace_back performance by 2x
            closet.reserve(atom->get_arity());
            for (const Handle& h : atom->getOutgoingSet()) {
                // operator->() will be null if its a Value that is
                // not an atom.
                if (nullptr == h.operator->()) return Handle::UNDEFINED;
                closet.emplace_back(add(h));
            }
            atom = createLink(std::move(closet), atom->get_type());
        } else {
            atom->unsetRemovalFlag();
        }
    }
    else if (atom->getAtomTable())
    {
        std::string name(atom->get_name());
        atom = createNode(atom->get_type(), std::move(name));
    }
    else
        atom->unsetRemovalFlag();

    if (atom != orig) atom->copyValues(orig);
    atom->setAtomSpace(_as);
    atom->install();
    atom->keep_incoming_set();

    typeIndex.insertAtom(atom);

    // Unlock, because the signal needs to run unlocked.
    lck.unlock();

    // Now that we are completely done, emit the added signal.
    // Don't emit signal until after the indexes are updated!
    _addAtomSignal.emit(atom);

    return atom;
}

void AtomTable::barrier()
{
}

size_t AtomTable::getSize() const
{
    return getNumAtomsOfType(ATOM, true);
}

size_t AtomTable::getNumNodes() const
{
    return getNumAtomsOfType(NODE, true);
}

size_t AtomTable::getNumLinks() const
{
    return getNumAtomsOfType(LINK, true);
}

size_t AtomTable::getNumAtomsOfType(Type type, bool subclass) const
{
    std::shared_lock<std::shared_mutex> lck(_mtx);

    size_t result = typeIndex.size(type);
    if (subclass)
    {
        // Also count subclasses of this type, if need be.
        Type ntypes = _nameserver.getNumberOfClasses();
        for (Type t = ATOM; t<ntypes; t++)
        {
            if (t != type and _nameserver.isA(t, type))
                result += typeIndex.size(t);
        }
    }

    if (_environ)
        result += _environ->getNumAtomsOfType(type, subclass);

    return result;
}

Handle AtomTable::getRandom(RandGen *rng) const
{
    size_t x = rng->randint(getSize());

    Handle randy(Handle::UNDEFINED);

    // XXX TODO it would be considerably more efficient to go into the
    // the type index, and decrement x by the size of the index for
    // each type.  This would speed up the algo by about 100 (by about
    // the number of types that are in use...).
    foreachHandleByType(
        [&](Handle h)->void {
            if (0 == x) randy = h;
            x--;
        },
        ATOM, true);
    return randy;
}

HandleSet AtomTable::extract(Handle& handle, bool recursive)
{
    HandleSet result;

    // Make sure the atom is fully resolved before we go about
    // deleting it.
    handle = getHandle(handle);

    if (nullptr == handle or handle->isMarkedForRemoval()) return result;

    // Perhaps the atom is not in any table? Or at least, not in this
    // atom table? Its a user-error if the user is trying to extract
    // atoms that are not in this atomspace, but we're going to be
    // silent about this error -- it seems pointless to throw.
    AtomTable* other = handle->getAtomTable();
    if (other != this)
    {
        if (not in_environ(handle)) return result;
        return other->extract(handle, recursive);
    }

    // Lock before fetching the incoming set. (Why, exactly??)
    // Because if multiple threads are trying to delete the same
    // atom, then ... ???
    std::unique_lock<std::shared_mutex> lck(_mtx);

    if (handle->isMarkedForRemoval()) return result;
    handle->markForRemoval();

    // If recursive-flag is set, also extract all the links in the atom's
    // incoming set
    if (recursive) {
        // We need to make a copy of the incoming set because the
        // recursive call will trash the incoming set when the atom
        // is removed.
        IncomingSet is(handle->getIncomingSet());

        IncomingSet::iterator is_it = is.begin();
        IncomingSet::iterator is_end = is.end();
        for (; is_it != is_end; ++is_it)
        {
            Handle his(*is_it);
            DPRINTF("[AtomTable::extract] incoming set: %s",
                 (his) ? his->to_string().c_str() : "INVALID HANDLE");

            // Something is seriously screwed up if the incoming set
            // is not in this atomtable, and its not a child of this
            // atom table.  So flag that as an error; it will assert
            // a few dozen lines later, below.
            AtomTable* other = his->getAtomTable();
            if (other and other != this and not other->in_environ(handle)) {
                logger().warn() << "AtomTable::extract() internal error, "
                                << "non-DAG membership.";
            }
            if (not his->isMarkedForRemoval()) {
                DPRINTF("[AtomTable::extract] marked for removal is false");
                if (other) {
                    HandleSet ex = other->extract(his, true);
                    result.insert(ex.begin(), ex.end());
                }
            }
        }
    }

    // The check is done twice: the call to getIncomingSetSize() can
    // return a non-zero value if the incoming set has weak pointers to
    // deleted atoms. Thus, a second check is made for strong pointers,
    // since getIncomingSet() converts weak to strong.
    if (not recursive and 0 < handle->getIncomingSetSize())
    {
        IncomingSet iset(handle->getIncomingSet());
        if (0 < iset.size())
        {
            // User asked for a non-recursive remove, and the
            // atom is still referenced. So, do nothing.
            handle->unsetRemovalFlag();
            return result;
        }
    }

    // Issue the atom removal signal *BEFORE* the atom is actually
    // removed.  This is needed so that certain subsystems, e.g. the
    // Agent system activity table, can correctly manage the atom;
    // it needs info that gets blanked out during removal.
    // Pfft. Give up the pretension. This is a recursive lock;
    // unlocking it once is not enough, because it can still be
    // recurisvely locked.
    // lck.unlock();
    _removeAtomSignal.emit(handle);
    // lck.lock();

    typeIndex.removeAtom(handle);

    // Remove handle from other incoming sets.
    handle->remove();

    handle->setAtomSpace(nullptr);

    result.insert(handle);
    return result;
}

/// This is the resize callback, when a new type is dynamically added.
void AtomTable::typeAdded(Type t)
{
    std::unique_lock<std::shared_mutex> lck(_mtx);
    typeIndex.resize();
}
