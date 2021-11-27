/*
 * opencog/atomspace/AtomTable.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013-2021 Linas Vepstas <linasvepstas@gmail.com>
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

#include "AtomSpace.h"

#include <atomic>
#include <functional>
#include <iterator>
#include <mutex>
#include <set>

#include <stdlib.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/StateLink.h>
#include <opencog/atoms/core/TypedAtomLink.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/functional.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

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

// ====================================================================
// Nothing should ever get the uuid of zero. Zero is reserved for
// "no atomspace" (in the persist code).
static std::atomic<UUID> _id_pool(1);

void AtomSpace::init(void)
{
    _uuid = _id_pool.fetch_add(1, std::memory_order_relaxed);

    _name = "(uuid . " + std::to_string(_uuid) + ")";

    // Connect signal to find out about type additions
    addedTypeConnection =
        _nameserver.typeAddedSignal().connect(
            std::bind(&AtomSpace::typeAdded, this, std::placeholders::_1));
}

/**
 * Transient atomspaces are intended for use as scratch spaces, to hold
 * temporary results during evaluation, pattern matching and inference.
 */
AtomSpace::AtomSpace(AtomSpace* parent, bool transient) :
    Atom(ATOMSPACE),
    _read_only(false),
    _copy_on_write(transient),
    _transient(transient),
    _nameserver(nameserver())
{
    if (parent)
        _environ.push_back(HandleCast(parent->shared_from_this()));
    init();
}

AtomSpace::AtomSpace(AtomSpacePtr& parent) :
    Atom(ATOMSPACE),
    _read_only(false),
    _copy_on_write(false),
    _transient(false),
    _nameserver(nameserver())
{
    if (nullptr != parent)
        _environ.push_back(HandleCast(parent));

    init();
}

AtomSpace::AtomSpace(const HandleSeq& bases) :
    Atom(ATOMSPACE),
    _read_only(false),
    _copy_on_write(false),
    _transient(false),
    _nameserver(nameserver())
{
    _environ = bases;
    for (const Handle& base : bases)
    {
        if (not _nameserver.isA(base->get_type(), ATOMSPACE))
            throw opencog::RuntimeException(TRACE_INFO,
                    "AtomSpace - bases must be AtomSpaces!");
    }
    init();
}


AtomSpace::~AtomSpace()
{
    _nameserver.typeAddedSignal().disconnect(addedTypeConnection);
    clear_all_atoms();
}

void AtomSpace::ready_transient(AtomSpace* parent)
{
    _copy_on_write = true;

    if (not _transient or nullptr == parent)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomSpace - ready called on non-transient atom table.");

    // Set the new parent environment and holder atomspace.
    _environ.push_back(HandleCast(parent->shared_from_this()));
}

void AtomSpace::clear_transient()
{
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomSpace - clear_transient called on non-transient atom table.");

    // Clear all the atoms
    clear_all_atoms();

    // Clear the  parent environment and holder atomspace.
    _environ.clear();
}

void AtomSpace::clear_all_atoms()
{
    typeIndex.clear();
}

void AtomSpace::clear()
{
    clear_all_atoms();
}

Handle AtomSpace::getHandle(Type t, const std::string&& n) const
{
    Handle h(createNode(t, std::move(n)));
    return lookupHandle(h);
}

Handle AtomSpace::getHandle(Type t, const HandleSeq&& seq) const
{
    Handle h(createLink(std::move(seq), t));
    return lookupHandle(h);
}

/// Find an equivalent atom that is exactly the same as the arg. If
/// such an atom is in the table, it is returned, else return nullptr.
Handle AtomSpace::lookupHandle(const Handle& a) const
{
    Handle h(typeIndex.findAtom(a));
    if (h) return h;

    for (const Handle& base: _environ)
    {
        const Handle& found = AtomSpaceCast(base)->lookupHandle(a);
        if (found) return found;
    }

    return Handle::UNDEFINED;
}

/// Ask the atom if it belongs to this Atomtable. If so, we're done.
/// Otherwise, search for an equivalent atom that we might be holding.
Handle AtomSpace::get_atom(const Handle& a) const
{
    if (nullptr == a) return Handle::UNDEFINED;

    if (in_environ(a))
        return a;

    return lookupHandle(a);
}

/// Helper utility for adding atoms to the atomspace. Checks to see
/// if the indicated atom already is in the atomspace. If it is, it
/// returns that atom. Copies over values in the process.
Handle AtomSpace::check(const Handle& orig, bool force)
{
    if (not force) {
        // If we have it already, return it. If we had a fast and
        // easy COW check, then we could avoid `copyValues()` for
        // read-only atomspaces, and do a COW instead. But we don't,
        // so we won't.
        Handle hcheck(lookupHandle(orig));
        if (hcheck) {
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
    return Handle::UNDEFINED;
}

Handle AtomSpace::add(const Handle& orig, bool force)
{
    // Can be null, if its a Value
    if (nullptr == orig) return Handle::UNDEFINED;

    // Is the atom already in this table, or one of its environments?
    if (not force and in_environ(orig))
        return orig;

    // Force computation of hash.
    orig->get_hash();

    // Check to see if we already have this atom in the atomspace.
    const Handle& hc(check(orig, force));
    if (hc) return hc;

    // Make a copy of the atom, if needed. Otherwise, use what we were
    // given. Not making a copy saves a lot of time, especially by
    // avoiding running the factories a second time. This is, however,
    // potentially buggy, if the user is sneaky and hands us an Atom
    // that should have gone through a factory, but did not.
    // (We can solve this by setting a factory flag.)
    Handle atom(orig);
    if (atom->is_link()) {
        bool need_copy = false;
        if (atom->getAtomSpace())
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
                closet.emplace_back(add(h, force));
            }
            atom = createLink(std::move(closet), atom->get_type());
        } else {
            atom->unsetRemovalFlag();
        }
    }
    else if (atom->getAtomSpace())
    {
        std::string name(atom->get_name());
        atom = createNode(atom->get_type(), std::move(name));
    }
    else
        atom->unsetRemovalFlag();

    // Must set atomspace before insertion. This must be done before the
    // atom becomes visible at the typeIndex insert.  Likewise for setting
    // up the incoming set.
    atom->setAtomSpace(this);
    atom->keep_incoming_set();

    // Not entirily critical to copy values before the insert below, but
    // it is a nice gesture to anyone who expects and atomic atomspace add.
    if (atom != orig) atom->copyValues(orig);

    // Set up the incoming set. We have to do this before the typeIndex
    // insert, because that is when this atom becomes visible to other
    // threads. If we do this later, then insertion and deletion race,
    // specifically in SCMPrimitiveUTest, which trips over the assert
    // that the incoming set hash bucket hasn't yet been created even
    // as the atom is being deleted.
    atom->install();

    // Between the time that we last checked, and here, some other thread
    // may have raced and inserted this atom already. So the insert does
    // have to be an atomic test-n-set.
    Handle oldh(typeIndex.insertAtom(atom));
    if (oldh) return oldh;

    // Now that we are completely done, emit the added signal.
    // Don't emit signal until after the indexes are updated!
    _addAtomSignal.emit(atom);

    return atom;
}

void AtomSpace::barrier()
{
}

size_t AtomSpace::get_size() const
{
    return get_num_atoms_of_type(ATOM, true);
}

size_t AtomSpace::get_num_nodes() const
{
    return get_num_atoms_of_type(NODE, true);
}

size_t AtomSpace::get_num_links() const
{
    return get_num_atoms_of_type(LINK, true);
}

size_t AtomSpace::get_num_atoms_of_type(Type type, bool subclass) const
{
    size_t result = typeIndex.size(type, subclass);

    for (const Handle& base : _environ)
        result += AtomSpaceCast(base)->get_num_atoms_of_type(type, subclass);

    return result;
}

Handle AtomSpace::getRandom(RandGen *rng) const
{
    size_t x = rng->randint(get_size());

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

bool AtomSpace::extract_atom(const Handle& h, bool recursive)
{
    // Make sure the atom is fully resolved before we go about
    // deleting it.
    Handle handle(get_atom(h));

    if (nullptr == handle) return false;

    // User asked for a non-recursive remove, and the
    // atom is still referenced. So, do nothing.
    if (not recursive and not handle->isIncomingSetEmpty())
        return false;

    // Perhaps the atom is not in any table? Or at least, not in this
    // atom table? Its a user-error if the user is trying to extract
    // atoms that are not in this atomspace, but we're going to be
    // silent about this error -- it seems pointless to throw.
    AtomSpace* other = handle->getAtomSpace();
    if (other != this)
    {
        if (not in_environ(handle)) return false;
        return other->extract_atom(handle, recursive);
    }

    // If it is already marked, just return.
    if (handle->markForRemoval()) return false;

    // If recursive-flag is set, also extract all the links in the atom's
    // incoming set
    if (recursive) {
        // We need to make a copy of the incoming set because the
        // recursive call will trash the incoming set when the atom
        // is removed.
        HandleSeq is(handle->getIncomingSet());

        for (const Handle& his : is)
        {
            AtomSpace* other = his->getAtomSpace();

            // Something is seriously screwed up if the incoming set
            // is not in this atomspace, and its not a child of this
            // atomspace.  So flag that as an error; it will assert
            // a few dozen lines later, below.
            OC_ASSERT(nullptr == other or other == this or
                      other->in_environ(handle),
                "AtomSpace::extract() internal error, non-DAG membership.");

            if (not his->isMarkedForRemoval()) {
                if (other) {
                    if (other != this) {
                        other->extract_atom(his, true);
                    } else {
                        extract_atom(his, true);
                    }
                }
            }
        }
    }

    // Issue the atom removal signal *BEFORE* the atom is actually
    // removed.  This is needed so that certain subsystems, e.g. the
    // Agent system activity table, can correctly manage the atom;
    // it needs info that gets blanked out during removal.
    _removeAtomSignal.emit(handle);

    // Remove handle from other incoming sets.
    handle->remove();

    typeIndex.removeAtom(handle);

    handle->setAtomSpace(nullptr);
    return true;
}

/// This is the resize callback, when a new type is dynamically added.
void AtomSpace::typeAdded(Type t)
{
    typeIndex.resize();
}

/**
 * Returns the set of atoms of a given type (subclasses optionally).
 *
 * @param The desired type.
 * @param Whether type subclasses should be considered.
 * @return The set of atoms of a given type (subclasses optionally).
 */
void AtomSpace::get_handles_by_type(HandleSeq& hseq,
                                    Type type,
                                    bool subclass,
                                    bool parent,
                                    const AtomSpace* cas) const
{
    if (nullptr == cas) cas = this;

    // For STATE_LINK, and anything else inheriting from UNIQUE_LINK,
    // we only want the shallowest state, i.e. the state in *this*
    // AtomSpace. It hides/over-rides any state in any deeper atomspaces.
    // XXX FIXME do this for all UniqueLinks.
    // XXX Also, a minor bug, not sure if it matters: if parent is set
    // to true, then any UniqueLinks appearing here and in the parent
    // will be duplicated repeatedly in the result. Might be nice to
    // deduplicate, but that would cost CPU time.
    if (STATE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hseq.push_back(StateLinkCast(h)->get_link(cas));
    }
    else if (DEFINE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hseq.push_back(
                DefineLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else if (TYPED_ATOM_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hseq.push_back(
                TypedAtomLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else
    {
        typeIndex.get_handles_by_type(hseq, type, subclass);
    }

    // If an atom is already in the set, it will hide any duplicate
    // atom in the parent. What??? XXX NOT TRUE FIXME
    if (parent) {
        for (const Handle& base : _environ)
            AtomSpaceCast(base)->get_handles_by_type(hseq, type, subclass, parent, cas);
    }
}

/**
 * Returns the set of atoms of a given type, but only if they have
 * and empty outgoing set. 
 *
 * @param The desired type.
 * @param Whether type subclasses should be considered.
 * @return The set of atoms of a given type (subclasses optionally).
 */
void AtomSpace::get_root_set_by_type(HandleSeq& hseq,
                                     Type type,
                                     bool subclass,
                                     bool parent,
                                     const AtomSpace* cas) const
{
    // cut-n-paste of above.
    if (nullptr == cas) cas = this;

    // For STATE_LINK, and anything else inheriting from UNIQUE_LINK,
    // we only want the shallowest state, i.e. the state in *this*
    // AtomSpace. It hides/over-rides any state in any deeper atomspaces.
    // XXX FIXME do this for all UniqueLinks.
    if (STATE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_rootset_by_type(rawseq, type, subclass, cas);
        for (const Handle& h : rawseq)
            hseq.push_back(StateLinkCast(h)->get_link(cas));
    }
    else if (DEFINE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_rootset_by_type(rawseq, type, subclass, cas);
        for (const Handle& h : rawseq)
            hseq.push_back(
                DefineLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else if (TYPED_ATOM_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_rootset_by_type(rawseq, type, subclass, cas);
        for (const Handle& h : rawseq)
            hseq.push_back(
                TypedAtomLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else
    {
        typeIndex.get_rootset_by_type(hseq, type, subclass, cas);
    }

    // If an atom is already in the set, it will hide any duplicate
    // atom in the parent. What??? XXX NOT TRUE FIXME
    if (parent) {
        for (const Handle& base : _environ)
            AtomSpaceCast(base)->get_root_set_by_type(hseq, type, subclass, parent, cas);
    }
}
