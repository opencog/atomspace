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

#include <stdlib.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/StateLink.h>
#include <opencog/atoms/core/TypedAtomLink.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

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
    Frame(ATOM_SPACE),
    _read_only(false),
    _copy_on_write(transient),
    _transient(transient),
    _nameserver(nameserver())
{
    if (parent) {
        // Set the COW flag by default, for any Atomspace that sits on
        // top of another one. This provides a "common-sense" behavior
        // that most users would expect.
        _copy_on_write = true;
        _environ.push_back(AtomSpaceCast(parent));
        _outgoing.push_back(HandleCast(parent));
    }
    init();
}

AtomSpace::AtomSpace(AtomSpacePtr& parent) :
    Frame(ATOM_SPACE),
    _read_only(false),
    _copy_on_write(false),
    _transient(false),
    _nameserver(nameserver())
{
    if (nullptr != parent) {
        // Set the COW flag by default; it seems like a simpler
        // default than setting it to be write-through.
        _copy_on_write = true;
        _environ.push_back(parent);
        _outgoing.push_back(HandleCast(parent));
    }

    init();
}

AtomSpace::AtomSpace(const HandleSeq& bases) :
    Frame(ATOM_SPACE, bases),
    _read_only(false),
    _copy_on_write(false),
    _transient(false),
    _nameserver(nameserver())
{
    for (const Handle& base : bases)
    {
        _environ.push_back(AtomSpaceCast(base));
        if (not _nameserver.isA(base->get_type(), ATOM_SPACE))
            throw RuntimeException(TRACE_INFO,
                    "AtomSpace - bases must be AtomSpaces!");
    }

    if (0 < bases.size()) _copy_on_write = true;
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
        throw RuntimeException(TRACE_INFO,
                "AtomSpace - ready called on non-transient atom table.");

    // Set the new parent environment and holder atomspace.
    _environ.push_back(AtomSpaceCast(parent));
    _outgoing.push_back(HandleCast(parent));
}

void AtomSpace::clear_transient()
{
    if (not _transient)
        throw RuntimeException(TRACE_INFO,
                "AtomSpace - clear_transient called on non-transient atom table.");

    // Clear all the atoms
    clear_all_atoms();

    // Clear the  parent environment and holder atomspace.
    _environ.clear();
    _outgoing.clear();
}

void AtomSpace::clear_all_atoms()
{
    typeIndex.clear();
}

void AtomSpace::clear()
{
    clear_all_atoms();
}

/// Find an equivalent atom that is exactly the same as the arg. If
/// such an atom is in the table, it is returned, else return nullptr.
///
/// If there are multiple AtomSpace frames below this, they will be
/// searched as well, returning the shallowest copy that is found.
/// If there is a merge below (inheritance from multiple frames)
/// and the atom occurs in more than one frame, then the first such
/// found will be returned. It is left up to the user to resolve
/// cases of ambiguous multiple inheritance.
Handle AtomSpace::lookupHide(const Handle& a, bool hide) const
{
    const Handle& h(typeIndex.findAtom(a));
    if (h) {
        if (hide and h->isAbsent()) return Handle::UNDEFINED;
        return h;
    }

    // The complicated-looking while-loop is just implementing
    // a non-recursive version of what would otherwise be a
    // much simpler recursive call back to ourselves. There's
    // real code which will have environments that go thousands
    // deep, and we want to avoid having thousands of stack-frames,
    // here.
    size_t esz = _environ.size();
    if (0 == esz) return Handle::UNDEFINED;

    AtomSpacePtr eas = _environ[0];
    while (1 == esz)
    {
        const Handle& h(eas->typeIndex.findAtom(a));
        if (h) {
            if (hide and h->isAbsent()) return Handle::UNDEFINED;
            return h;
        }

        esz = eas->_environ.size();
        if (0 == esz) return Handle::UNDEFINED;

        // If there's a case of multiple inheritance, we make a
        // recursive call to find the atom.
        if (1 < esz)
        {
             for (const AtomSpacePtr& base: eas->_environ)
             {
                 const Handle& found = base->lookupHide(a, hide);
                 if (found) return found;
             }
             return Handle::UNDEFINED;
        }

        eas = eas->_environ[0];
    }

    // In the case of multiple inheritance, check each merge, until
    // one is found. This is done via recursive call.
    for (const AtomSpacePtr& base: _environ)
    {
        const Handle& found = base->lookupHide(a, hide);
        if (found) return found;
    }

    // Since we're using this function to hide an Atom,
    // this function cannot come up empty!
    OC_ASSERT(hide, "Internal Error!");

    return Handle::UNDEFINED;
}

/// Search for an equivalent atom that we might be holding.
Handle AtomSpace::get_atom(const Handle& a) const
{
    if (nullptr == a) return Handle::UNDEFINED;
    return lookupHandle(a);
}

/// Helper utility for adding atoms to the atomspace. Checks to see
/// if the indicated atom already is in the atomspace. If it is, it
/// returns that atom. Copies over values in the process.
Handle AtomSpace::check(const Handle& orig, bool force)
{
    // If we have a version of this atom in this AtomSpace, return it.
    // If it was previously marked hidden, unhide it first.
    const Handle& hc(typeIndex.findAtom(orig));
    if (hc) {
        if (hc->isAbsent()) {
            if (_read_only) return Handle::UNDEFINED;
            hc->setPresent();
        }
        return hc;
    }

    // If force-adding, do not recurse. We don't have it.
    if (force) return Handle::UNDEFINED;

    // If this is not a COW atomspace, then search recursively for
    // some version, any version of this atom in any parent atomspace.
    if (not _copy_on_write)
        return lookupHandle(orig);

    // If this is a transient atomspace, then just grab any version
    // we find. This alters the behavior of glob matching in the
    // MinerUTest (specifically, test_glob and test_typed_glob).
    // I'm not sure what the deal is, though, why we need to check.
    if (_transient)
        return lookupHandle(orig);

    // If its a node, then the shallowest matching node will do.
    if (not orig->is_link())
        return lookupHandle(orig);

    // If we have a COW atomspace, then we must respect the atomspace
    // membership of the provided outgoing set. That is, the user will
    // have supplied an explicit outgoing set, with explicit atomspace
    // membership, and we must respect that wish.
    const Handle& cand(lookupHandle(orig));
    if (not cand) return Handle::UNDEFINED;
    const HandleSeq& oset(orig->getOutgoingSet());
    const HandleSeq& cset(cand->getOutgoingSet());
    size_t sz = oset.size();
    for (size_t i=0; i<sz; i++) {
        AtomSpace* oa = oset[i]->getAtomSpace();
        if (nullptr == oa)
            return cand;
        if (oa != cset[i]->getAtomSpace())
            return Handle::UNDEFINED;
    }
    return cand;
}

Handle AtomSpace::add(const Handle& orig, bool force,
                      bool recurse, bool absent)
{
    // Can be null, if its a Value
    if (nullptr == orig) return Handle::UNDEFINED;

    // Atomspaces themselves are now Atoms. So it's not impossible for
    // someone to try to add them. But we don't actually want that to
    // happen, at least, not right now. Unsupported, because its not
    // really clear how this should work or how it should be used.
    if (ATOM_SPACE == orig->get_type()) return orig;

    // Check to see if we already have this atom in the atomspace.
    // If this is a top-level add, then copy the values over. If it's
    // a recursive add, the `orig` atom may contain wild values from
    // outer space, and we do not want to copy those.
    const Handle& hc(check(orig, force));
    if (hc) {
        if (not recurse and orig != hc)
            hc->copyValues(orig);
        return hc;
    }

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
                if (lookupHandle(h) != h)
                  { need_copy = true; break; }

        if (need_copy) {
            // Insert the outgoing set of this link.
            HandleSeq closet;
            // Reserving space improves emplace_back performance by 2x
            closet.reserve(atom->get_arity());
            for (const Handle& h : atom->getOutgoingSet()) {
                // operator->() will be null if its a Value that is
                // not an atom.
                if (nullptr == h.operator->()) return Handle::UNDEFINED;

                // If the goal of the add is to hide atoms in lower
                // layers, then avoid accidentally adding atoms that
                // unhide other atoms.
                if (absent)
                    closet.emplace_back(lookupHide(h));
                else
                    closet.emplace_back(add(h, false, true));
            }
            atom = createLink(std::move(closet), atom->get_type());

            // Now that the outgoing set is correct, check again to
            // see if we already have this atom in the atomspace.
            const Handle& hc(check(atom, force));
            if (hc and (not _copy_on_write or this == hc->getAtomSpace())) {
                if (not recurse)
                    hc->copyValues(orig);
                return hc;
            }

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
    {
        atom->unsetRemovalFlag();
    }

    // If we are shadowing a deeper atom, copy it's values.
    if (_transient or _copy_on_write)
    {
        Handle covered(lookupHandle(atom));
        if (covered) atom->copyValues(covered);
    }

    if (atom != orig)
        atom->copyValues(orig);

    // Must set atomspace before insertion. This must be done before the
    // atom becomes visible at the typeIndex insert.  Likewise for setting
    // up the incoming set.
    atom->setAtomSpace(this);
    atom->keep_incoming_set();

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
    const Handle& oldh(typeIndex.insertAtom(atom));
    if (oldh)
    {
        // If it was already in the index, then undo the install above.
        atom->setAtomSpace(nullptr);
        atom->remove();
        return oldh;
    }
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
    // If the flag is set, we need to deduplicate the atoms,
    // and then count them.
    if (_copy_on_write) {
        UnorderedHandleSet hset;
        shadow_by_type(hset, type, subclass, true, this);
        return hset.size();
    }

    size_t result = typeIndex.size(type, subclass);

    for (const AtomSpacePtr& base : _environ)
        result += base->get_num_atoms_of_type(type, subclass);

    return result;
}

bool AtomSpace::extract_atom(const Handle& h, bool recursive)
{
    if (nullptr == h) return false;

    // Make sure the atom is fully resolved before we go about
    // deleting it.
    const Handle& handle(get_atom(h));

    // Report success if its already gone.
    if (nullptr == handle) return true;

    // If the recursive-flag is set, then extract all the links in the
    // atom's incoming set. This might not succeed, if those atoms are
    // in other (higher) atomspaces (because recursion must not reach up
    // into those).
    if (recursive)
    {
        HandleSeq is(handle->getIncomingSet());
        for (const Handle& his : is)
        {
            AtomSpace* other = his->getAtomSpace();

            // Atoms in the incoming set must belong to some Atomspace
            // above that of `handle`.  Space frames must be in stacking
            // order.
            OC_ASSERT(nullptr == other or other == this or
                      other->in_environ(handle),
                "AtomSpace::extract() internal error, non-DAG membership.");

            if (not his->isMarkedForRemoval())
            {
                if (other and other->in_environ(this))
                    other->extract_atom(his, true);
                else
                    extract_atom(his, true);
            }
        }
    }
    else // if (not recursive)
    if (0 < handle->getIncomingSetSize())
    {
        // User asked for a non-recursive remove, and the atom still
        // appears in incoming sets. Do nothing for the ordinary case.
        if (not _copy_on_write)
            return false;

        // If this is a copy-on-write space, and any of the links are
        // in this atomspace, then do nothing. If they are all elsewhere,
        // then we must mask.
        HandleSeq is(handle->getIncomingSet());
        for (const Handle& his : is)
        {
            AtomSpace* other = his->getAtomSpace();
            if (this == other) return false;
        }

        // If we are here, then mask.
        const Handle& hide(add(handle, true, true, true));
        hide->setAbsent();
        return true;
    }

    // Atom seems to reside somewhere else. This "somewhere else" is
    // assumed to lie underneath this space.
    AtomSpace* other = handle->getAtomSpace();
    if (other != this)
    {
        // If the Atom is in some other AtomSpace that is not in our
        // environment, then it is a user error. ... Except that this
        // can be hit during multi-threaded racing of add-delete, as
        // witnessed by `UseCountUTest`.
        if (not in_environ(handle)) return false;

        // If this is a COW space, then force-add it, so that it
        // can hide the atom in the deeper space. (Because for COW
        // spaces, we are not allowed to reach down to its actual
        // location to delete it there.)
        if (_copy_on_write) {
            const Handle& hide(add(handle, true, true, true));
            hide->setAbsent();
            return true;
        }

        // Delegate to the other atomspace for processing.
        return other->extract_atom(handle, recursive);
    }

    // For COW spaces, when there is some atom in a lower space, then
    // we must hide that atom.  Otherwise, if there's nothing to hide,
    // then we can just delete. (i.e. fall through and delete.)
    if (_copy_on_write) {
        for (const AtomSpacePtr& base: _environ)
        {
            const Handle& found = base->lookupHandle(handle);
            if (found)
            {
                const Handle& hide(add(handle, true, true, true));
                hide->setAbsent();
                return true;
            }
        }
    }

    // If it is already marked, just return.
    if (handle->markForRemoval()) return false;

    // This check avoids a race condition with the add() method.
    // The add() method installs the atom into the incoming set,
    // which makes it visible externally. Another thread looks at
    // that incoming set and finds this atom, and initiates a
    // remove. The remove gets started, and reaches here, before
    // the adding thread has inserted into the type index. Thus,
    // the typeIndex remove below does not find it (because it's
    // not there yet). If we failed to return here, then code
    // below wipes out the incoming set, while the adder thread
    // finishes inserting the now-broken atom into the type index!
    // This can be hit in 1 out of 4 runs of `UseCountUTest`.
    //
    // One possibility is a "fix" that introduces new problems:
    // Alter the add() method to place the atom into the typeIndex
    // first, and only then update the incoming set. But this seems
    // to create a new problem: the atom becomes visible as soon as
    // it's added to the type index, exposing a window where it
    // briefly has broken incoming set.
    //
    if (not typeIndex.removeAtom(handle)) {
        handle->unsetRemovalFlag();
        return false;
    }

    // Remove handle from other incoming sets.
    handle->remove();
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

    // If this is a copy-on-write space, then deduplicate the Atoms,
    // returning the shallowest version of each Atom.
    if (_copy_on_write)
    {
        UnorderedHandleSet rawset;
        shadow_by_type(rawset, type, subclass, parent, cas);

        // Look for the shallowest version of each Atom.
        for (const Handle& h: rawset)
        {
            // hshallow might be null, if h is mask-deleted
            // in this AtomSpace (h->isAbsent() == true)
            const Handle& hshallow(lookupHandle(h));
            if (hshallow) hseq.push_back(hshallow);
        }
        return;
    }

    // For STATE_LINK, and most things inheriting from UNIQUE_LINK,
    // we only want the shallowest state, i.e. the state in *this*
    // AtomSpace. It hides/over-rides any state in any deeper atomspaces.
    // Do NOT DO THIS for GrantLink; grants must be unique over all
    // frames.
    //
    // XXX Also, a minor bug, not sure if it matters: if parent is set
    // to true, then any UniqueLinks appearing here and in the parent
    // will be duplicated repeatedly in the result. Might be nice to
    // deduplicate, but that would cost CPU time. (The copy_on_write
    // variant immediately above should haved handled this correctly,
    // I think. Not sure. Confused.)
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

    if (parent) {
        for (const AtomSpacePtr& base : _environ)
            base->get_handles_by_type(hseq, type, subclass, parent, cas);
    }
}

// Same as above, but works with an unordered set, instead of a vector.
// By working with a set instead of a sequence, there will not be any
// duplicate atoms due to shadowing of child spaces by parent spaces.
// However, the returned set is NOT guaranteed to contain the shallowest
// Atoms! These need to be obtained with a distinct step.
void AtomSpace::shadow_by_type(UnorderedHandleSet& hset,
                               Type type,
                               bool subclass,
                               bool parent,
                               const AtomSpace* cas) const
{
    // See the vector version of this code for documentation.
    if (STATE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hset.insert(StateLinkCast(h)->get_link(cas));
    }
    else if (DEFINE_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hset.insert(
                DefineLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else if (TYPED_ATOM_LINK == type)
    {
        HandleSeq rawseq;
        typeIndex.get_handles_by_type(rawseq, type, subclass);
        for (const Handle& h : rawseq)
            hset.insert(
                TypedAtomLink::get_link(UniqueLinkCast(h)->get_alias(), cas));
    }
    else
    {
        typeIndex.get_handles_by_type(hset, type, subclass);
    }

    if (parent) {
        for (const AtomSpacePtr& base : _environ)
            base->shadow_by_type(hset, type, subclass, parent, cas);
    }
}

void AtomSpace::get_handles_by_type(UnorderedHandleSet& hset,
                                    Type type,
                                    bool subclass,
                                    bool parent,
                                    const AtomSpace* cas) const
{
    // If this is a copy-on-write space, then deduplicate the Atoms,
    // returning the shallowest version of each Atom.
    if (_copy_on_write)
    {
        UnorderedHandleSet rawset;
        shadow_by_type(rawset, type, subclass, parent, cas);

        // Look for the shallowest version of each Atom.
        for (const Handle& h: rawset)
            hset.insert(lookupHandle(h));
        return;
    }

    shadow_by_type(hset, type, subclass, parent, cas);
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

    // For STATE_LINK, and most things inheriting from UNIQUE_LINK,
    // we only want the shallowest state, i.e. the state in *this*
    // AtomSpace. It hides/over-rides any state in any deeper atomspaces.
    // DO NOT DO THIS for GrantLinks! Grants must remain unique, over
    // all AtomSpace frames.
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
        for (const AtomSpacePtr& base : _environ)
            base->get_root_set_by_type(hseq, type, subclass, parent, cas);
    }
}

/// Return only those atoms that are marked with the isAbsent() flag.
/// Used by StorageNodes to maintain correct deletion state.
void AtomSpace::get_absent_atoms(HandleSeq& missing) const
{
    // Place the deepest atoms first. (Recurse first.)
    for (const AtomSpacePtr& base : _environ)
        base->get_absent_atoms(missing);

    HandleSeq rawseq;
    typeIndex.get_handles_by_type(rawseq, ATOM, true);
    for (const Handle& h: rawseq)
        if (h->isAbsent()) missing.push_back(h);
}

void AtomSpace::get_atoms_in_frame(HandleSeq& hseq) const
{
    typeIndex.get_handles_by_type(hseq, ATOM, true);
}
