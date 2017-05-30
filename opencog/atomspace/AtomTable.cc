/*
 * opencog/atomspace/AtomTable.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013-2015 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
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
#include <iterator>
#include <mutex>
#include <set>

#include <stdlib.h>
#include <boost/bind.hpp>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/DeleteLink.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/StateLink.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/functional.h>
#include <opencog/util/Logger.h>

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

// Nothing should ever get the uuid of zero. Zero is reserved for
// "no atomtable" (in the persist code).
static std::atomic<UUID> _id_pool(1);

AtomTable::AtomTable(AtomTable* parent, AtomSpace* holder, bool transient)
    // Hmm. Right now async doesn't work anyway, so lets not create
    // threads for it. It just makes using gdb that much harder.
    // FIXME later. Actually, the async idea is not going to work as
    // originally envisioned, anyway.  What we really need are
    // consistent views of the atomtable.
    // : _index_queue(this, &AtomTable::put_atom_into_index, transient?0:4)
    : _index_queue(this, &AtomTable::put_atom_into_index, 0)
{
    _as = holder;
    _environ = parent;
    _uuid = _id_pool.fetch_add(1, std::memory_order_relaxed);
    _size = 0;
    _num_nodes = 0;
    _num_links = 0;
    size_t ntypes = classserver().getNumberOfClasses();
    _size_by_type.resize(ntypes);
    _transient = transient;

    // Connect signal to find out about type additions
    addedTypeConnection =
        classserver().addTypeSignal().connect(
            boost::bind(&AtomTable::typeAdded, this, _1));
}

AtomTable::~AtomTable()
{
    // Disconnect signals. Only then clear the resolver.
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    addedTypeConnection.disconnect();

    // No one who shall look at these atoms shall ever again
    // find a reference to this atomtable.
    for (auto& pr : _atom_store) {
        Handle& atom_to_delete = pr.second;
        atom_to_delete->_atom_space = nullptr;

        // Aiee ... We added this link to every incoming set;
        // thus, it is our responsibility to remove it as well.
        // This is a stinky design, but I see no other way,
        // because it seems that we can't do this in the Atom
        // destructor (which is where this should be happening).
        if (atom_to_delete->isLink()) {
            LinkPtr link_to_delete = LinkCast(atom_to_delete);
            for (AtomPtr atom_in_out_set : atom_to_delete->getOutgoingSet()) {
                atom_in_out_set->remove_atom(link_to_delete);
            }
        }
    }
}

void AtomTable::ready_transient(AtomTable* parent, AtomSpace* holder)
{
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomTable - ready called on non-transient atom table.");

    // Set the new parent environment and holder atomspace.
    _environ = parent;
    _as = holder;
}

void AtomTable::clear_transient()
{
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomTable - clear_transient called on non-transient atom table.");

    // Clear all the atoms
    clear_all_atoms();

    // Clear the  parent environment and holder atomspace.
    _environ = NULL;
    _as = NULL;

}

void AtomTable::clear_all_atoms()
{
    // For now, this only works for transient atomspaces which do not
    // index the atoms. So throw an exception to warn against use in
    // normal atomspaces.
    if (not _transient)
        throw opencog::RuntimeException(TRACE_INFO,
                "AtomTable - clear_all_atoms called on non-transient atom table.");

    // Reset the size to zero.
    _size = 0;
    _num_nodes = 0;
    _num_links = 0;

    // Clear the by-type size cache.
    Type total_types = _size_by_type.size();
    for (Type type = ATOM; type < total_types; type++)
        _size_by_type[type] = 0;

    // Clear the atoms in the set.
    for (auto& pr : _atom_store) {
        Handle& atom_to_clear = pr.second;
        atom_to_clear->_atom_space = nullptr;

        // If this is a link we need to remove this atom from the incoming
        // sets for any atoms in this atom's outgoing set. See note in
        // the analogous loop in ~AtomTable above.
        if (atom_to_clear->isLink()) {
            LinkPtr link_to_clear = LinkCast(atom_to_clear);
            for (AtomPtr atom_in_out_set : atom_to_clear->getOutgoingSet()) {
                atom_in_out_set->remove_atom(link_to_clear);
            }
        }
    }

    // Clear the atom store. This will delete all the atoms since
    // this will be the last shared_ptr referecence, and set the
    // size of the set to 0.
    _atom_store.clear();
}

void AtomTable::clear()
{
    if (_transient)
    {
        // Do the fast clear since we're a transient atom table.
        clear_all_atoms();
    }
    else
    {
        HandleSeq allAtoms;

        getHandlesByType(back_inserter(allAtoms), ATOM, true, false);

        DPRINTF("atoms in allAtoms: %lu\n", allAtoms.size());

        // Uncomment to turn on logging at DEBUG level.
        // Logger::Level save = logger().get_level();
        // logger().set_level(Logger::DEBUG);

        // XXX FIXME TODO This is a stunningly inefficient way to clear the
        // atomtable! This will take minutes on any decent-sized atomspace!
        HandleSeq::iterator i;
        for (i = allAtoms.begin(); i != allAtoms.end(); ++i) {
            extract(*i, true);
        }

        allAtoms.clear();
        getHandlesByType(back_inserter(allAtoms), ATOM, true, false);
        assert(allAtoms.size() == 0);

        // logger().set_level(save);
    }
}

AtomTable& AtomTable::operator=(const AtomTable& other)
{
    throw opencog::RuntimeException(TRACE_INFO,
            "AtomTable - Cannot copy an object of this class");
}

AtomTable::AtomTable(const AtomTable& other)
    :_index_queue(this, &AtomTable::put_atom_into_index)
{
    throw opencog::RuntimeException(TRACE_INFO,
            "AtomTable - Cannot copy an object of this class");
}

Handle AtomTable::getHandle(Type t, const std::string& n) const
{
    // Special types need validation
    AtomPtr a;
    try {
        if (NUMBER_NODE == t) a = createNumberNode(n);
        else if (classserver().isA(t, TYPE_NODE)) a = createTypeNode(n);
        else a = createNode(t,n);
    }
    catch (...) { return Handle::UNDEFINED; }

    return getNodeHandle(a);
}

Handle AtomTable::getNodeHandle(const AtomPtr& orig) const
{
    AtomPtr a(orig);
    // The hash function will fail to find NumberNodes unless
    // they are in the proper format.
    if (NUMBER_NODE == a->getType()) {
       if (nullptr == NumberNodeCast(a))
           a = createNumberNode(a->getName());
    }

    ContentHash ch = a->get_hash();
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    auto range = _atom_store.equal_range(ch);
    auto bkt = range.first;
    auto end = range.second;
    for (; bkt != end; bkt++) {
        if (*((AtomPtr) bkt->second) == *a) {
            return bkt->second;
        }
    }

    if (_environ)
        return _environ->getHandle(a);
    return Handle::UNDEFINED;
}

Handle AtomTable::getHandle(Type t, const HandleSeq& seq) const
{
    AtomPtr a(createLink(seq, t));
    return getLinkHandle(a);
}

Handle AtomTable::getLinkHandle(const AtomPtr& orig, Quotation quotation) const
{
    AtomPtr a(orig);
    Type t = a->getType();
    const HandleSeq &seq = a->getOutgoingSet();
    bool unquoted = not quotation.is_quoted();

    // Update quotation for the outgoing given the atom type
    quotation.update(t);

    // Make sure all the atoms in the outgoing set are in a valid
    // format. One of the troublemakers here is the NumberNode, which
    // will hash incorrectly, unless its in proper format. We exclude
    // unquoted scope links from it, otherwise it will prematurely
    // abort and possibly miss alpha equivalent atom in _atom_store.
    if (not unquoted or not classserver().isA(t, SCOPE_LINK)) {
        HandleSeq resolved_seq;
        for (const Handle& ho : seq) {
            Handle rh(getHandle(ho, quotation));
            if (not rh) return Handle::UNDEFINED;
            resolved_seq.emplace_back(rh);
        }

        a = createLink(resolved_seq, t);
    }

    // Start searching to see if we have this atom.
    ContentHash ch = a->get_hash();

    // Currently, ScopeLinks use a custom hash, and, in order
    // for it to work, we must have an actual instance of the
    // class, so that the correct virtual method can be called.
    //
    // However, bad quotation nesting means that some things
    // that look like ScopeLinks are just invalid fragments
    // of search patterns. Ignore those.
    if (unquoted and classserver().isA(t, SCOPE_LINK)) {
        ScopeLinkPtr wanted = ScopeLinkCast(a);
        if (nullptr == wanted) {
            wanted = ScopeLinkCast(classserver().factory(Handle(a)));
        }
        ch = wanted->get_hash();
        a = wanted;
    }

    std::lock_guard<std::recursive_mutex> lck(_mtx);

    // So ... check to see if we have it or not.
    auto range = _atom_store.equal_range(ch);
    auto bkt = range.first;
    auto end = range.second;
    for (; bkt != end; bkt++) {
        if (*((AtomPtr) bkt->second) == *a) {
            return bkt->second;
        }
    }

    if (_environ) {
        return _environ->getHandle(a, quotation);
    }
    return Handle::UNDEFINED;
}

/// Find an equivalent atom that is exactly the same as the arg. If
/// such an atom is in the table, it is returned, else the return
/// is the bad handle.
Handle AtomTable::getHandle(const AtomPtr& a, Quotation quotation) const
{
    if (nullptr == a) return Handle::UNDEFINED;

    if (in_environ(a))
        return a->getHandle();

    if (a->isNode())
        return getNodeHandle(a);
    else if (a->isLink())
        return getLinkHandle(a, quotation);

    return Handle::UNDEFINED;
}

// Special atom types support.
AtomPtr AtomTable::cast_factory(Type atom_type, AtomPtr atom)
{
    // Very special handling for DeleteLink's
    if (DELETE_LINK == atom_type) {
        DeleteLinkPtr delp(DeleteLinkCast(atom));
        // If it can be cast, then its not an open term.
        if (nullptr != delp)
            return delp;

        // Trying to create a closed-term DeleteLink will throw.
        // This is a sign that we need to remove stuff.
        try {
            delp = createDeleteLink(*LinkCast(atom));
        }
        catch (...) {
            LinkPtr lp(LinkCast(atom));
            for (Handle ho : lp->getOutgoingSet()) {
                this->extract(ho);
            }
            return Handle();
        }
        return delp;
    }
    return atom;
}

/// The purpose of the clone factory is to create a private, unique
/// copy of the atom, so as to avoid accidental, unintentional
/// sharing with others. In particular, the atom that we are given
/// may already exist in some other atomspace; we want our own private
/// copy, in that case.
AtomPtr AtomTable::clone_factory(Type atom_type, AtomPtr atom)
{
    // Nodes of various kinds -----------
    if (NUMBER_NODE == atom_type)
        return createNumberNode(*NodeCast(atom));
    if (classserver().isA(atom_type, TYPE_NODE))
        return createTypeNode(*NodeCast(atom));
    if (classserver().isA(atom_type, NODE))
        return createNode(*NodeCast(atom));

    // The createLink *forces* a copy of the link to be made.
    return classserver().factory(Handle(createLink(*LinkCast(atom))));
}

#if 0
static void prt_diag(AtomPtr atom, size_t i, size_t arity, const HandleSeq& ogs)
{
    Logger::Level save = logger().getBackTraceLevel();
    logger().setBackTraceLevel(Logger::Level::NONE);
    logger().error() << "AtomTable - Insert link with "
               "invalid outgoing members";
    logger().error() << "Failing index i=" << i
                    << " and arity=" << arity;
    logger().error() << "Failing outset is this:";
    for (unsigned int fk=0; fk<arity; fk++)
        logger().error() << "outset i=" << fk;

    logger().error() << "link is " << atom->toString();
    logger().flush();
    logger().setBackTraceLevel(save);
}
#endif

Handle AtomTable::add(AtomPtr atom, bool async)
{
    // Can be null, if its a ProtoAtom
    if (nullptr == atom) return Handle::UNDEFINED;

    // Is the atom already in this table, or one of its environments?
    if (in_environ(atom))
        return atom->getHandle();

    AtomPtr orig(atom);
    Type atom_type = atom->getType();

    // Certain DeleteLinks can never be added!
    atom = cast_factory(atom_type, atom);
    if (nullptr == atom) return Handle();

    // If this atom is in some other atomspace or not in any atomspace,
    // then we need to clone it. We cannot insert it into this atomtable
    // as-is.  (We already know that its not in this atomspace, or its
    // environ.)
    if (atom->isLink()) {
        // Well, if the link was in some other atomspace, then
        // the outgoing set will probably be too. (It might not
        // be if the other atomspace is a child of this one).
        // So we recursively clone that too.
        HandleSeq closet;
        for (const Handle& h : atom->getOutgoingSet()) {
            // operator->() will be null if its a ProtoAtom that is
            // not an atom.
            if (nullptr == h.operator->()) return Handle::UNDEFINED;
            closet.emplace_back(add(h, async));
        }
        atom = createLink(closet, atom_type);
        atom = clone_factory(atom_type, atom);
    }

    // Clone, if we haven't done so already. We MUST maintain our own
    // private copy of the atom, else crazy things go wrong.
    else if (atom == orig)
        atom = clone_factory(atom_type, atom);

    // Lock before checking to see if this kind of atom is already in
    // the atomspace.  Lock, to prevent two different threads from
    // trying to add exactly the same atom.
    std::unique_lock<std::recursive_mutex> lck(_mtx);
    Handle hcheck(getHandle(orig));
    if (hcheck) return hcheck;

    atom->copyValues(Handle(orig));

    if (atom->isLink()) {
        if (STATE_LINK == atom_type) {
            // If this is a closed StateLink, (i.e. has no variables)
            // then make sure that the old state gets removed from the
            // atomtable. The atomtable must contain no more than one
            // closed state at a time.  Also: we must be careful to
            // update the incoming set in an atomic fashion, so that
            // the pattern matcher never finds two closed StateLinks
            // for any one given alias.  Any number of non-closed
            // StateLinks are allowed.

            StateLinkPtr slp(StateLinkCast(atom));
            if (slp->is_closed()) {
                try {
                    Handle alias = slp->get_alias();
                    Handle old_state = StateLink::get_link(alias);
                    atom->setAtomSpace(_as);
                    alias->swap_atom(LinkCast(old_state), slp);
                    extract(old_state, true);
                } catch (const InvalidParamException& ex) {}
            }
        }

        // Build the incoming set of outgoing atom h.
        size_t arity = atom->getArity();
        LinkPtr llc(LinkCast(atom));
        for (size_t i = 0; i < arity; i++) {
            llc->_outgoing[i]->insert_atom(llc);
        }
    }

    atom->keep_incoming_set();
    atom->setAtomSpace(_as);

    _size++;
    if (atom->isNode()) _num_nodes++;
    if (atom->isLink()) _num_links++;
    _size_by_type[atom->_type] ++;

    Handle h(atom->getHandle());
    _atom_store.insert({atom->get_hash(), h});

    if (not _transient and not async)
        put_atom_into_index(atom);

    // We can now unlock, since we are done.
    lck.unlock();

    // Update the indexes asynchronously
    if (not _transient and async)
        _index_queue.enqueue(atom);

    DPRINTF("Atom added: %s\n", atom->toString().c_str());
    return h;
}

void AtomTable::put_atom_into_index(const AtomPtr& atom)
{
    if (_transient)
        throw RuntimeException(TRACE_INFO,
          "AtomTable - transient should not index atoms!");

    std::unique_lock<std::recursive_mutex> lck(_mtx);
    Atom* pat = atom.operator->();
    typeIndex.insertAtom(pat);

    // We can now unlock, since we are done. In particular, the signals
    // need to run unlocked, since they may result in more atom table
    // additions.
    lck.unlock();

    // Now that we are completely done, emit the added signal.
    // Don't emit signal until after the indexes are updated!
    _addAtomSignal(atom->getHandle());
}

void AtomTable::barrier()
{
    _index_queue.flush_queue();
}

size_t AtomTable::getSize() const
{
    return _size;
}

size_t AtomTable::getNumNodes() const
{
    return _num_nodes;
}

size_t AtomTable::getNumLinks() const
{
    return _num_links;
}

size_t AtomTable::getNumAtomsOfType(Type type, bool subclass) const
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    size_t result = _size_by_type[type];
    if (subclass)
    {
        // Also count subclasses of this type, if need be.
        Type ntypes = _size_by_type.size();
        for (Type t = ATOM; t<ntypes; t++)
        {
            if (t != type and classserver().isA(type, t))
                result += _size_by_type[t];
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

AtomPtrSet AtomTable::extract(Handle& handle, bool recursive)
{
    AtomPtrSet result;

    // Make sure the atom is fully resolved before we go about
    // deleting it.
    AtomPtr atom(handle);
    atom = getHandle(atom);

    if (nullptr == atom or atom->isMarkedForRemoval()) return result;

    // Perhaps the atom is not in any table? Or at least, not in this
    // atom table? Its a user-error if the user is trying to extract
    // atoms that are not in this atomspace, but we're going to be
    // silent about this error -- it seems pointless to throw.
    AtomTable* other = atom->getAtomTable();
    if (other != this)
    {
        if (not in_environ(handle)) return result;
        return other->extract(handle, recursive);
    }

    // Lock before fetching the incoming set. Since getting the
    // incoming set also grabs a lock, we need this mutex to be
    // recursive. We need to lock here to avoid confusion if multiple
    // threads are trying to delete the same atom.
    std::unique_lock<std::recursive_mutex> lck(_mtx);

    if (atom->isMarkedForRemoval()) return result;
    atom->markForRemoval();

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
                 (his) ? his->toString().c_str() : "INVALID HANDLE");

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
                    AtomPtrSet ex = other->extract(his, true);
                    result.insert(ex.begin(), ex.end());
                }
            }
        }
    }

    // The check is done twice: the call to getIncomingSetSize() can
    // return a non-zero value if the incoming set has weak pointers to
    // deleted atoms. Thus, a second check is made for strong pointers,
    // since getIncomingSet() converts weak to strong.
    if (0 < handle->getIncomingSetSize())
    {
        IncomingSet iset(handle->getIncomingSet());
        if (0 < iset.size())
        {
            if (not recursive)
            {
                // User asked for a non-recursive remove, and the
                // atom is still referenced. So, do nothing.
                handle->unsetRemovalFlag();
                return result;
            }

            // Check for an invalid condition that should not occur. See:
            // https://github.com/opencog/opencog/commit/a08534afb4ef7f7e188e677cb322b72956afbd8f#commitcomment-5842682
            size_t ilen = iset.size();
            for (size_t i=0; i<ilen; i++)
            {
                // Its OK if the atom being extracted is in a link
                // that is not currently in any atom space, or if that
                // link is in a child subspace, in which case, we
                // extract from the child.
                //
                // A bit of a race can happen: when the unlock is
                // done below, to send the removed signal, another
                // thread can sneak in and get to here, if it is
                // deleting a different atom with an overlapping incoming
                // set.  Since the incoming set hasn't yet been updated
                // (that happens after re-acquiring the lock),
                // it will look like the incoming set has not yet been
                // fully cleared.  Well, it hasn't been, but as long as
                // we are marked for removal, things should end up OK.
                //
                // XXX this might not be exactly thread-safe, if
                // other atomspaces are involved...
                if (iset[i]->getAtomTable() != NULL and
                    (not iset[i]->getAtomTable()->in_environ(handle) or
                     not iset[i]->isMarkedForRemoval()))
                {
                    Logger::Level lev = logger().get_backtrace_level();
                    logger().set_backtrace_level(Logger::ERROR);
                    logger().warn() << "AtomTable::extract() internal error";
                    logger().warn() << "Non-empty incoming set of size "
                                    << ilen << " First trouble at " << i;
                    logger().warn() << "This atomtable=" << ((void*) this)
                                    << " other atomtale=" << ((void*) iset[i]->getAtomTable())
                                    << " in_environ=" << iset[i]->getAtomTable()->in_environ(handle);
                    logger().warn() << "This atom: " << handle->toString();
                    for (size_t j=0; j<ilen; j++) {
                        logger().warn() << "Atom j=" << j << " " << iset[j]->toString();
                        logger().warn() << "Marked: " << iset[j]->isMarkedForRemoval()
                                        << " Table: " << ((void*) iset[j]->getAtomTable());
                    }
                    logger().set_backtrace_level(lev);
                    atom->unsetRemovalFlag();
                    throw RuntimeException(TRACE_INFO,
                        "Internal Error: Cannot extract an atom with "
                        "a non-empty incoming set!");
                }
            }
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
    _removeAtomSignal(atom);
    // lck.lock();

    // Decrements the size of the table
    _size--;
    if (atom->isNode()) _num_nodes--;
    if (atom->isLink()) _num_links--;
    _size_by_type[atom->_type] --;

    auto range = _atom_store.equal_range(atom->get_hash());
    auto bkt = range.first;
    auto end = range.second;
    for (; bkt != end; bkt++) {
        if (handle == bkt->second) {
            _atom_store.erase(bkt);
            break;
        }
    }

    Atom* pat = atom.operator->();
    typeIndex.removeAtom(pat);

    if (atom->isLink()) {
        LinkPtr lll(LinkCast(atom));
        for (AtomPtr a : lll->_outgoing) {
            a->remove_atom(lll);
        }
    }

    // XXX Setting the atom table causes AVChanged signals to be emitted.
    // We should really do this unlocked, but I'm too lazy to fix, and
    // am hoping no one will notice. This will probably need to be fixed
    // someday.
    atom->setAtomSpace(nullptr);

    result.insert(atom);
    return result;
}

// This is the resize callback, when a new type is dynamically added.
void AtomTable::typeAdded(Type t)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    //resize all Type-based indexes
    size_t new_size = classserver().getNumberOfClasses();
    _size_by_type.resize(new_size);
    typeIndex.resize();
}

