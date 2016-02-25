/*
 * opencog/atomspace/AtomSpace.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009, 2013 Linas Vepstas
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

#include <string>
#include <iostream>
#include <fstream>
#include <list>

#include <stdlib.h>

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/types.h>

#include "AtomSpace.h"

//#define DPRINTF printf
#define DPRINTF(...)

using std::string;
using std::cerr;
using std::cout;
using std::endl;
using std::min;
using std::max;
using namespace opencog;

// ====================================================================

/**
 * Transient atomspaces skip some of the initialization steps,
 * so that they can be constructed more quickly.  Transient atomspaces
 * are typically used as scratch spaces, to hold temporary results
 * during evaluation, pattern matching and inference. Such temporary
 * spaces don't need some of the heavier-weight crud that atomspaces
 * are festooned with.
 */
AtomSpace::AtomSpace(AtomSpace* parent, bool transient) :
    _atom_table(parent? &parent->_atom_table : NULL, this, transient),
    _bank(_atom_table, transient),
    _backing_store(NULL),
    _transient(transient)
{
}

AtomSpace::~AtomSpace()
{
    // Be sure to disconnect the attention bank signals before the
    // atom table destructor runs. XXX FIXME yes this is an ugly hack.
    _bank.shutdown();
}

AtomSpace::AtomSpace(const AtomSpace&) :
    _atom_table(NULL),
    _bank(_atom_table, true),
    _backing_store(NULL)
{
     throw opencog::RuntimeException(TRACE_INFO,
         "AtomSpace - Cannot copy an object of this class");
}

void AtomSpace::ready_transient(AtomSpace* parent)
{
    _atom_table.ready_transient(parent? &parent->_atom_table : NULL, this);
}

void AtomSpace::clear_transient()
{
    _atom_table.clear_transient();
}

AtomSpace& AtomSpace::operator=(const AtomSpace&)
{
     throw opencog::RuntimeException(TRACE_INFO,
         "AtomSpace - Cannot copy an object of this class");
}


// ====================================================================

void AtomSpace::registerBackingStore(BackingStore *bs)
{
    _backing_store = bs;
}

void AtomSpace::unregisterBackingStore(BackingStore *bs)
{
    if (bs == _backing_store) _backing_store = NULL;
}

// ====================================================================

Handle AtomSpace::add_atom(AtomPtr atom, bool async)
{
    if (nullptr == atom) return Handle();

    // Is this atom already in the atom table?
    Handle hexist(_atom_table.getHandle(atom));
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    Type t = atom->getType();
    if (_backing_store and not _backing_store->ignoreType(t))
    {
        AtomPtr ba;
        NodePtr n(NodeCast(atom));
        if (n) {
            ba = _backing_store->getNode(n->getType(),
                                        n->getName().c_str());
        } else {
            LinkPtr l(LinkCast(atom));
            if (l)
                 ba = _backing_store->getLink(l->getType(),
                                             l->getOutgoingSet());
        }
        if (ba) {
            return _atom_table.add(ba, async);
        }
    }

    // If we are here, neither the AtomTable nor backing store know
    // about this atom. Just add it.  If it is a DeleteLink, then the
    // addition will fail. Deal with it.
    Handle rh;
    try {
        rh = _atom_table.add(atom, async);
    }
    catch (const DeleteException& ex) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
        if (_backing_store)
// Under construction ....
	        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return rh;
}

Handle AtomSpace::add_node(Type t, const string& name,
                           bool async)
{
    // Is this atom already in the atom table?
    Handle hexist(_atom_table.getHandle(t, name));
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (_backing_store and not _backing_store->ignoreType(t))
    {
        NodePtr n(_backing_store->getNode(t, name.c_str()));
        if (n) return _atom_table.add(n, async);
    }

    // If we are here, neither the AtomTable nor backing store know about
    // this atom. Just add it.
    return _atom_table.add(createNode(t, name), async);
}

Handle AtomSpace::get_node(Type t, const string& name)
{
    // Is this atom already in the atom table?
    Handle hexist = _atom_table.getHandle(t, name);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (_backing_store and not _backing_store->ignoreType(t))
    {
        NodePtr n(_backing_store->getNode(t, name.c_str()));
        if (n) {
            return _atom_table.add(n, false);
        }
    }

    // If we are here, nobody knows about this.
    return Handle::UNDEFINED;
}

Handle AtomSpace::add_link(Type t, const HandleSeq& outgoing,
                           bool async)
{
    // Is this atom already in the atom table?
    Handle hexist = _atom_table.getHandle(t, outgoing);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (_backing_store and not _backing_store->ignoreType(t))
    {
        // If any of the outgoing set is ignorable, we will not
        // fetch the thing from the backing store.
        if (not std::any_of(outgoing.begin(), outgoing.end(),
            [this](Handle ho) { return _backing_store->ignoreAtom(ho); }))
        {
            LinkPtr l(_backing_store->getLink(t, outgoing));
            if (l) {
                // Put the atom into the atomtable, so it gets placed
                // in indices, so we can find it quickly next time.
                return _atom_table.add(l, async);
            }
        }
    }

    // If we are here, neither the AtomTable nor backing store know
    // about this atom. Just add it.  If it is a DeleteLink, then the
    // addition will fail. Deal with it.
    Handle rh;
    try {
        rh = _atom_table.add(createLink(t, outgoing), async);
    }
    catch (const DeleteException& ex) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
        if (_backing_store)
// Under construction ....
	        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return rh;
}

Handle AtomSpace::get_link(Type t, const HandleSeq& outgoing)
{
    // Is this atom already in the atom table?
    Handle hexist = _atom_table.getHandle(t, outgoing);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (_backing_store and not _backing_store->ignoreType(t))
    {
        // If any of the outgoing set is ignorable, we will not
        // fetch the thing from the backing store.
        if (not std::any_of(outgoing.begin(), outgoing.end(),
            [this](Handle ho) { return _backing_store->ignoreAtom(ho); }))
        {
            LinkPtr l(_backing_store->getLink(t, outgoing));
            if (l) {
                // Register the atom with the atomtable (so it
                // gets placed in indices)
                return _atom_table.add(l, false);
            }
        }
    }

    // If we are here, nobody knows about this.
    return Handle::UNDEFINED;
}

void AtomSpace::store_atom(Handle h)
{
    if (NULL == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    _backing_store->storeAtom(h);
}

Handle AtomSpace::fetch_atom(Handle h)
{
    if (NULL == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");
    if (NULL == h) return h;

    // We deal with two distinct cases.
    // 1) If atom table already knows about this atom, then this
    //    function returns the atom-table's version of the atom.
    //    In particular, no attempt is made to reconcile the possibly
    //    differing truth values in the atomtable vs. backing store.
    //    Why?  Because it is likely that the user plans to over-write
    //    what is in the backend.
    // 2) If (1) does not hold, i.e. the atom is not in this table, nor
    //    it's environs, then assume that atom is from some previous
    //    (recursive) query; do fetch it from backing store (i.e. fetch
    //    the TV) and add it to the atomtable.
    // For case 2, if the atom is a link, then it's outgoing set is
    // fetched as well, as currently, a link cannot be added to the
    // atomtable, unless all of its outgoing set already is in the
    // atomtable.

    // Case 1:
    Handle hb(_atom_table.getHandle(h));
    if (_atom_table.holds(hb))
        return hb;

    // Case 2:
    // This atom is not yet in any (this??) atomspace; go get it.
    if (NULL == h->getAtomTable()) {
        AtomPtr ba;
        NodePtr n(NodeCast(h));
        if (n) {
            ba = _backing_store->getNode(n->getType(),
                                        n->getName().c_str());
        } else {
            LinkPtr l(LinkCast(h));
            if (l)
                 ba = _backing_store->getLink(l->getType(),
                                             l->getOutgoingSet());
        }

        // If we still don't have an atom, then the requested UUID
        // was "insane", that is, unknown by either the atom table
        // (case 1) or the backend.
        if (NULL == ba)
            throw RuntimeException(TRACE_INFO,
                "Asked backend for an atom %s\n",
                h->toString().c_str());
        h = ba;
    }

    return _atom_table.add(h, false);
}

Handle AtomSpace::fetch_atom(UUID uuid)
{
    if (NULL == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    // OK, we have to handle two distinct cases.
    // 1) If atom table already knows about this uuid, then this
    //    function returns the atom-table's version of the atom.
    //    In particular, no attempt is made to reconcile the possibly
    //    differing truth values in the atomtable vs. backing store.
    // 2) Else, get the atom corresponding to the UUID from storage.
    //    If the atom is a link, then it's outgoing set is fetched as
    //    well, as currently, a link cannot be added to the atomtable,
    //    unless all of its outgoing set already is in the atomtable.

    // Case 1:
    Handle hb(_atom_table.getHandle(uuid));
    if (_atom_table.holds(hb))
        return hb;

    // Case 2:
    // We don't have the atom for this UUID, then go get it.
    AtomPtr a(_backing_store->getAtom(uuid));

    // If we still don't have an atom, then the requested UUID
    // was "insane", that is, unknown by either the atom table
    // (case 1) or the backend.
    if (NULL == a.operator->())
        throw RuntimeException(TRACE_INFO,
            "Asked backend for an unknown handle; UUID=%lu\n",
            uuid);

    return _atom_table.add(a, false);
}

Handle AtomSpace::fetch_incoming_set(Handle h, bool recursive)
{
    if (NULL == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    h = get_atom(h);

    if (nullptr == h) return Handle::UNDEFINED;

    // Get everything from the backing store.
    HandleSeq iset = _backing_store->getIncomingSet(h);
    size_t isz = iset.size();
    for (size_t i=0; i<isz; i++) {
        Handle hi(iset[i]);
        if (recursive) {
            fetch_incoming_set(hi, true);
        } else {
            get_atom(hi);
        }
    }
    return h;
}

bool AtomSpace::remove_atom(Handle h, bool recursive)
{
    if (_backing_store) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
// Under construction ....
        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return 0 < _atom_table.extract(h, recursive).size();
}

void AtomSpace::clear()
{
    std::vector<Handle> allAtoms;

    _atom_table.getHandlesByType(back_inserter(allAtoms), ATOM, true, false);

    DPRINTF("atoms in allAtoms: %lu\n", allAtoms.size());

    // Uncomment to turn on logging at DEBUG level.
    // Logger::Level save = logger().get_level();
    // logger().set_level(Logger::DEBUG);

    // XXX FIXME TODO This is a stunningly inefficient way to clear the
    // atomspace! This will take minutes on any decent-sized atomspace!
    std::vector<Handle>::iterator i;
    for (i = allAtoms.begin(); i != allAtoms.end(); ++i) {
        purge_atom(*i, true);
    }

    allAtoms.clear();
    _atom_table.getHandlesByType(back_inserter(allAtoms), ATOM, true, false);
    assert(allAtoms.size() == 0);

    // logger().set_level(save);
}

namespace std {

ostream& operator<<(ostream& out, const opencog::AtomSpace& as) {
    list<opencog::Handle> results;
    as.get_handles_by_type(back_inserter(results), opencog::ATOM, true);
    for (const opencog::Handle& h : results)
	    if (h->getIncomingSetSize() == 0)
		    out << h->toString() << endl;
    return out;
}

} // namespace std
