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

#include <boost/bind.hpp>

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
// #include <opencog/atoms/bind/DeleteLink.h>
#include <opencog/atomspace/types.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

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

AtomSpace::AtomSpace(AtomSpace* parent) :
    atomTable(parent? &parent->atomTable : NULL, this),
    bank(atomTable),
    backing_store(NULL)
{
}

AtomSpace::~AtomSpace()
{
    // Be sure to disconnect the attention bank signals before the
    // atom table destructor runs. XXX FIXME yes this is an ugly hack.
    bank.shutdown();
}

AtomSpace::AtomSpace(const AtomSpace&) :
    atomTable(NULL),
    bank(atomTable),
    backing_store(NULL)
{
     throw opencog::RuntimeException(TRACE_INFO,
         "AtomSpace - Cannot copy an object of this class");
}

AtomSpace& AtomSpace::operator=(const AtomSpace&)
{
     throw opencog::RuntimeException(TRACE_INFO,
         "AtomSpace - Cannot copy an object of this class");
}


// ====================================================================

void AtomSpace::registerBackingStore(BackingStore *bs)
{
    backing_store = bs;
}

void AtomSpace::unregisterBackingStore(BackingStore *bs)
{
    if (bs == backing_store) backing_store = NULL;
}

// ====================================================================

Handle AtomSpace::add_atom(AtomPtr atom, bool async)
{
    if (nullptr == atom) return Handle();

    // Is this atom already in the atom table?
    Handle hexist(atomTable.getHandle(atom));
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    Type t = atom->getType();
    if (backing_store and not backing_store->ignoreType(t))
    {
        AtomPtr ba;
        NodePtr n(NodeCast(atom));
        if (n) {
            ba = backing_store->getNode(n->getType(),
                                        n->getName().c_str());
        } else {
            LinkPtr l(LinkCast(atom));
            if (l)
                 ba = backing_store->getLink(l->getType(),
                                             l->getOutgoingSet());
        }
        if (ba) {
            return atomTable.add(ba, async);
        }
    }

    // If we are here, neither the AtomTable nor backing store know
    // about this atom. Just add it.  If it is a DeleteLink, then the
    // addition will fail. Deal with it.
    Handle rh;
    try {
        rh = atomTable.add(atom, async);
    }
    catch (const DeleteException& ex) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
        if (backing_store)
// Under construction ....
	        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return rh;
}

Handle AtomSpace::add_node(Type t, const string& name,
                           bool async)
{
    // Is this atom already in the atom table?
    Handle hexist(atomTable.getHandle(t, name));
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (backing_store and not backing_store->ignoreType(t))
    {
        NodePtr n(backing_store->getNode(t, name.c_str()));
        if (n) return atomTable.add(n, async);
    }

    // If we are here, neither the AtomTable nor backing store know about
    // this atom. Just add it.
    return atomTable.add(createNode(t, name), async);
}

Handle AtomSpace::get_node(Type t, const string& name)
{
    // Is this atom already in the atom table?
    Handle hexist = atomTable.getHandle(t, name);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (backing_store and not backing_store->ignoreType(t))
    {
        NodePtr n(backing_store->getNode(t, name.c_str()));
        if (n) {
            return atomTable.add(n, false);
        }
    }

    // If we are here, nobody knows about this.
    return Handle::UNDEFINED;
}

Handle AtomSpace::add_link(Type t, const HandleSeq& outgoing,
                           bool async)
{
    // Is this atom already in the atom table?
    Handle hexist = atomTable.getHandle(t, outgoing);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (backing_store and not backing_store->ignoreType(t))
    {
        // If any of the outgoing set is ignorable, we will not
        // fetch the thing from the backing store.
        if (not std::any_of(outgoing.begin(), outgoing.end(),
            [this](Handle ho) { return backing_store->ignoreAtom(ho); }))
        {
            LinkPtr l(backing_store->getLink(t, outgoing));
            if (l) {
                // Put the atom into the atomtable, so it gets placed
                // in indices, so we can find it quickly next time.
                return atomTable.add(l, async);
            }
        }
    }

    // If we are here, neither the AtomTable nor backing store know
    // about this atom. Just add it.  If it is a DeleteLink, then the
    // addition will fail. Deal with it.
    Handle rh;
    try {
        rh = atomTable.add(createLink(t, outgoing), async);
    }
    catch (const DeleteException& ex) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
        if (backing_store)
// Under construction ....
	        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return rh;
}

Handle AtomSpace::get_link(Type t, const HandleSeq& outgoing)
{
    // Is this atom already in the atom table?
    Handle hexist = atomTable.getHandle(t, outgoing);
    if (hexist) return hexist;

    // If we are here, the AtomTable does not yet know about this atom.
    // Maybe the backing store knows about this atom.
    if (backing_store and not backing_store->ignoreType(t))
    {
        // If any of the outgoing set is ignorable, we will not
        // fetch the thing from the backing store.
        if (not std::any_of(outgoing.begin(), outgoing.end(),
            [this](Handle ho) { return backing_store->ignoreAtom(ho); }))
        {
            LinkPtr l(backing_store->getLink(t, outgoing));
            if (l) {
                // Register the atom with the atomtable (so it
                // gets placed in indices)
                return atomTable.add(l, false);
            }
        }
    }

    // If we are here, nobody knows about this.
    return Handle::UNDEFINED;
}

void AtomSpace::store_atom(Handle h)
{
    if (NULL == backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    backing_store->storeAtom(h);
}

Handle AtomSpace::fetch_atom(Handle h)
{
    if (NULL == backing_store)
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
    Handle hb(atomTable.getHandle(h));
    if (atomTable.holds(hb))
        return hb;

    // Case 2:
    // This atom is not yet in any (this??) atomspace; go get it.
    if (NULL == h->getAtomTable()) {
        AtomPtr ba;
        NodePtr n(NodeCast(h));
        if (n) {
            ba = backing_store->getNode(n->getType(),
                                        n->getName().c_str());
        } else {
            LinkPtr l(LinkCast(h));
            if (l)
                 ba = backing_store->getLink(l->getType(),
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

    return atomTable.add(h, false);
}

Handle AtomSpace::fetch_atom(UUID uuid)
{
    if (NULL == backing_store)
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
    Handle hb(atomTable.getHandle(uuid));
    if (atomTable.holds(hb))
        return hb;

    // Case 2:
    // We don't have the atom for this UUID, then go get it.
    AtomPtr a(backing_store->getAtom(uuid));

    // If we still don't have an atom, then the requested UUID
    // was "insane", that is, unknown by either the atom table
    // (case 1) or the backend.
    if (NULL == a.operator->())
        throw RuntimeException(TRACE_INFO,
            "Asked backend for an unknown handle; UUID=%lu\n",
            uuid);

    return atomTable.add(a, false);
}

Handle AtomSpace::get_atom(Handle h)
{
    Handle he(atomTable.getHandle(h));
    if (he) return he;
    if (backing_store)
        return fetch_atom(h);
    return Handle::UNDEFINED;
}

Handle AtomSpace::get_atom(UUID uuid)
{
    Handle he(atomTable.getHandle(uuid));
    if (he) return he;
    if (backing_store)
        return fetch_atom(uuid);
    return Handle::UNDEFINED;
}

Handle AtomSpace::fetch_incoming_set(Handle h, bool recursive)
{
    if (NULL == backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    h = get_atom(h);

    if (nullptr == h) return Handle::UNDEFINED;

    // Get everything from the backing store.
    HandleSeq iset = backing_store->getIncomingSet(h);
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
    if (backing_store) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
// Under construction ....
        throw RuntimeException(TRACE_INFO, "Not implemented!!!");
    }
    return 0 < atomTable.extract(h, recursive).size();
}

void AtomSpace::clear()
{
    std::vector<Handle> allAtoms;

    atomTable.getHandlesByType(back_inserter(allAtoms), ATOM, true, false);

    DPRINTF("atoms in allAtoms: %lu\n", allAtoms.size());

    Logger::Level save = logger().getLevel();
    logger().setLevel(Logger::DEBUG);

    // XXX FIXME TODO This is a stunningly inefficient way to clear the
    // atomspace! This will take minutes on any decent-sized atomspace!
    std::vector<Handle>::iterator i;
    for (i = allAtoms.begin(); i != allAtoms.end(); ++i) {
        purge_atom(*i, true);
    }

    allAtoms.clear();
    atomTable.getHandlesByType(back_inserter(allAtoms), ATOM, true, false);
    assert(allAtoms.size() == 0);

    logger().setLevel(save);
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
