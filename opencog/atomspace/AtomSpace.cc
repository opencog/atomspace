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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/proto/types.h>

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
    _atom_table(parent? &parent->_atom_table : nullptr, this, transient),
    _backing_store(nullptr),
    _transient(transient)
{
}

AtomSpace::~AtomSpace()
{
}

void AtomSpace::ready_transient(AtomSpace* parent)
{
    _atom_table.ready_transient(parent? &parent->_atom_table : nullptr, this);
}

void AtomSpace::clear_transient()
{
    _atom_table.clear_transient();
}

bool AtomSpace::compare_atomspaces(const AtomSpace& space_first,
                                   const AtomSpace& space_second,
                                   bool check_truth_values,
                                   bool emit_diagnostics)
{
    // Compare sizes
    if (space_first.get_size() != space_second.get_size())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - size " << 
                    space_first.get_size() << " != size " << 
                    space_second.get_size() << std::endl;
        return false;
    }

    // Compare node count
    if (space_first.get_num_nodes() != space_second.get_num_nodes())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - node count " << 
                    space_first.get_num_nodes() << " != node count " << 
                    space_second.get_num_nodes() << std::endl;
        return false;
    }

    // Compare link count
    if (space_first.get_num_links() != space_second.get_num_links())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - link count " << 
                    space_first.get_num_links() << " != link count " << 
                    space_second.get_num_links() << std::endl;
        return false;
    }

    // If we get this far, we need to compare each individual atom.

    // Get the atoms in each atomspace.
    HandleSeq atomsInFirstSpace, atomsInSecondSpace;
    space_first.get_all_atoms(atomsInFirstSpace);
    space_second.get_all_atoms(atomsInSecondSpace);

    // Uncheck each atom in the second atomspace.
    for (auto atom : atomsInSecondSpace)
        atom->setUnchecked();

    // Loop to see if each atom in the first has a match in the second.
    const AtomTable& table_second = space_second._atom_table;
    for (auto atom_first : atomsInFirstSpace)
    {
        Handle atom_second = table_second.getHandle(atom_first);

        if( false)
        {
        Handle atom_second;
        if (atom_first->is_node())
        {
            atom_second = table_second.getHandle(atom_first->get_type(),
                        atom_first->get_name());
        }
        else if (atom_first->is_link())
        {
            atom_second =  table_second.getHandle(atom_first->get_type(),
                        atom_first->getOutgoingSet());
        }
        else
        {
             throw opencog::RuntimeException(TRACE_INFO,
                 "AtomSpace::compare_atomspaces - atom not Node or Link");
        }
        }

        // If the atoms don't match because one of them is null.
        if ((atom_first and not atom_second) or
            (atom_second and not atom_first))
        {
            if (emit_diagnostics)
            {
                if (atom_first)
                    std::cout << "compare_atomspaces - first atom " << 
                            atom_first->to_string() << " != NULL " <<
                            std::endl;
                if (atom_second)
                    std::cout << "compare_atomspaces - first atom " <<
                            "NULL != second atom " << 
                            atom_second->to_string() << std::endl;
            }
            return false;
        }

        // If the atoms don't match... Compare the atoms not the pointers
        // which is the default if we just use Handle operator ==.
        if (*((AtomPtr) atom_first) != *((AtomPtr) atom_second))
        {
            if (emit_diagnostics)
                std::cout << "compare_atomspaces - first atom " << 
                        atom_first->to_string() << " != second atom " <<
                        atom_second->to_string() << std::endl;
            return false;
        }

        // Check the truth values...
        if (check_truth_values)
        {
            TruthValuePtr truth_first = atom_first->getTruthValue();
            TruthValuePtr truth_second = atom_second->getTruthValue();
            if (*truth_first != *truth_second)
            {
                if (emit_diagnostics)
                    std::cout << "compare_atomspaces - first truth " << 
                            atom_first->to_string() << " != second truth " <<
                            atom_second->to_string() << std::endl;
                return false;
            }
        }

        // Set the check for the second atom.
        atom_second->setChecked();
    }

    // Make sure each atom in the second atomspace has been checked.
    bool all_checked = true;
    for (auto atom : atomsInSecondSpace)
    {
        if (!atom->isChecked())
        {
            if (emit_diagnostics)
                std::cout << "compare_atomspaces - unchecked space atom " << 
                        atom->to_string() << std::endl;
            all_checked = false;
        }
    }
    if (!all_checked)
        return false;

    // If we get this far, then the spaces are equal.
    return true;
}

bool AtomSpace::operator==(const AtomSpace& other) const
{
    return compare_atomspaces(*this, other, CHECK_TRUTH_VALUES, 
            DONT_EMIT_DIAGNOSTICS);
}

bool AtomSpace::operator!=(const AtomSpace& other) const
{
    return not operator==(other);
}


// ====================================================================

bool AtomSpace::isAttachedToBackingStore()
{
    if (nullptr != _backing_store) return true;
    return false;
}

void AtomSpace::registerBackingStore(BackingStore *bs)
{
    if (isAttachedToBackingStore())
        throw RuntimeException(TRACE_INFO,
            "AtomSpace is already connected to a BackingStore.");

    _backing_store = bs;
}

void AtomSpace::unregisterBackingStore(BackingStore *bs)
{
    if (not isAttachedToBackingStore())
        throw RuntimeException(TRACE_INFO,
            "AtomSpace is not connected to a BackingStore.");

    if (bs == _backing_store) _backing_store = nullptr;
}

// ====================================================================

Handle AtomSpace::add_atom(const Handle& h, bool async)
{
    // If it is a DeleteLink, then the addition will fail. Deal with it.
    Handle rh;
    try {
        rh = _atom_table.add(h, async);
    }
    catch (const DeleteException& ex) {
        // Atom deletion has not been implemented in the backing store
        // This is a major to-do item.
        if (_backing_store)
           _backing_store->removeAtom(h, false);
    }
    return rh;
}

Handle AtomSpace::add_node(Type t, const string& name,
                           bool async)
{
    return _atom_table.add(createNode(t, name), async);
}

Handle AtomSpace::get_node(Type t, const string& name)
{
    return _atom_table.getHandle(t, name);
}

Handle AtomSpace::add_link(Type t, const HandleSeq& outgoing, bool async)
{
    // If it is a DeleteLink, then the addition will fail. Deal with it.
    Handle rh;
    try {
        rh = _atom_table.add(createLink(outgoing, t), async);
    }
    catch (const DeleteException& ex) {
        if (_backing_store) {
           Handle h(createLink(outgoing, t));
           _backing_store->removeAtom(h, false);
        }
    }
    return rh;
}

Handle AtomSpace::get_link(Type t, const HandleSeq& outgoing)
{
    return _atom_table.getHandle(t, outgoing);
}

void AtomSpace::store_atom(const Handle& h)
{
    if (nullptr == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    _backing_store->storeAtom(h);
}

Handle AtomSpace::fetch_atom(const Handle& h)
{
    if (nullptr == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");
    if (nullptr == h) return Handle::UNDEFINED;

    // First, make sure the atom is actually in the atomspace.
    Handle hc(_atom_table.add(h, false));

    // Now, get the latest values from the backing store.
    // The operation here is to CLOBBER the values, NOT to merge them!
    // The goal of an explicit fetch is to explicitly fetch the values,
    // and not to play monkey-shines with them.  If you want something
    // else, then save the old TV, fetch the new TV, and combine them
    // with your favorite algo.
    Handle hv;
    if (h->is_node()) {
        hv = _backing_store->getNode(h->get_type(),
                                     h->get_name().c_str());
    }
    else if (h->is_link()) {
        hv = _backing_store->getLink(h->get_type(),
                                     h->getOutgoingSet());
    }

    // Hmm I don't quite get it ...  shouldn't hv and hc be the same
    // atom, by now? So the below should not be needed...!?
    if (hv and hv != hc)
    {
        hc->copyValues(hv);
        TruthValuePtr tv = hv->getTruthValue();
        hc->setTruthValue(tv);
    }

    return hc;
}

Handle AtomSpace::fetch_incoming_set(Handle h, bool recursive)
{
    if (nullptr == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    h = get_atom(h);
    if (nullptr == h) return h;

    // Get everything from the backing store.
    _backing_store->getIncomingSet(_atom_table, h);

    if (not recursive) return h;

    IncomingSet vh(h->getIncomingSet());
    for (const LinkPtr& lp : vh)
        fetch_incoming_set(Handle(lp), true);

    return h;
}

Handle AtomSpace::fetch_incoming_by_type(Handle h, Type t)
{
    if (nullptr == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    h = get_atom(h);
    if (nullptr == h) return h;

    // Get everything from the backing store.
    _backing_store->getIncomingByType(_atom_table, h, t);

    return h;
}

void AtomSpace::fetch_valuations(Handle key, bool get_all_values)
{
    if (nullptr == _backing_store)
        throw RuntimeException(TRACE_INFO, "No backing store");

    key = get_atom(key);
    if (nullptr == key) return;

    // Get everything from the backing store.
    _backing_store->getValuations(_atom_table, key, get_all_values);
}

bool AtomSpace::remove_atom(Handle h, bool recursive)
{
    if (_backing_store)
        _backing_store->removeAtom(h, recursive);
    return 0 < _atom_table.extract(h, recursive).size();
}

std::string AtomSpace::to_string() const
{
	std::stringstream ss;
	ss << *this;
	return ss.str();
}

namespace std {

ostream& operator<<(ostream& out, const opencog::AtomSpace& as) {
    list<opencog::Handle> results;
    as.get_handles_by_type(back_inserter(results), opencog::ATOM, true);
    for (const opencog::Handle& h : results)
	    if (h->getIncomingSetSize() == 0)
		    out << h->to_string() << endl;
    return out;
}

} // namespace std
