/*
 * opencog/atoms/base/Atom.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
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

#include <set>
#include <sstream>

#ifndef WIN32
#include <unistd.h>
#endif

#include <opencog/util/misc.h>
#include <opencog/util/platform.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/AtomTable.h>

//! Atom flag
#define FETCHED_RECENTLY        1  //BIT0
#define MARKED_FOR_REMOVAL      2  //BIT1
// #define MULTIPLE_TRUTH_VALUES   4  //BIT2
// #define FIRED_ACTIVATION        8  //BIT3
// #define HYPOTETHICAL_FLAG       16 //BIT4
// #define REMOVED_BY_DECAY        32 //BIT5
#define CHECKED                 64  //BIT6

//#define DPRINTF printf
#define DPRINTF(...)

#undef Type

namespace std {

// The hash of a weak pointer is just the atom type. Actually,
// it *has* to be the atom type, as otherwise the hash buckets
// won't be correct, and getIncomingByType() will fail to be fast.
opencog::Type
hash<opencog::WinkPtr>::operator()(const opencog::WinkPtr& w) const noexcept
{
    opencog::LinkPtr h(w.lock());
    if (nullptr == h) return 0;
    return h->getType();
}

bool
equal_to<opencog::WinkPtr>::operator()(const opencog::WinkPtr& lw,
                                       const opencog::WinkPtr& rw) const noexcept
{
    opencog::Handle hl(lw.lock());
    opencog::Handle hr(rw.lock());
    return hl == hr;
}

} // namespace std

namespace opencog {

Atom::~Atom()
{
    _atom_space = NULL;
    if (0 < getIncomingSetSize()) {
        // This can't ever possibly happen. If it does, then there is
        // some very sick bug with the reference counting that the
        // shared pointers are doing. (Or someone explcitly called the
        // destructor! Which they shouldn't do.)
        OC_ASSERT(0 == getIncomingSet().size(),
             "Atom deletion failure; incoming set not empty for %s h=%x",
             classserver().getTypeName(_type).c_str(), get_hash());
    }
    drop_incoming_set();
}

// ==============================================================
// Whole lotta truthiness going on here.  Does it really need to be
// this complicated!?

void Atom::setTruthValue(TruthValuePtr newTV)
{
    if (nullptr == newTV) return;

    // If both old and new are e.g. DEFAULT_TV, then do nothing.
    if (_truthValue.get() == newTV.get()) return;

    // We need to guarantee that the signal goes out with the
    // correct truth value.  That is, another setter could be changing
    // this, even as we are.  So make a copy, first.
    TruthValuePtr oldTV(getTruthValue());

    // ... and we still need to make sure that only one thread is
    // writing this at a time. std:shared_ptr is NOT thread-safe against
    // multiple writers: see "Example 5" in
    // http://www.boost.org/doc/libs/1_53_0/libs/smart_ptr/shared_ptr.htm#ThreadSafety
    std::unique_lock<std::mutex> lck(_mtx);
    _truthValue = newTV;
    lck.unlock();

    if (_atom_space != nullptr) {
        TVCHSigl& tvch = _atom_space->_atom_table.TVChangedSignal();
        tvch(getHandle(), oldTV, newTV);
    }
}

TruthValuePtr Atom::getTruthValue() const
{
    // OK. The atomic thread-safety of shared-pointers is subtle. See
    // http://www.boost.org/doc/libs/1_53_0/libs/smart_ptr/shared_ptr.htm#ThreadSafety
    // and http://cppwisdom.quora.com/shared_ptr-is-almost-thread-safe
    // What it boils down to here is that we must *always* make a copy
    // of _truthValue before we use it, since it can go out of scope
    // because it can get set in another thread.  Viz, using it to
    // dereference can return a raw pointer to an object that has been
    // deconstructed.  The AtomSpaceAsyncUTest will hit this, as will
    // the multi-threaded async atom store in the SQL peristance backend.
    // Furthermore, we must make a copy while holding the lock! Got that?

    std::lock_guard<std::mutex> lck(_mtx);
    TruthValuePtr local(_truthValue);
    return local;
}

// ==============================================================
// Setting values associated with this atom.
void Atom::setValue(const Handle& key, const ProtoAtomPtr& value)
{
    if (nullptr == _atom_space) return;
    _atom_space->_value_table.addValuation(key, getHandle(), value);
}

ProtoAtomPtr Atom::getValue(const Handle& key) const
{
    if (nullptr == _atom_space) return nullptr;
    return _atom_space->_value_table.getValue(key, getHandle());
}

HandleSet Atom::getKeys() const
{
    if (nullptr == _atom_space) return HandleSet();
    return _atom_space->_value_table.getKeys(getHandle());
}

void Atom::copyValues(const Handle& other)
{
    HandleSet okeys(other->getKeys());
    for (const Handle& k: okeys)
    {
        ProtoAtomPtr p = other->getValue(k);
        setValue(k, p);
    }

    // Special case for truth values
    setTruthValue(other->getTruthValue());
}

std::string Atom::valuesToString() const
{
    std::string rv;

    HandleSet keys(getKeys());
    for (const Handle& k: keys)
    {
        ProtoAtomPtr p = getValue(k);
        rv += "; key = " + k->toString();
        rv += "; val = " + p->toString() + "\n";
    }
    return rv;
}

// ==============================================================
// Flag stuff
bool Atom::isMarkedForRemoval() const
{
    return (_flags & MARKED_FOR_REMOVAL) != 0;
}

void Atom::unsetRemovalFlag(void)
{
    _flags &= ~MARKED_FOR_REMOVAL;
}

void Atom::markForRemoval(void)
{
    _flags |= MARKED_FOR_REMOVAL;
}

bool Atom::isChecked() const
{
    return (_flags & CHECKED) != 0;
}

void Atom::setChecked(void)
{
    _flags |= CHECKED;
}

void Atom::setUnchecked(void)
{
    _flags &= ~CHECKED;
}

// ==============================================================

void Atom::setAtomSpace(AtomSpace *tb)
{
    if (tb == _atom_space) return;

    // Either the existing _atomSpace is null, and tb is not, i.e. this
    // atom is being inserted into tb, or _atomSpace is not null, while
    // tb is null, i.e. atom is being removed from _atomSpace.  It is
    // illegal to just switch membership: one or the other of these two
    // pointers must be null.
    OC_ASSERT (nullptr == _atom_space or tb == nullptr,
               "Atom table is not null!");
    _atom_space = tb;
}

AtomTable* Atom::getAtomTable() const
{
    return &(_atom_space->_atom_table);
}

// ==============================================================
// Incoming set stuff

/// Start tracking the incoming set for this atom.
/// An atom can't know what it's incoming set is, until this method
/// is called.  If this atom is added to any links before this call
/// is made, those links won't show up in the incoming set.
///
/// We don't automatically track incoming sets for two reasons:
/// 1) std::set takes up 48 bytes
/// 2) adding and remoiving uses up cpu cycles.
/// Thus, if the incoming set isn't needed, then don't bother
/// tracking it.
void Atom::keep_incoming_set()
{
    if (_incoming_set) return;
    _incoming_set = std::make_shared<InSet>();
}

/// Stop tracking the incoming set for this atom.
/// After this call, the incoming set for this atom can no longer
/// be queried; it is erased.
void Atom::drop_incoming_set()
{
    if (NULL == _incoming_set) return;
    std::lock_guard<std::mutex> lck (_mtx);
    _incoming_set->_iset.clear();
    _incoming_set = NULL;
}

/// Add an atom to the incoming set.
void Atom::insert_atom(const LinkPtr& a)
{
    if (NULL == _incoming_set) return;
    std::lock_guard<std::mutex> lck (_mtx);

    Type at = a->getType();
    auto bucket = _incoming_set->_iset.find(at);
    if (bucket == _incoming_set->_iset.end())
    {
        auto pr = _incoming_set->_iset.emplace(
                   std::make_pair(at, WincomingSet()));
        bucket = pr.first;
    }
    bucket->second.insert(a);

#ifdef INCOMING_SET_SIGNALS
    _incoming_set->_addAtomSignal(shared_from_this(), a);
#endif /* INCOMING_SET_SIGNALS */
}

/// Remove an atom from the incoming set.
void Atom::remove_atom(const LinkPtr& a)
{
    if (NULL == _incoming_set) return;
    std::lock_guard<std::mutex> lck (_mtx);
#ifdef INCOMING_SET_SIGNALS
    _incoming_set->_removeAtomSignal(shared_from_this(), a);
#endif /* INCOMING_SET_SIGNALS */
    Type at = a->getType();
    auto bucket = _incoming_set->_iset.find(at);
    bucket->second.erase(a);
}

/// Remove old, and add new, atomically, so that every user
/// will see either one or the other, but not both/neither in
/// the incoming set. This is used to manage the StateLink.
void Atom::swap_atom(const LinkPtr& old, const LinkPtr& neu)
{
    if (NULL == _incoming_set) return;
    std::lock_guard<std::mutex> lck (_mtx);

#ifdef INCOMING_SET_SIGNALS
    _incoming_set->_removeAtomSignal(shared_from_this(), old);
#endif /* INCOMING_SET_SIGNALS */
    Type ot = old->getType();
    auto bucket = _incoming_set->_iset.find(ot);
    bucket->second.erase(old);

    Type nt = neu->getType();
    bucket = _incoming_set->_iset.find(nt);
    if (bucket == _incoming_set->_iset.end())
    {
        auto pr = _incoming_set->_iset.emplace(
                   std::make_pair(nt, WincomingSet()));
        bucket = pr.first;
    }
    bucket->second.insert(neu);

#ifdef INCOMING_SET_SIGNALS
    _incoming_set->_addAtomSignal(shared_from_this(), neu);
#endif /* INCOMING_SET_SIGNALS */
}

size_t Atom::getIncomingSetSize() const
{
    if (NULL == _incoming_set) return 0;
    std::lock_guard<std::mutex> lck (_mtx);

    size_t cnt = 0;
    for (const auto pr : _incoming_set->_iset)
        cnt += pr.second.size();
    return cnt;
}

// We return a copy here, and not a reference, because the set itself
// is not thread-safe during reading while simultaneous insertion and
// deletion.  Besides, the incoming set is weak; we have to make it
// strong in order to hand it out.
IncomingSet Atom::getIncomingSet(AtomSpace* as) const
{
    static IncomingSet empty_set;
    if (NULL == _incoming_set) return empty_set;

    if (as) {
        const AtomTable *atab = &as->get_atomtable();
        // Prevent update of set while a copy is being made.
        std::lock_guard<std::mutex> lck (_mtx);
        IncomingSet iset;
        for (const auto bucket : _incoming_set->_iset)
        {
            for (const WinkPtr& w : bucket.second)
            {
                LinkPtr l(w.lock());
                if (l and atab->in_environ(l))
                    iset.emplace_back(l);
            }
        }
        return iset;
    }

    // Prevent update of set while a copy is being made.
    std::lock_guard<std::mutex> lck (_mtx);
    IncomingSet iset;
    for (const auto bucket : _incoming_set->_iset)
    {
        for (const WinkPtr& w : bucket.second)
        {
            LinkPtr l(w.lock());
            if (l) iset.emplace_back(l);
        }
    }
    return iset;
}

IncomingSet Atom::getIncomingSetByType(Type type) const
{
    IncomingSet result;

    // The code below is mostly a cut-n-paste from the header file.
    // The only difference is that it works with LinkPtr instead of
    // Handle.  The primary issue is that casting from Handle back
    // to LinkPtr is slowwwwwww.  So we avoid that, here.
    if (NULL == _incoming_set) return result;
    std::lock_guard<std::mutex> lck(_mtx);

    const auto bucket = _incoming_set->_iset.find(type);
    if (bucket == _incoming_set->_iset.cend()) return result;

    for (const WinkPtr& w : bucket->second)
    {
        LinkPtr h(w.lock());
        if (h) result.emplace_back(h);
    }
    return result;
}

std::string Atom::idToString() const
{
    return
        std::string("[") + std::to_string(get_hash()) + "]" +
        std::string("[") + std::to_string(_atom_space? _atom_space->_atom_table.get_uuid() : -1) + "]";
}

std::string oc_to_string(const IncomingSet& iset)
{
	std::stringstream ss;
	ss << "size = " << iset.size() << std::endl;
	for (unsigned i = 0; i < iset.size(); i++)
		ss << "link[" << i << "]:" << std::endl << iset[i]->toString();
	return ss.str();
}

} // ~namespace opencog
