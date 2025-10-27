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

#include <opencog/util/oc_assert.h>
#include <opencog/util/platform.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/FloatValue.h>

#include <opencog/atomspace/AtomSpace.h>

//#define DPRINTF printf
#define DPRINTF(...)

#undef Type

namespace opencog {

#if HAVE_SPARSEHASH
template <>
std::weak_ptr<Atom> hashable_weak_ptr<Atom>::_dummy = std::weak_ptr<Atom>();
#endif

#if USE_MUTEX_POOL
Atom::MutexPool Atom::_mutex_pool;
#endif

Atom::~Atom()
{
    _atom_space = nullptr;

    // Disable. This assert has never tripped;
    // there seems to be no point to checking it.
#if 0
    // This can't ever possibly happen. If it does, then there is
    // some very sick bug with the reference counting that the
    // shared pointers are doing. (Or someone explcitly called the
    // destructor! Which they shouldn't do.)
    OC_ASSERT(0 == getIncomingSet().size(),
         "Atom deletion failure; incoming set not empty for %s h=%x",
         nameserver().getTypeName(_type).c_str(), get_hash());
#endif
}

// ==============================================================
// Singleton key for backwards compat with TruthValues

const Handle& truth_key(void)
{
	static Handle tk(createNode(PREDICATE_NODE, "*-TruthValueKey-*"));
	return tk;
}

// ==============================================================
/// Setting values associated with this atom.
/// If the value is a null pointer, then the key is removed.
void Atom::setValue(const Handle& key, const ValuePtr& value)
{
	// We want to know if the key is .. being used as a key.
	key->markIsKey();

	// This is rather irritating, but we fake it for the
	// PredicateNode "*-TruthValueKey-*" because if we don't
	// then load-from-file and load-from-network breaks.
	if (key != truth_key() and *key == *truth_key())
	{
		KVP_UNIQUE_LOCK;
		if (nullptr != value)
			_values[truth_key()] = value;
		else
			_values.erase(truth_key());
	}
	else
	{
		KVP_UNIQUE_LOCK;
		if (nullptr != value)
			_values[key] = value;
		else
			_values.erase(key);
	}
}

ValuePtr Atom::getValue(const Handle& key) const
{
    // OK. The atomic thread-safety of shared-pointers is subtle. See
    // http://www.boost.org/doc/libs/1_53_0/libs/smart_ptr/shared_ptr.htm#ThreadSafety
    // and http://cppwisdom.quora.com/shared_ptr-is-almost-thread-safe
    // What it boils down to here is that we must *always* make a copy
    // of the value-pointer before we use it, since it can go out of scope
    // because it can get set in another thread.  Viz, using it to
    // dereference can return a raw pointer to an object that has been
    // deconstructed.  The AtomSpaceAsyncUTest will hit this, as will
    // the multi-threaded async atom store in the SQL peristance backend.
    // Furthermore, we must make a copy while holding the lock! Got that?

    // This is rather irritating, but we fake it for the
    // PredicateNode "*-TruthValueKey-*" because if we don't
    // then load-from-file and load-from-network breaks.
    if ((key != truth_key()) and (*key == *truth_key()))
    {
        KVP_SHARED_LOCK;
        auto pr = _values.find(truth_key());
        if (_values.end() != pr) return pr->second;
    }
    else
    {
        KVP_SHARED_LOCK;
        auto pr = _values.find(key);
        if (_values.end() != pr) return pr->second;
    }

    return ValuePtr();
}

ValuePtr Atom::incrementCount(const Handle& key, const std::vector<double>& count)
{
	KVP_UNIQUE_LOCK;

	// Find the existing value, if it is there.
	auto pr = _values.find(key);
	if (_values.end() != pr)
	{
		ValuePtr pap = pr->second;

		// Its not a float. Do nothing.
		if (not pap->is_type(FLOAT_VALUE))
			return pap;

		// Its a float. Let it increment itself.
		FloatValuePtr fv(FloatValueCast(pap));
		ValuePtr nv = fv->incrementCount(count);

		_values[key] = nv;
		return nv;
	}

	// If we are here, an existing value was not found.
	// Create a brand new float.
	ValuePtr nv = createFloatValue(count);

	_values[key] = nv;
	return nv;
}

// Cut-n-paste of the code above.
ValuePtr Atom::incrementCount(const Handle& key, size_t idx, double count)
{
	KVP_UNIQUE_LOCK;

	// Find the existing value, if it is there.
	auto pr = _values.find(key);
	if (_values.end() != pr)
	{
		ValuePtr pap = pr->second;

		// Its not a float. Do nothing.
		if (not pap->is_type(FLOAT_VALUE))
			return pap;

		// Its a float. Let it increment itself.
		FloatValuePtr fv(FloatValueCast(pap));
		ValuePtr nv = fv->incrementCount(idx, count);

		_values[key] = nv;
		return nv;
	}

	// If we are here, an existing value was not found.

	// Create a brand new float.
	std::vector<double> new_vect;
	new_vect.resize(idx+1, 0.0);
	new_vect[idx] += count;

	ValuePtr nv = createFloatValue(new_vect);

	_values[key] = nv;
	return nv;
}

HandleSet Atom::getKeys() const
{
    HandleSet keyset;
    KVP_SHARED_LOCK;
    for (const auto& pr : _values)
        keyset.insert(pr.first);

    return keyset;
}

void Atom::copyValues(const Handle& other)
{
    HandleSet okeys(other->getKeys());
    for (const Handle& k: okeys)
    {
        setValue(k, other->getValue(k));
    }
}

void Atom::clearValues(void)
{
    KVP_UNIQUE_LOCK;
    _values.clear();
}

/**
 * Return all of the Values on this Atom, formatted as a scheme
 * association-list. It must have the SRFI-1 a-list format,
 * as other code parses this, and expects it to be in this format.
 * The format is `((key . value) (key2 . value2) ...)`
 * It is expected that this method will execute relaitvely efficiently,
 * as it is frequently used, e.g. in the DHT backend.
 */
std::string Atom::valuesToString() const
{
    std::stringstream rv;

    rv << "(";
    for (const Handle& k: getKeys())
    {
        ValuePtr p = getValue(k);
        rv << "(" << k->to_short_string()
           << " . " << p->to_short_string() + ")";
    }
    rv << ")";
    return rv.str();
}

// ==============================================================
// Flag stuff
bool Atom::isMarkedForRemoval() const
{
    return _flags.load() & MARKED_FLAG;
}

bool Atom::unsetRemovalFlag(void)
{
    uint8_t old_flags = _flags.fetch_and(~MARKED_FLAG);
    return old_flags & MARKED_FLAG;
}

bool Atom::markForRemoval(void)
{
    uint8_t old_flags = _flags.fetch_or(MARKED_FLAG);
    return old_flags & MARKED_FLAG;
}

bool Atom::isChecked() const
{
    return _flags.load() & CHECKED_FLAG;
}

bool Atom::setChecked(void)
{
    uint8_t old_flags = _flags.fetch_or(CHECKED_FLAG);
    return old_flags & CHECKED_FLAG;
}

bool Atom::setUnchecked(void)
{
    uint8_t old_flags = _flags.fetch_and(~CHECKED_FLAG);
    return old_flags & CHECKED_FLAG;
}

bool Atom::isAbsent() const
{
    return _flags.load() & ABSENT_FLAG;
}

/// Marking an Atom as being absent makes it invisible, as if it
/// were deleted. We reclaim the "impossible-to-access" storage on
/// it, as well. (Is this really needed? I dunno. Seems like a good
/// idea.)
bool Atom::setAbsent(void)
{
    KVP_UNIQUE_LOCK;
    _values.clear();
    uint8_t old_flags = _flags.fetch_or(ABSENT_FLAG);
    return old_flags & ABSENT_FLAG;
}

bool Atom::setPresent(void)
{
    uint8_t old_flags = _flags.fetch_and(~ABSENT_FLAG);
    return old_flags & ABSENT_FLAG;
}

void Atom::markIsKey(void)
{
    _flags.fetch_or(IS_KEY_FLAG);
}

void Atom::markIsMessage(void)
{
    _flags.fetch_or(IS_MESSAGE_FLAG);
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

// ==============================================================
// Incoming set stuff

#if USE_BARE_BACKPOINTER
	#define GET_PTR(a) a.const_atom_ptr()
#else // USE_BARE_BACKPOINTER
	#define GET_PTR(a) a
#endif // USE_BARE_BACKPOINTER

#if USE_INCOME_INDEX
bool Atom::have_inset_map(void) const
{
    // During extraction, the atomspace will get set to nullptr.
    // Normally, this doesn't amtter, unless threads are racing.
    if (nullptr == _atom_space) return false;
    return _atom_space->have_inset_map(get_handle());
}
InSetMap& Atom::get_inset_map(void)
{
    return _atom_space->get_inset_map(get_handle());
}
const InSetMap& Atom::get_inset_map_const(void) const
{
    return _atom_space->get_inset_map(get_handle());
}
void Atom::drop_inset_map(void)
{
    OC_ASSERT( nullptr != _atom_space, "ooo noooo");
    _atom_space->drop_inset_map(get_handle());
}
#endif

/// Start tracking the incoming set for this atom.
/// An atom can't know what it's incoming set is, until this method
/// is called.  If this atom is added to any links before this call
/// is made, those links won't show up in the incoming set.
///
/// This is a minor, perhaps pointless(?) performance optimization:
/// adding and removing uses up cpu cycles.  If the incoming set isn't
/// needed, then don't bother tracking it.  That said: the only time
/// that the incoming set is not needed is when creating free-floating
/// Atoms, that have not (yet) been added to any AtomSpace. But such
/// floaters are transient; they are always quickly jammed into the
/// AtomSpace, which promptly calls `keep_incoming_set()`. So, for
/// all(?) practical purposes, we could just skip this. But ... well,
/// its here, its been here for over a decade, its fully debugged, so
/// whatever. It's not hurting us much.
void Atom::keep_incoming_set()
{
   _flags.fetch_or(USE_ISET_FLAG);
}

/// Stop tracking the incoming set for this atom.
/// After this call, the incoming set for this atom can no longer
/// be queried; it is erased.
void Atom::drop_incoming_set()
{
    if (not (_flags.load() & USE_ISET_FLAG)) return;
    INCOMING_UNIQUE_LOCK;
    _flags.fetch_and(~USE_ISET_FLAG);
    drop_inset_map();
    _atom_space = nullptr;
}

/// Add an atom to the incoming set.
void Atom::insert_atom(const Handle& a)
{
    if (not (_flags.load() & USE_ISET_FLAG)) return;
    INCOMING_UNIQUE_LOCK;

    Type at = a->get_type();
    InSetMap& iset = get_inset_map();
    auto bucket = iset.find(at);
    if (bucket == iset.end())
    {
        auto pr = iset.emplace(
                   std::make_pair(at, WincomingSet()));
        bucket = pr.first;
#if USE_SPARSE_INCOMING
        bucket->second.set_deleted_key(Handle());
#endif
    }
    bucket->second.insert(GET_PTR(a));
}

/// Remove an atom from the incoming set.
void Atom::remove_atom(const Handle& a)
{
    if (not (_flags.load() & USE_ISET_FLAG)) return;
    INCOMING_UNIQUE_LOCK;

    // Normally, there should be an incoming set, unless threads
    // are racing to extract Atoms. From what I can tell, the
    // extract code should be redesigned to avoid multiple accidental
    // extracts.
    if (not have_inset_map()) return;

    InSetMap& iset = get_inset_map();
    Type at = a->get_type();
    const auto bucket = iset.find(at);

    OC_ASSERT(bucket != iset.end(), "No bucket!");
    bucket->second.erase(GET_PTR(a));

    // Don't bother. Unit test takes this into account.
#if 0
    // This does a bit of eager garbage collection, when the incoming
    // set drops to zero size. There is really no reason to actually
    // do this, except that the UseCountUTest checks for this. The
    // empty incoming set still heelps a handle warm in the inset map.
    // This handle is counted in UseCountUTest and is flagged as an
    // error. The right answer is to just change the unit test.
    if (0 == bucket->second.size())
    {
        iset.erase(at);
        if (0 == iset.size())
            drop_inset_map();
    }
#endif

    // Don't bother. This never triggers.
#if 0
    size_t erc = bucket->second.erase(GET_PTR(a));
    // std::set is a "true set", in that it either contains something,
    // or it does not.  Therefore, the erase count is either 1 (the
    // atom was found and erased) or 0 (the atom was not found, that's
    // because it was erased earlier, e.g. it had been inserted more
    // than once into the outgoing set. All other erase counts are ...
    // unexpected.
    OC_ASSERT(2 > erc, "Unexpected erase count!");
#endif
}

/// Remove old, and add new, atomically, so that every user
/// will see either one or the other, but not both/neither in
/// the incoming set. This is used to manage the StateLink.
void Atom::swap_atom(const Handle& old, const Handle& neu)
{
    if (not (_flags.load() & USE_ISET_FLAG)) return;
    INCOMING_UNIQUE_LOCK;

    Type ot = old->get_type();
    InSetMap& iset = get_inset_map();
    auto bucket = iset.find(ot);
    bucket->second.erase(GET_PTR(old));

    Type nt = neu->get_type();
    bucket = iset.find(nt);
    if (bucket == iset.end())
    {
        auto pr = iset.emplace(
                   std::make_pair(nt, WincomingSet()));
        bucket = pr.first;
    }
    bucket->second.insert(GET_PTR(neu));
}

// Virtual. Derived classes want to know about incoming set add/remove.
void Atom::install() {}
void Atom::remove() {}

bool Atom::isIncomingSetEmpty(const AtomSpace* as) const
{
    if (not (_flags.load() & USE_ISET_FLAG)) return true;
    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return false;

    const InSetMap& iset = get_inset_map_const();
    for (const auto& bucket : iset)
    {
        for (const WinkPtr& w : bucket.second)
            WEAKLY_DO(l, w, { if (not as or as->in_environ(l) or nameserver().isA(_type, FRAME)) return false; })
    }
    return true;
}

size_t Atom::getIncomingSetSize(const AtomSpace* as) const
{
    if (not (_flags.load() & USE_ISET_FLAG)) return 0;

    if (as and not nameserver().isA(_type, FRAME))
    {
        // If the _copy_on_write flag is set, we need to
        // deduplicate the incoming set.
        if (as->get_copy_on_write())
        {
            HandleSet hs;
            getCoveredInc(as, hs, NOTYPE);
            return hs.size();
        }

        size_t cnt = 0;
        INCOMING_SHARED_LOCK;
        if (not have_inset_map()) return 0;
        const InSetMap& iset = get_inset_map_const();
        for (const auto& bucket : iset)
        {
            for (const WinkPtr& w : bucket.second)
                WEAKLY_DO(l, w, { if (as->in_environ(l)) cnt++; })
        }
        return cnt;
    }

    size_t cnt = 0;
    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return 0;
    const InSetMap& iset = get_inset_map_const();
    for (const auto& pr : iset)
        cnt += pr.second.size();
    return cnt;
}

/// Add the incoming set for this Atom only to the HandleSet.
void Atom::getLocalInc(const AtomSpace* as, HandleSet& hs, Type t) const
{
    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return;

    if (NOTYPE != t)
    {
        const InSetMap& iset = get_inset_map_const();
        const auto bucket = iset.find(t);
        if (bucket == iset.cend()) return;
        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, {
                const Handle& local(as->lookupHandle(l));
                if (local) hs.insert(local);
            })
        return;
    }

    // If NOTYPE was given, then loop over all possibilities.
    const InSetMap& iset = get_inset_map_const();
    for (const auto& bucket : iset)
    {
        for (const WinkPtr& w : bucket.second)
            WEAKLY_DO(l, w, {
                const Handle& local(as->lookupHandle(l));
                if (local) hs.insert(local);
            })
    }
}

/// Find all copies of this atom in deeper AtomSpaces, and add the
/// incoming sets of those copies into the HandleSet. The search
/// stops when a hiding Atom is encountered; i.e. it will not go
/// below a hidden Atom.
void Atom::getCoveredInc(const AtomSpace* as, HandleSet& hs, Type t) const
{
    getLocalInc(as, hs, t);
    AtomSpace* eva = _atom_space;

    // If we are a FRAME, then it is possible that the owning
    // _atom_space is not set. Avoid a null-pointer deref. See
    // https://github.com/opencog/atomspace-rocks/issues/20
    if (nullptr == eva) return;

    while (true)
    {
        size_t asz = eva->size();
        if (0 == asz) return;
        if (1 < asz) break;
        AtomSpacePtr alo = eva->getEnviron()[0];
        eva = (AtomSpace*) alo.get();
        Handle hat = eva->lookupHandle(get_handle());
        if (nullptr == hat) return;
        hat->getLocalInc(as, hs, t);
        eva = hat->getAtomSpace();
    }

    for (const AtomSpacePtr& has : eva->getEnviron())
        has->getCoveredInc(as, hs, t);
}

// We return a copy here, and not a reference, because the set itself
// is not thread-safe during reading while simultaneous insertion and
// deletion.  Besides, the incoming set is weak; we have to make it
// strong in order to hand it out.
IncomingSet Atom::getIncomingSet(const AtomSpace* as) const
{
    static const IncomingSet empty_set;
    if (not (_flags.load() & USE_ISET_FLAG)) return empty_set;

    if (as and not nameserver().isA(_type, FRAME))
    {
        // If the _copy_on_write flag is set, we need to
        // deduplicate the incoming set.
        if (as->get_copy_on_write())
        {
            // We use a set here, in order to perform
            // deduplication. Lookups of multiple copies
            // may result in duplicates in the incoming set.
            HandleSet hs;
            getCoveredInc(as, hs, NOTYPE);

				// Copy from set to vector.
            IncomingSet iset;
            for (const Handle& h: hs)
                iset.emplace_back(h);
            return iset;
        }

        // Prevent update of set while a copy is being made.
        INCOMING_SHARED_LOCK;
        if (not have_inset_map()) return empty_set;
        IncomingSet retset;
        const InSetMap& iset = get_inset_map_const();
        for (const auto& bucket : iset)
        {
            for (const WinkPtr& w : bucket.second)
                WEAKLY_DO(l, w, { if (as->in_environ(l)) retset.emplace_back(l); })
        }
        return retset;
    }

    // Prevent update of set while a copy is being made.
    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return empty_set;
    IncomingSet retset;
    const InSetMap& iset = get_inset_map_const();
    for (const auto& bucket : iset)
    {
        for (const WinkPtr& w : bucket.second)
            WEAKLY_DO(l, w, { retset.emplace_back(l); });
    }
    return retset;
}

IncomingSet Atom::getIncomingSetByType(Type type, const AtomSpace* as) const
{
    static const IncomingSet empty_set;
    if (not (_flags.load() & USE_ISET_FLAG)) return empty_set;

    if (as and not nameserver().isA(_type, FRAME))
    {
        // If the _copy_on_write flag is set, we need to
        // deduplicate the incoming set.
        if (as->get_copy_on_write())
        {
            HandleSet hs;
            getCoveredInc(as, hs, type);

				// Copy from set to vector.
            IncomingSet retset;
            for (const Handle& h: hs)
                retset.emplace_back(h);
            return retset;
        }

        // Lock to prevent updates of the set of atoms.
        INCOMING_SHARED_LOCK;
        if (not have_inset_map()) return empty_set;
        const InSetMap& iset = get_inset_map_const();
        const auto bucket = iset.find(type);
        if (bucket == iset.cend()) return empty_set;

        IncomingSet result;
        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, { if (as->in_environ(l)) result.emplace_back(l); })
        return result;
    }

    // Lock to prevent updates of the set of atoms.
    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return empty_set;
    const InSetMap& iset = get_inset_map_const();
    const auto bucket = iset.find(type);
    if (bucket == iset.cend()) return empty_set;

    IncomingSet result;
    for (const WinkPtr& w : bucket->second)
        WEAKLY_DO(l, w, { result.emplace_back(l); })
    return result;
}

size_t Atom::getIncomingSetSizeByType(Type type, const AtomSpace* as) const
{
    if (not (_flags.load() & USE_ISET_FLAG)) return 0;

    size_t cnt = 0;

    if (as and not nameserver().isA(_type, FRAME))
    {
        // If the _copy_on_write flag is set, we need to
        // deduplicate the incoming set.
        if (as->get_copy_on_write())
        {
            HandleSet hs;
            getCoveredInc(as, hs, type);
            return hs.size();
        }

        INCOMING_SHARED_LOCK;
        if (not have_inset_map()) return 0;
        const InSetMap& iset = get_inset_map_const();
        const auto bucket = iset.find(type);
        if (bucket == iset.cend()) return 0;

        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, { if (as->in_environ(l)) cnt++; })
        return cnt;
    }

    INCOMING_SHARED_LOCK;
    if (not have_inset_map()) return 0;
    const InSetMap& iset = get_inset_map_const();
    const auto bucket = iset.find(type);
    if (bucket == iset.cend()) return 0;

    for (const WinkPtr& w : bucket->second)
        WEAKLY_DO(l, w, { cnt++; })
    return cnt;
}

std::string Atom::id_to_string() const
{
    std::stringstream ss;
    ss << "[" << std::hex << get_hash() << "][";
    if (_atom_space) ss << _atom_space->get_uuid();
    else ss << "-1";
    ss << "]";
    if (isAbsent()) ss << "[absent]";
    if (isMarkedForRemoval()) ss << " !!! ERROR: marked for removal!";
    return ss.str();
}

std::string oc_to_string(const Atom& atom, const std::string& indent)
{
	return atom.to_string(indent);
}

} // ~namespace opencog
