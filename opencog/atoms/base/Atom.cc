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

#include <opencog/util/misc.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/platform.h>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/value/FloatValue.h>

#include <opencog/atomspace/AtomSpace.h>

//#define DPRINTF printf
#define DPRINTF(...)

#undef Type

namespace opencog {

Atom::~Atom()
{
    _atom_space = nullptr;

    // Disable for now. This assert has never tripped; there
    // seems to be no point to checking it.
#if 0
    // This can't ever possibly happen. If it does, then there is
    // some very sick bug with the reference counting that the
    // shared pointers are doing. (Or someone explcitly called the
    // destructor! Which they shouldn't do.)
    OC_ASSERT(0 == getIncomingSet().size(),
         "Atom deletion failure; incoming set not empty for %s h=%x",
         nameserver().getTypeName(_type).c_str(), get_hash());
#endif
    drop_incoming_set();
}

// ==============================================================

static const Handle& truth_key(void)
{
	static Handle tk(createNode(PREDICATE_NODE, "*-TruthValueKey-*"));
	return tk;
}

void Atom::setTruthValue(const TruthValuePtr& newTV)
{
    if (nullptr == newTV) return;

    // Another setter could be changing this, even as we are.
    // So make a copy, first.
    TruthValuePtr oldTV(getTruthValue());

    // If both old and new are e.g. DEFAULT_TV, then do nothing.
    if (oldTV.get() == newTV.get()) return;

    // ... and we still need to make sure that only one thread is
    // writing this at a time. std:shared_ptr is NOT thread-safe against
    // multiple writers: see "Example 5" in
    // http://www.boost.org/doc/libs/1_53_0/libs/smart_ptr/shared_ptr.htm#ThreadSafety
    setValue (truth_key(), ValueCast(newTV));
}

TruthValuePtr Atom::getTruthValue() const
{
    ValuePtr pap(getValue(truth_key()));
    if (nullptr == pap) return TruthValue::DEFAULT_TV();
    return TruthValueCast(pap);
}

TruthValuePtr Atom::incrementCountTV(double cnt)
{
	double mean = 1.0;
	double conf = 0.0;

	// Lock so that count updates are atomic!
	KVP_UNIQUE_LOCK;

	auto pr = _values.find(truth_key());
	if (_values.end() != pr)
	{
		const TruthValuePtr& tvp = TruthValueCast(pr->second);
		// tvp might be nullptr, if someone set the TV to something
		// that is not a truth value. This can happen if the truth
		// predicate is used directly with setValue().
		if (tvp)
		{
			if (COUNT_TRUTH_VALUE == tvp->get_type())
				cnt += tvp->get_count();
			mean = tvp->get_mean();
			conf = tvp->get_confidence();
		}
	}

	TruthValuePtr newTV = CountTruthValue::createTV(mean, conf, cnt);

	_values[truth_key()] = ValueCast(newTV);
	return newTV;
}

// ==============================================================
/// Setting values associated with this atom.
/// If the value is a null pointer, then the key is removed.
void Atom::setValue(const Handle& key, const ValuePtr& value)
{
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

		// Backwards compatibility: If we're incrementing the count
		// location on a SimpleTruthValue, then automatically promote
		// it to a CountTruthValue.
		if (*key == *truth_key() and
			 not pap->is_type(COUNT_TRUTH_VALUE))
		{
			FloatValuePtr fv(FloatValueCast(pap));
			std::vector<double> vect = fv->value();
			pap = ValueCast(TruthValue::factory(COUNT_TRUTH_VALUE, vect));
		}

		// Its a float. Let it increment itself.
		FloatValuePtr fv(FloatValueCast(pap));
		ValuePtr nv = fv->incrementCount(count);

		_values[key] = nv;
		return nv;
	}

	// If we are here, an existing value was not found.
	// Backwards compatibility: If there's no prior value *and*
	// the key is the default TruthValue key, then automatically
	// create a CountTruthValue.

	// Create a brand new float.
	ValuePtr nv;
	if (*truth_key() == *key)
		nv = ValueCast(TruthValue::factory(COUNT_TRUTH_VALUE, count));
	else
		nv = createFloatValue(FLOAT_VALUE, count);

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

		// Backwards compatibility: If we're incrementing the count
		// location on a SimpleTruthValue, then automatically promote
		// it to a CountTruthValue.
		if (2 == idx and *key == *truth_key() and
			 not pap->is_type(COUNT_TRUTH_VALUE))
		{
			FloatValuePtr fv(FloatValueCast(pap));
			std::vector<double> vect = fv->value();
			pap = ValueCast(TruthValue::factory(COUNT_TRUTH_VALUE, vect));
		}

		// Its a float. Let it increment itself.
		FloatValuePtr fv(FloatValueCast(pap));
		ValuePtr nv = fv->incrementCount(idx, count);

		_values[key] = nv;
		return nv;
	}

	// If we are here, an existing value was not found.
	// Backwards compatibility: If there's no prior value *and*
	// the key is the default TruthValue key, then automatically
	// create a CountTruthValue.

	// Create a brand new float.
	std::vector<double> new_vect;
	new_vect.resize(idx+1, 0.0);
	new_vect[idx] += count;

	ValuePtr nv;
	if (2 == idx and *truth_key() == *key)
		nv = ValueCast(TruthValue::factory(COUNT_TRUTH_VALUE, new_vect));
	else
		nv = createFloatValue(FLOAT_VALUE, new_vect);

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
    return _marked_for_removal.load();
}

bool Atom::unsetRemovalFlag(void)
{
    return _marked_for_removal.exchange(false);
}

bool Atom::markForRemoval(void)
{
    return _marked_for_removal.exchange(true);
}

bool Atom::isChecked() const
{
    return _checked.load();
}

bool Atom::setChecked(void)
{
    return _checked.exchange(true);
}

bool Atom::setUnchecked(void)
{
    return _checked.exchange(false);
}

bool Atom::isAbsent() const
{
    return _absent.load();
}

/// Marking an Atom as being absent makes it invisible, as if it
/// were deleted. We reclaim the "impossible-to-access" storage on
/// it, as well. (Is this really needed? I dunno. Seems like a good
/// idea.)
bool Atom::setAbsent(void)
{
    KVP_UNIQUE_LOCK;
    _values.clear();
    return _absent.exchange(true);
}

bool Atom::setPresent(void)
{
    return _absent.exchange(false);
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

/// Start tracking the incoming set for this atom.
/// An atom can't know what it's incoming set is, until this method
/// is called.  If this atom is added to any links before this call
/// is made, those links won't show up in the incoming set.
///
/// We don't automatically track incoming sets for two reasons:
/// 1) std::set takes up 48 bytes
/// 2) adding and removing uses up cpu cycles.
/// Thus, if the incoming set isn't needed, then don't bother
/// tracking it.
void Atom::keep_incoming_set()
{
    INCOMING_UNIQUE_LOCK;
    if (_incoming_set) return;
    _incoming_set = std::make_shared<InSet>();
}

/// Stop tracking the incoming set for this atom.
/// After this call, the incoming set for this atom can no longer
/// be queried; it is erased.
void Atom::drop_incoming_set()
{
    if (nullptr == _incoming_set) return;
    INCOMING_UNIQUE_LOCK;
    _incoming_set.reset();
}

/// Add an atom to the incoming set.
void Atom::insert_atom(const Handle& a)
{
    if (nullptr == _incoming_set) return;
    INCOMING_UNIQUE_LOCK;

    Type at = a->get_type();
    auto bucket = _incoming_set->_iset.find(at);
    if (bucket == _incoming_set->_iset.end())
    {
        auto pr = _incoming_set->_iset.emplace(
                   std::make_pair(at, WincomingSet()));
        bucket = pr.first;
    }
    bucket->second.insert(GET_PTR(a));
}

/// Remove an atom from the incoming set.
void Atom::remove_atom(const Handle& a)
{
    if (nullptr == _incoming_set) return;
    INCOMING_UNIQUE_LOCK;
    Type at = a->get_type();

    const auto bucket = _incoming_set->_iset.find(at);

    OC_ASSERT(bucket != _incoming_set->_iset.end(), "No bucket!");
    size_t erc = bucket->second.erase(GET_PTR(a));

    // std::set is a "true set", in that it either contains something,
    // or it does not.  Therefore, the erase count is either 1 (the
    // atom was found and erased) or 0 (the atom was not found, that's
    // because it was erased earlier, e.g. it had more than once in the
    // outgoing set. All other erase counts are ... unexpected.
    OC_ASSERT(2 > erc, "Unexpected erase count!");
}

/// Remove old, and add new, atomically, so that every user
/// will see either one or the other, but not both/neither in
/// the incoming set. This is used to manage the StateLink.
void Atom::swap_atom(const Handle& old, const Handle& neu)
{
    if (nullptr == _incoming_set) return;
    INCOMING_UNIQUE_LOCK;

    Type ot = old->get_type();
    auto bucket = _incoming_set->_iset.find(ot);
    bucket->second.erase(GET_PTR(old));

    Type nt = neu->get_type();
    bucket = _incoming_set->_iset.find(nt);
    if (bucket == _incoming_set->_iset.end())
    {
        auto pr = _incoming_set->_iset.emplace(
                   std::make_pair(nt, WincomingSet()));
        bucket = pr.first;
    }
    bucket->second.insert(GET_PTR(neu));
}

void Atom::install() {}
void Atom::remove() {}

bool Atom::isIncomingSetEmpty(const AtomSpace* as) const
{
    if (nullptr == _incoming_set) return true;

    INCOMING_SHARED_LOCK;

    for (const auto& bucket : _incoming_set->_iset)
    {
        for (const WinkPtr& w : bucket.second)
            WEAKLY_DO(l, w, { if (not as or as->in_environ(l) or nameserver().isA(_type, FRAME)) return false; })
    }
    return true;
}

size_t Atom::getIncomingSetSize(const AtomSpace* as) const
{
    if (nullptr == _incoming_set) return 0;

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
        for (const auto& bucket : _incoming_set->_iset)
        {
            for (const WinkPtr& w : bucket.second)
                WEAKLY_DO(l, w, { if (as->in_environ(l)) cnt++; })
        }
        return cnt;
    }

    size_t cnt = 0;
    INCOMING_SHARED_LOCK;
    for (const auto& pr : _incoming_set->_iset)
        cnt += pr.second.size();
    return cnt;
}

/// Add the incoming set for this Atom only to the HandleSet.
void Atom::getLocalInc(const AtomSpace* as, HandleSet& hs, Type t) const
{
    INCOMING_SHARED_LOCK;
    if (NOTYPE != t)
    {
        const auto bucket = _incoming_set->_iset.find(t);
        if (bucket == _incoming_set->_iset.cend()) return;
        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, {
                const Handle& local(as->lookupHandle(l));
                if (local) hs.insert(local);
            })
        return;
    }

    // If NOTYPE was given, then loop over all possibilities.
    for (const auto& bucket : _incoming_set->_iset)
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
    static IncomingSet empty_set;
    if (nullptr == _incoming_set) return empty_set;

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
        IncomingSet iset;
        for (const auto& bucket : _incoming_set->_iset)
        {
            for (const WinkPtr& w : bucket.second)
                WEAKLY_DO(l, w, { if (as->in_environ(l)) iset.emplace_back(l); })
        }
        return iset;
    }

    // Prevent update of set while a copy is being made.
    INCOMING_SHARED_LOCK;
    IncomingSet iset;
    for (const auto& bucket : _incoming_set->_iset)
    {
        for (const WinkPtr& w : bucket.second)
            WEAKLY_DO(l, w, { iset.emplace_back(l); });
    }
    return iset;
}

IncomingSet Atom::getIncomingSetByType(Type type, const AtomSpace* as) const
{
    static IncomingSet empty_set;
    if (nullptr == _incoming_set) return empty_set;

    if (as and not nameserver().isA(_type, FRAME))
    {
        // If the _copy_on_write flag is set, we need to
        // deduplicate the incoming set.
        if (as->get_copy_on_write())
        {
            HandleSet hs;
            getCoveredInc(as, hs, type);

				// Copy from set to vector.
            IncomingSet iset;
            for (const Handle& h: hs)
                iset.emplace_back(h);
            return iset;
        }

        // Lock to prevent updates of the set of atoms.
        INCOMING_SHARED_LOCK;
        const auto bucket = _incoming_set->_iset.find(type);
        if (bucket == _incoming_set->_iset.cend()) return empty_set;

        IncomingSet result;
        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, { if (as->in_environ(l)) result.emplace_back(l); })
        return result;
    }

    // Lock to prevent updates of the set of atoms.
    INCOMING_SHARED_LOCK;
    const auto bucket = _incoming_set->_iset.find(type);
    if (bucket == _incoming_set->_iset.cend()) return empty_set;

    IncomingSet result;
    for (const WinkPtr& w : bucket->second)
        WEAKLY_DO(l, w, { result.emplace_back(l); })
    return result;
}

size_t Atom::getIncomingSetSizeByType(Type type, const AtomSpace* as) const
{
    if (nullptr == _incoming_set) return 0;

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
        const auto bucket = _incoming_set->_iset.find(type);
        if (bucket == _incoming_set->_iset.cend()) return 0;

        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(l, w, { if (as->in_environ(l)) cnt++; })
        return cnt;
    }

    INCOMING_SHARED_LOCK;
    const auto bucket = _incoming_set->_iset.find(type);
    if (bucket == _incoming_set->_iset.cend()) return 0;

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

#if 0
std::string oc_to_string(const IncomingSet& iset, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << iset.size();
	for (unsigned i = 0; i < iset.size(); i++)
		ss << std::endl << indent << "link[" << i << "]:" << std::endl
		   << iset[i]->to_string(indent + OC_TO_STRING_INDENT);
	return ss.str();
}
#endif

std::string oc_to_string(const Atom& atom, const std::string& indent)
{
	return atom.to_string(indent);
}

} // ~namespace opencog
