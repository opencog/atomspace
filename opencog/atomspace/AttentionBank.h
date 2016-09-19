/*
 * opencog/atomspace/AttentionBank.h
 *
 * Copyright (C) 2011 OpenCog Foundation
 * Copyright (C) 2016 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Joel Pitt <joel@opencog.org>
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

#ifndef _OPENCOG_ATTENTION_BANK_H
#define _OPENCOG_ATTENTION_BANK_H

#include <atomic>
#include <mutex>

#include <boost/signals2.hpp>

#include <opencog/util/async_method_caller.h>
#include <opencog/util/recent_val.h>

#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/atomspace/ImportanceIndex.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/* Attention Value changed */
typedef boost::signals2::signal<void (const Handle&,
                                      const AttentionValuePtr&,
                                      const AttentionValuePtr&)> AVCHSigl;

/* Attentional Focus changed */
typedef boost::signals2::signal<void (const Handle&,
                                      const AttentionValuePtr&,
                                      const AttentionValuePtr&)> AFCHSigl;

class AtomSpace;

class AttentionBank
{
    AtomSpace* _as;
    /**
     * If true, then this AttentionBank is not being used.
     * Yes, this is totally bogus, but is needed, due to design
     * flaws related to attention allocation.
     */
    bool _zombie;
    

    /** The connection by which we are notified of AV changes */
    boost::signals2::connection _AVChangedConnection;
    void AVChanged(const Handle&, const AttentionValuePtr&, const AttentionValuePtr&);

    boost::signals2::connection _addAtomConnection;
    boost::signals2::connection _removeAtomConnection;

    /**
     * Boundary at which an atom is considered within the attentional
     * focus of opencog. Atom's with STI less than this value are
     * not charged STI rent.
     */
    AttentionValue::sti_t _attentionalFocusBoundary;

    /**
     * Signal emitted when an atom crosses in or out of the
     * AttentionalFocus.
     */
    AFCHSigl _AddAFSignal;
    AFCHSigl _RemoveAFSignal;

    /**
     * Running average min and max STI, together with locks to pretect updates.
     */
    opencog::recent_val<AttentionValue::sti_t> _maxSTI;
    opencog::recent_val<AttentionValue::sti_t> _minSTI;

    mutable std::mutex _lock_maxSTI;
    mutable std::mutex _lock_minSTI;

    /**
     * The amount importance funds available in the AttentionBank.
     * Atomic, so that updates don't need a lock.
     */
    std::atomic_long fundsSTI;
    std::atomic_long fundsLTI;

    long startingFundsSTI;
    long startingFundsLTI;

    AttentionValue::sti_t stiFundsBuffer;
    AttentionValue::lti_t ltiFundsBuffer;

    AttentionValue::sti_t targetSTI;
    AttentionValue::lti_t targetLTI;

    AttentionValue::sti_t STIAtomWage;
    AttentionValue::lti_t LTIAtomWage;

    /** The importance index, and it's lock. */
    mutable std::mutex _lock_index;
    ImportanceIndex _importanceIndex;

    async_caller<AttentionBank,Handle> _index_insert_queue;
    async_caller<AttentionBank,AtomPtr> _index_remove_queue;

    /** Signal emitted when the AV changes. */
    AVCHSigl _AVChangedSignal;

public:
    /** The table notifies us about AV changes */
    AttentionBank(AtomSpace*, bool);
    ~AttentionBank();
    void shutdown(void);

    /**
     * Provide ability for others to find out about atoms that cross in or
     * out of the AttentionalFocus
     */
    AFCHSigl& AddAFSignal() { return _AddAFSignal; }
    AFCHSigl& RemoveAFSignal() { return _RemoveAFSignal; }

    /** Provide ability for others to find out about AV changes */
    AVCHSigl& getAVChangedSignal() { return _AVChangedSignal; }

    /**
     * Stimulate an atom.
     *
     * @warning Should only be used by attention allocation system.
     * @param  h Handle to be stimulated
     * @param stimulus stimulus amount
     */
    void stimulate(Handle&, double stimulus);

    /**
     * Get the total amount of STI in the AtomSpace, sum of
     * STI across all atoms.
     *
     * @return total STI in AtomSpace
     */
    long getTotalSTI() const {
        return startingFundsSTI - fundsSTI;
    }

    /**
     * Get the total amount of LTI in the AtomSpace, sum of
     * all LTI across atoms.
     *
     * @return total LTI in AtomSpace
     */
    long getTotalLTI() const {
        return startingFundsLTI - fundsLTI;
    }

    /**
     * Get the STI funds available in the AtomSpace pool.
     *
     * @return STI funds available
     */
    long getSTIFunds() const { return fundsSTI; }

    /**
     * Get the LTI funds available in the AtomSpace pool.
     *
     * @return LTI funds available
     */
    long getLTIFunds() const { return fundsLTI; }

    long updateSTIFunds(AttentionValue::sti_t diff) {
        return fundsSTI += diff;
    }

    long updateLTIFunds(AttentionValue::lti_t diff) {
        return fundsLTI += diff;
    }

    /**
     * Get attentional focus boundary, generally atoms below
     * this threshold won't be accessed unless search methods
     * are unsuccessful on those that are above this value.
     *
     * @return Short Term Importance threshold value
     */
    AttentionValue::sti_t getAttentionalFocusBoundary() const {
        return _attentionalFocusBoundary;
    }

    /**
     * Change the attentional focus boundary. Some situations
     * may benefit from less focussed searches.
     *
     * @param s New threshold
     * @return Short Term Importance threshold value
     */
    AttentionValue::sti_t setAttentionalFocusBoundary(
        AttentionValue::sti_t s)
    {
        _attentionalFocusBoundary = s;
        return s;
    }

    /**
     * Get the maximum STI observed in the AtomSpace.
     *
     * @param average If true, return an exponentially decaying
     * average of maximum STI, otherwise return the actual maximum.
     * @return Maximum STI
     */
    AttentionValue::sti_t getMaxSTI(bool average=true) const;

    /**
     * Get the minimum STI observed in the AtomSpace.
     *
     * @param average If true, return an exponentially decaying
     * average of minimum STI, otherwise return the actual maximum.
     * @return Minimum STI
     */
    AttentionValue::sti_t getMinSTI(bool average=true) const;

    AttentionValue::sti_t calculateSTIWage(void);

    AttentionValue::sti_t calculateLTIWage(void);

    /**
     * Update the minimum STI observed in the connected AtomSpace.
     * Min/max are not updated on setSTI because average is calculate
     * by lobe cycle, although this could potentially also be handled
     * by the cogServer.
     *
     * @warning Should only be used by attention allocation system.
     * @param m New minimum STI
     */
    void updateMinSTI(AttentionValue::sti_t m);

    /**
     * Update the maximum STI observed in the connected AtomSpace.
     * Min/max are not updated on setSTI because average is calculate
     * by lobe cycle, although this could potentially also be handled
     * by the cogServer.
     *
     * @warning Should only be used by attention allocation system.
     * @param m New maximum STI
     */
    void updateMaxSTI(AttentionValue::sti_t m);

    /** Change the Very-Long-Term Importance of an attention value holder */
    //void setVLTI(AttentionValueHolderPtr avh, AttentionValue::vlti_t);

    /**
     * Retrieve the doubly normalised Short-Term Importance between -1..1
     * for a given AttentionValue. STI above and below threshold
     * normalised separately and linearly.
     *
     * @param h The attention value holder to get STI for
     * @param average Should the recent average max/min STI be used,
     *        or the exact min/max?
     * @param clip Should the returned value be clipped to -1..1?
     *        Outside this range can be return if average=true
     * @return normalised STI between -1..1
     */
    double getNormalisedSTI(AttentionValuePtr, bool average, bool clip) const;

    /**
     * @see getNormalisedSTI()
     */
    double getNormalisedSTI(AttentionValuePtr) const;

    /**
     * Retrieve the linearly normalised Short-Term Importance between 0..1
     * for a given AttentionValue.
     *
     * @param h The attention value holder to get STI for
     * @param average Should the recent average max/min STI be used,
     *        or the exact min/max?
     * @param clip Should the returned value be clipped to 0..1?
     *        Outside this range can be return if average=true
     * @return normalised STI between 0..1
     */
    double getNormalisedZeroToOneSTI(AttentionValuePtr, bool average, bool clip) const;

    /**
     * Returns the set of atoms within the given importance range.
     *
     * @param Importance range lower bound (inclusive).
     * @param Importance range upper bound (inclusive).
     * @return The set of atoms within the given importance range.
     */
    UnorderedHandleSet getHandlesByAV(AttentionValue::sti_t lowerBound,
                  AttentionValue::sti_t upperBound = AttentionValue::MAXSTI) const
    {
        std::lock_guard<std::mutex> lck(_lock_index);
        return _importanceIndex.getHandleSet(lowerBound, upperBound);
    }

    /**
     * Updates the importance index for the given atom. According to the
     * new importance of the atom, it may change importance bins.
     *
     * @param The atom whose importance index will be updated.
     * @param The old importance bin where the atom originally was.
     */
    void updateImportanceIndex(AtomPtr a, int bin)
    {
        std::lock_guard<std::mutex> lck(_lock_index);
        _importanceIndex.updateImportance(a.operator->(), bin);
    }

    void add_atom_to_indexInsertQueue(const Handle& h)
    {
        _index_insert_queue.enqueue(h);
    }

    void add_atom_to_indexRemoveQueue(const AtomPtr& atom)
    {
        _index_remove_queue.enqueue(atom);
    }

    void put_atom_into_index(const Handle& h)
    {
        std::lock_guard<std::mutex> lck(_lock_index);
        _importanceIndex.insertAtom(h.operator->());
    }

    void remove_atom_from_index(const AtomPtr& atom)
    {
        std::lock_guard<std::mutex> lck(_lock_index);
        _importanceIndex.removeAtom(atom.operator->());
    }

};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_ATTENTION_BANK_H
