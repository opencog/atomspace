/*
 * opencog/atomspace/AttentionBank.cc
 *
 * Copyright (C) 2013 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Joel Pitt
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

#include <boost/bind.hpp>

#include <opencog/atoms/base/Handle.h>
#include "AttentionBank.h"
#include "AtomTable.h"

#include <opencog/util/Config.h>

using namespace opencog;

AttentionBank::AttentionBank(AtomTable& atab, bool transient)
{
    /* Do not boether with initialization, if this is transient */
    if (transient) { _zombie = true; return; }
    _zombie = false;

    startingFundsSTI = fundsSTI = config().get_int("STARTING_STI_FUNDS", 100000);
    startingFundsLTI = fundsLTI = config().get_int("STARTING_LTI_FUNDS", 100000);
    stiFundsBuffer = config().get_int("STI_FUNDS_BUFFER", 10000);
    ltiFundsBuffer = config().get_int("LTI_FUNDS_BUFFER", 10000);
    targetLTI = config().get_int("TARGET_LTI_FUNDS", 10000);
    targetSTI = config().get_int("TARGET_STI_FUNDS", 10000);
    STIAtomWage = config().get_int("ECAN_STARTING_ATOM_STI_WAGE", 10);
    LTIAtomWage = config().get_int("ECAN_STARTING_ATOM_LTI_WAGE", 10);

    attentionalFocusBoundary = 1;

    AVChangedConnection =
        atab.AVChangedSignal().connect(
            boost::bind(&AttentionBank::AVChanged, this, _1, _2, _3));
}

/// This must be called before the AtomTable is destroyed. Which
/// means that it cannot be in the destructor (since the AtomTable
/// is probably gone by then, leading to a crash.  XXX FIXME yes this
/// is a tacky hack to fix a design bug.
void AttentionBank::shutdown(void)
{
    if (_zombie) return;  /* no-op, if a zombie */
    AVChangedConnection.disconnect();
}

AttentionBank::~AttentionBank() {}


void AttentionBank::AVChanged(Handle h, AttentionValuePtr old_av,
                                        AttentionValuePtr new_av)
{
    // Add the old attention values to the AtomSpace funds and
    // subtract the new attention values from the AtomSpace funds
    updateSTIFunds(old_av->getSTI() - new_av->getSTI());
    updateLTIFunds(old_av->getLTI() - new_av->getLTI());

    logger().fine("AVChanged: fundsSTI = %d, old_av: %d, new_av: %d",
                   fundsSTI, old_av->getSTI(), new_av->getSTI());

    // Check if the atom crossed into or out of the AttentionalFocus
    // and notify any interested parties
    if (old_av->getSTI() < attentionalFocusBoundary and
        new_av->getSTI() >= attentionalFocusBoundary)
    {
        AFCHSigl& afch = AddAFSignal();
        afch(h, old_av, new_av);
    }
    else if (new_av->getSTI() < attentionalFocusBoundary and
             old_av->getSTI() >= attentionalFocusBoundary)
    {
        AFCHSigl& afch = RemoveAFSignal();
        afch(h, old_av, new_av);
    }
}

void AttentionBank::stimulate(Handle& h, double stimulus)
{
    int sti = h->getAttentionValue()->getSTI();
    int lti = h->getAttentionValue()->getLTI();
    int stiWage = calculateSTIWage() * stimulus;
    int ltiWage = calculateLTIWage() * stimulus;

    h->setSTI(sti + stiWage);
    h->setLTI(lti + ltiWage);
}

void AttentionBank::updateMaxSTI(AttentionValue::sti_t m)
{
    std::lock_guard<std::mutex> lock(_lock_maxSTI);
    _maxSTI.update(m);
}

void AttentionBank::updateMinSTI(AttentionValue::sti_t m)
{
    std::lock_guard<std::mutex> lock(_lock_minSTI);
    _minSTI.update(m);
}

AttentionValue::sti_t AttentionBank::getMaxSTI(bool average) const
{
    std::lock_guard<std::mutex> lock(_lock_maxSTI);
    if (average) {
        return (AttentionValue::sti_t) _maxSTI.recent;
    } else {
        return _maxSTI.val;
    }
}

AttentionValue::sti_t AttentionBank::getMinSTI(bool average) const
{
    std::lock_guard<std::mutex> lock(_lock_minSTI);
    if (average) {
        return (AttentionValue::sti_t) _minSTI.recent;
    } else {
        return _minSTI.val;
    }
}

AttentionValue::sti_t AttentionBank::calculateSTIWage()
{
    long funds = getSTIFunds();
    double diff  = funds - targetSTI;
    double ndiff = diff / stiFundsBuffer;
    ndiff = std::min(ndiff, 1.0);
    ndiff = std::max(ndiff, -1.0);

    return STIAtomWage + (STIAtomWage * ndiff);
}

AttentionValue::lti_t AttentionBank::calculateLTIWage()
{
    long funds = getLTIFunds();
    double diff  = funds - targetLTI;
    double ndiff = diff / ltiFundsBuffer;
    ndiff = std::min(ndiff, 1.0);
    ndiff = std::max(ndiff, -1.0);

    return LTIAtomWage + (LTIAtomWage * ndiff);
}

AttentionValue::sti_t AttentionBank::getAttentionalFocusBoundary() const
{
    return attentionalFocusBoundary;
}

AttentionValue::sti_t AttentionBank::setAttentionalFocusBoundary(AttentionValue::sti_t boundary)
{
    attentionalFocusBoundary = boundary;
    return boundary;
}

double AttentionBank::getNormalisedSTI(AttentionValuePtr av,
                                   bool average, bool clip) const
{
    double val;
    // get normalizer (maxSTI - attention boundary)
    AttentionValue::sti_t s = av->getSTI();
    if (s > getAttentionalFocusBoundary()) {
        int normaliser = (int) getMaxSTI(average) - getAttentionalFocusBoundary();
        if (normaliser == 0) return 0.0;
        val = (s - getAttentionalFocusBoundary()) / (double) normaliser;
    } else {
        int normaliser = -((int) getMinSTI(average) + getAttentionalFocusBoundary());
        if (normaliser == 0) return 0.0;
        val = (s + getAttentionalFocusBoundary()) / (double) normaliser;
    }

    if (clip) return std::max(-1.0, std::min(val, 1.0));
    return val;
}

double AttentionBank::getNormalisedSTI(AttentionValuePtr av) const
{
    AttentionValue::sti_t s = av->getSTI();
    auto normaliser =
            s > getAttentionalFocusBoundary() ? getMaxSTI() : getMinSTI();

    return (s / normaliser);
}

double AttentionBank::getNormalisedZeroToOneSTI(AttentionValuePtr av,
                                    bool average, bool clip) const
{
    AttentionValue::sti_t s = av->getSTI();
    int normaliser = getMaxSTI(average) - getMinSTI(average);
    if (normaliser == 0) return 0.0;

    double val = (s - getMinSTI(average)) / (double) normaliser;
    if (clip) return std::max(0.0, std::min(val, 1.0));
    return val;
}
