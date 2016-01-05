/*
 * opencog/atomspace/IndefiniteTruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Fabricio Silva <fabricio@vettalabs.com>
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

#include <math.h>
#include <opencog/util/exceptions.h>

#include "IndefiniteTruthValue.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

confidence_t IndefiniteTruthValue::DEFAULT_CONFIDENCE_LEVEL = 0.9;
strength_t IndefiniteTruthValue::diffError = 0.001;
strength_t IndefiniteTruthValue::s = 0.5;


// Formula defined in the integral of one step (x-L1)^ks * (U1-x)^k(1-s)
static double integralFormula (double x, void * params)
{
    double L_, U_, k_, s_;
    double *in_params = static_cast<double*>(params);
    L_ = in_params[0];
    U_ = in_params[1];
    k_ = in_params[2];
    s_ = in_params[3];
    double f = (pow((x - L_), (k_ * s_))) * pow((U_ - x), (k_ * (1 - s_)));
    return f;
}

static strength_t DensityIntegral(strength_t lower, strength_t upper,
                                  strength_t L_, strength_t U_,
                                  count_t k_, strength_t s_)
{
    double params[4];
    params[0] = static_cast<double>(L_);
    params[1] = static_cast<double>(U_);
    params[2] = static_cast<double>(k_);
    params[3] = static_cast<double>(s_);

    // Previous code computed this integral to an absolute accuracy
    // of 0.1 which seems absurd to me. Improve on this just a bit.
#define NSTEPS 15.0
    double delta = (upper-lower) / NSTEPS;
    double result = 0.0;
    for (double x=lower; x<upper; x += delta) {
        result += integralFormula(x, &params);
    }
    result *= delta;
    return (strength_t) result;
}

void IndefiniteTruthValue::init(strength_t l, strength_t u, confidence_t c)
{
    L = l;
    U = u;
    confidenceLevel = c;

    firstOrderDistribution.clear();
    symmetric = true;

    mean = (L + U) / 2;

    strength_t W = U-L;
    // to avoid division by zero
    W = std::max(W, static_cast<strength_t>(0.0000001));
    // This is a bad heuristic that comes from c = N / (N+k). By
    // assuming c is an estimate of 1 - W we end up with this
    // formula. The problem is that c is definitely not a good
    // estimate for 1 - W.
    count = (DEFAULT_K * (1 - W) / W);

    confidence = count / (count + DEFAULT_K);

    diff = 0.0; // Not sure returning 0 is right
#ifdef HANGS_ININFINITE_LOOP
    if (U != L) {
        strength_t idiff = 0.01; // Initial diff suggestion
        diff = findDiff(idiff);
    }
#endif
}

void IndefiniteTruthValue::copy(const IndefiniteTruthValue& source)
{
    L = source.L;
    U = source.U;
    confidenceLevel = source.confidenceLevel;
    diff = source.diff;
    mean = source.mean;
    count = source.count;
    confidence = source.confidence;
    symmetric = source.symmetric;
}

IndefiniteTruthValue::IndefiniteTruthValue()
{
    init();
}

IndefiniteTruthValue::IndefiniteTruthValue(strength_t l, strength_t u,
                                           confidence_t c)
{
    init(l, u, c);
}

IndefiniteTruthValue::IndefiniteTruthValue(IndefiniteTruthValue const& source)
{
    copy(source);
}

bool IndefiniteTruthValue::operator==(const TruthValue& rhs) const
{
    const IndefiniteTruthValue* itv = dynamic_cast<const IndefiniteTruthValue*>(&rhs);
    if (NULL == itv) {
        return false;
    } else {
        return  (U == itv->U && L == itv->L 
                 && confidenceLevel == itv->confidenceLevel);
    }
}

strength_t IndefiniteTruthValue::findDiff(strength_t idiff) const
{
    strength_t min = 0.0;
    strength_t max = 0.5; //diff cannot be larger than 1/2 cause symmetric case
    strength_t L1, U1;
    strength_t numerator, denominator, result;
    strength_t expected = (1.0 - confidenceLevel) / 2.0;
    bool lte, gte; //smaller than expected, greater than expected

    //loop until convergence
    do {
        U1 = U + idiff;
        L1 = L - idiff;

        numerator = DensityIntegral(U, U1, L1, U1, DEFAULT_K, s);
        denominator = DensityIntegral(L1, U1, L1, U1, DEFAULT_K, s);

        if (denominator > 0.0) result = numerator / denominator;
        else result = 0.0;

        lte = result < expected - diffError;
        gte = result > expected + diffError;

        if (lte) {
            min = idiff;
            idiff = (idiff + max) / 2.0;
        }
        if (gte) {
            max = idiff;
            idiff = (min + idiff) / 2.0;
        }
    } while (lte or gte);

    return idiff;
}

const std::vector<strength_t*>& IndefiniteTruthValue::getFirstOrderDistribution() const
{
    return firstOrderDistribution;
}

TruthValueType IndefiniteTruthValue::getType() const
{
    return INDEFINITE_TRUTH_VALUE;
}

// Merge formula, as specified by PLN.
TruthValuePtr IndefiniteTruthValue::merge(TruthValuePtr other,
                                          const MergeCtrl& mc) const
{
    return higher_confidence_merge(other);
}

std::string IndefiniteTruthValue::toString() const
{
    char buf[1024];
    sprintf(buf, "[%f,%f,%f,%f,%f,%d]",
            static_cast<float>(mean),
            static_cast<float>(L),
            static_cast<float>(U),
            static_cast<float>(confidenceLevel),
            static_cast<float>(diff),
            symmetric);
    return buf;
}
