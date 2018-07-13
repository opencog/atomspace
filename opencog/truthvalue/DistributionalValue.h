/*
 * opencog/truthvalue/DistributionalValue.h
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_DISTRIBUTIONAL_VALUE_H
#define _OPENCOG_DISTRIBUTIONAL_VALUE_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/proto/ProtoAtom.h>
#include <opencog/atoms/base/Handle.h>

#include <opencog/truthvalue/ConditionalDV.h>

/** \addtogroup grp_atomspace
 *  @{
 */

namespace opencog
{

typedef double strength_t;
typedef double confidence_t;
typedef double count_t;

class DistributionalValue;
typedef std::shared_ptr<const DistributionalValue> DistributionalValuePtr;

class AtomSpace;

class DistributionalValue
    : public ProtoAtom
{

    friend class ConditionalDV;

    HandleCounter value;
    int k;

    // Disallow assignment -- truth values are immutable!
    DistributionalValue& operator=(const DistributionalValue& rhs) {
        throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
    }


public:
    static count_t DEFAULT_K;

    DistributionalValue();
    DistributionalValue(HandleCounter);
    DistributionalValue(double,double);

    static DistributionalValuePtr UniformDistributionalValue(Handle,int);
    static DistributionalValuePtr UniformDistributionalValue(HandleSeq,int);
    static DistributionalValuePtr TRUE_TV();
    static DistributionalValuePtr FALSE_TV();
    static DistributionalValuePtr DEFAULT_TV();
    static DistributionalValuePtr createDV(double,double);
    static DistributionalValuePtr createDV(HandleCounter);

    ConditionalDVPtr divide(DistributionalValuePtr,int) const;
    DistributionalValuePtr SumJoint(DistributionalValuePtr,int) const;
    HandleCounter PartJoint(Handle,int) const;

    bool IsUniform() const;

    std::vector<double> get_mode() const;
    std::vector<double> get_mean() const;
    std::vector<double> get_var() const;

    double get_fstord_mean() const;
    double middleOfInterval(Handle) const;

    double get_mode_for(double) const;
    double get_mean_for(double) const;
    double get_var_for(double) const;

    DistributionalValuePtr AddEvidence(Handle) const;
    DistributionalValuePtr merge(DistributionalValuePtr) const;
    DistributionalValuePtr negate() const;

    double minCount() const;
    double maxCount() const;

    double total_count() const;
    double get_confidence() const;

    static double to_conf(int c);
    static int to_count(double);

    Handle getKey(Handle) const;
    double getCount(Handle) const;
    double getMean(Handle) const;
    double getMode(Handle) const;
    double getVar(Handle) const;

    HandleSeq getKeys() const;

    virtual bool operator==(const ProtoAtom& rhs) const;

    using ProtoAtom::to_string;
    std::string to_string(const std::string&) const;
};

static inline DistributionalValuePtr DistributionalValueCast(const ProtoAtomPtr& pa)
    { return std::dynamic_pointer_cast<const DistributionalValue>(pa); }

static inline ProtoAtomPtr ProtoAtomCast(const DistributionalValuePtr& dv)
{
    // This should have worked!?
    // return std::const_pointer_cast<ProtoAtom>(tv);

    // This, too, should have worked!?
    // return std::shared_ptr<ProtoAtom>(tv, const_cast<ProtoAtom*>(tv.get()));

    // This works...
    return std::shared_ptr<ProtoAtom>(dv, (ProtoAtom*) dv.get());
}

std::string oc_to_string(const DistributionalValue& hc);

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
