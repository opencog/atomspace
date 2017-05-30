/*
 * opencog/truthvalue/EvidenceCountTruthValue.h
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#ifndef _OPENCOG_EVIDENCE_TRUTH_VALUE_H_
#define _OPENCOG_EVIDENCE_TRUTH_VALUE_H_

#include <opencog/truthvalue/TruthValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class EvidenceCountTruthValue;
typedef std::shared_ptr<const EvidenceCountTruthValue> EvidenceCountTruthValuePtr;

//! a TruthValue that stores a strength and confidence.
class EvidenceCountTruthValue : public TruthValue
{
protected:
   enum {
		POS_COUNT, /// Positive evidence count, i.e. number of
		           /// observations corroborating with the atom's truth.

		TOTAL_COUNT /// Total number of observations.
   };

	static count_t DEFAULT_K;

public:
	EvidenceCountTruthValue(count_t pos_count, count_t total_count = -1);
	EvidenceCountTruthValue(const TruthValue&);
	EvidenceCountTruthValue(EvidenceCountTruthValue const&);
	EvidenceCountTruthValue(const ProtoAtomPtr&);

	virtual bool operator==(const ProtoAtom&) const;

	std::string toString(const std::string&) const;

	strength_t getMean() const;
	count_t getCount() const;
	confidence_t getConfidence() const;

	// Return the positive evidence count
	count_t getPositiveCount() const;

	/// If _pos_count <= _total_count then _total_count is considered
	/// valid, otherwise it is considered invalid.
	bool is_count_valid() const;

	/**
	 * Truth value merge formula, as specified by PLN.
	 *
	 * Currently tv1.merge(tv2) works as follows:
	 * the resulting TV is either tv1 or tv2, the result being the one
	 * with the highest confidence.
	 */
	TruthValuePtr merge(const TruthValuePtr&,
	                    const MergeCtrl& mc=MergeCtrl()) const;

	static EvidenceCountTruthValuePtr createECTV(count_t pos_count,
	                                             count_t total_count = -1.0)
	{
		return std::make_shared<const EvidenceCountTruthValue>(pos_count, total_count);
	}
	static TruthValuePtr createTV(count_t pos_count, count_t total_count = -1.0)
	{
		return std::static_pointer_cast<const TruthValue>(createECTV(pos_count,
		                                                       total_count));
	}
	static TruthValuePtr createTV(const ProtoAtomPtr& pap)
	{
		return std::static_pointer_cast<const TruthValue>(
			std::make_shared<const EvidenceCountTruthValue>(pap));
	}

	TruthValuePtr clone() const
	{
		return std::make_shared<EvidenceCountTruthValue>(*this);
	}
	TruthValue* rawclone() const
	{
		return new EvidenceCountTruthValue(*this);
	}
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_EVIDENCE_COUNT_TRUTH_VALUE_H_
