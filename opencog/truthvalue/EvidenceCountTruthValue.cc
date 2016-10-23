/*
 * opencog/atomspace/EvidenceCountTruthValue.cc
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
 *
 */

#include <math.h>
#include <typeinfo>

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/numeric.h>

#include "EvidenceCountTruthValue.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

EvidenceCountTruthValue::EvidenceCountTruthValue(count_t pos_count,
                                                 count_t total_count)
{
	_pos_count = pos_count;
	_total_count = total_count;
}

EvidenceCountTruthValue::EvidenceCountTruthValue(const TruthValue& source)
{
	_pos_count = source.getMean() * source.getCount();
	_total_count = source.getCount();
}
EvidenceCountTruthValue::EvidenceCountTruthValue(EvidenceCountTruthValue const& source)
{
	_pos_count = source._pos_count;
	_total_count = source._total_count;
}

bool EvidenceCountTruthValue::is_total_count_valid() const
{
	return _pos_count <= _total_count;
}

strength_t EvidenceCountTruthValue::getMean() const
{
	if (is_total_count_valid())
		return _pos_count / _total_count;
	return NAN;
}

count_t EvidenceCountTruthValue::getCount() const
{
	return _total_count;
}

confidence_t EvidenceCountTruthValue::getConfidence() const
{
	if (is_total_count_valid())
		return _total_count / (DEFAULT_K + _total_count);
	return NAN;
}

// This is the merge formula appropriate for PLN.
TruthValuePtr EvidenceCountTruthValue::merge(TruthValuePtr other,
                                             const MergeCtrl& mc) const
{
	// TODO
	switch(mc.tv_formula)
	{
	case MergeCtrl::TVFormula::HIGHER_CONFIDENCE:
		return higher_confidence_merge(other);

	case MergeCtrl::TVFormula::PLN_BOOK_REVISION:
	{
		// Based on Section 5.10.2(A heuristic revision rule for STV)
		// of the PLN book
		if (other->getType() != SIMPLE_TRUTH_VALUE)
			throw RuntimeException(TRACE_INFO,
			                       "Don't know how to merge %s into a "
			                       "EvidenceCountTruthValue using the default style",
			                       typeid(*other).name());

		confidence_t cf = std::min(getConfidence(), 0.9999998f);
		auto count = static_cast<count_t>(DEFAULT_K * cf / (1.0f - cf));
		auto count2 = other->getCount();
#define CVAL  0.2f
		auto count_new = count + count2 - std::min(count, count2) * CVAL;
		auto mean_new = (getMean() * count + other->getMean() * count2)
			/ (count + count2);
		confidence_t confidence_new = static_cast<confidence_t>(count_new / (count_new + DEFAULT_K));
		return std::make_shared<EvidenceCountTruthValue>(mean_new, confidence_new);
	}
	default:
		throw RuntimeException(TRACE_INFO,
		                       "EvidenceCountTruthValue::merge: case not implemented");
		return nullptr;
	}
}

std::string EvidenceCountTruthValue::toString() const
{
	char buf[1024];
	sprintf(buf, "(ectv %f %f)",
	        static_cast<float>(_pos_count),
	        static_cast<float>(_total_count));
	return buf;
}

bool EvidenceCountTruthValue::operator==(const TruthValue& rhs) const
{
	const EvidenceCountTruthValue *ectv
		= dynamic_cast<const EvidenceCountTruthValue *>(&rhs);
	if (NULL == ectv) return false;

#define FLOAT_ACCEPTABLE_ERROR 0.000001
	auto close_enough = [](count_t lhs, count_t rhs) {
		return isWithin(lhs, rhs, FLOAT_ACCEPTABLE_ERROR);
	};
#undef FLOAT_ACCEPTABLE_ERROR

	return close_enough(_pos_count, ectv->_pos_count)
		and is_total_count_valid() == ectv->is_total_count_valid()
		and (!is_total_count_valid() or
		     close_enough(_total_count, ectv->_total_count));
}

TruthValueType EvidenceCountTruthValue::getType() const
{
	return EVIDENCE_COUNT_TRUTH_VALUE;
}
