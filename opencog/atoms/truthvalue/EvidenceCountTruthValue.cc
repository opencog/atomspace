/*
 * opencog/atoms/truthvalue/EvidenceCountTruthValue.cc
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

#include <opencog/atoms/value/ValueFactory.h>
#include "EvidenceCountTruthValue.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

count_t EvidenceCountTruthValue::DEFAULT_K = 800.0;

EvidenceCountTruthValue::EvidenceCountTruthValue(const std::vector<double>& v)
	: TruthValue(EVIDENCE_COUNT_TRUTH_VALUE)
{
	_value = v;
}

EvidenceCountTruthValue::EvidenceCountTruthValue(count_t pos_count,
                                                 count_t total_count)
	: TruthValue(EVIDENCE_COUNT_TRUTH_VALUE)
{
	_value.resize(2);
	_value[POS_COUNT] = pos_count;
	_value[TOTAL_COUNT] = total_count;
}

EvidenceCountTruthValue::EvidenceCountTruthValue(const TruthValue& source)
	: TruthValue(EVIDENCE_COUNT_TRUTH_VALUE)
{
	_value.resize(2);
	_value[POS_COUNT] = source.get_mean() * source.get_count();
	_value[TOTAL_COUNT] = source.get_count();
}

EvidenceCountTruthValue::EvidenceCountTruthValue(EvidenceCountTruthValue const& source)
	: TruthValue(EVIDENCE_COUNT_TRUTH_VALUE)
{
	_value.resize(2);
	_value[POS_COUNT] = source.getPositiveCount();
	_value[TOTAL_COUNT] = source.get_count();
}

EvidenceCountTruthValue::EvidenceCountTruthValue(const ValuePtr& source)
       : TruthValue(EVIDENCE_COUNT_TRUTH_VALUE)
{
    if (source->get_type() != EVIDENCE_COUNT_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a EvidenceCountTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(2);
    _value[POS_COUNT] = fp->value()[POS_COUNT];
    _value[TOTAL_COUNT] = fp->value()[TOTAL_COUNT];
}

strength_t EvidenceCountTruthValue::get_mean() const
{
	if (is_count_valid())
		return getPositiveCount() / get_count();
	return NAN;
}

count_t EvidenceCountTruthValue::getPositiveCount() const
{
	return _value[POS_COUNT];
}

count_t EvidenceCountTruthValue::get_count() const
{
	return _value[TOTAL_COUNT];
}

confidence_t EvidenceCountTruthValue::get_confidence() const
{
	if (is_count_valid())
		return _value[TOTAL_COUNT] / (DEFAULT_K + _value[TOTAL_COUNT]);
	return NAN;
}

bool EvidenceCountTruthValue::is_count_valid() const
{
	return _value[POS_COUNT] <= _value[TOTAL_COUNT];
}

// This is the merge formula appropriate for PLN.
TruthValuePtr EvidenceCountTruthValue::merge(const TruthValuePtr& other,
                                             const MergeCtrl& mc) const
{
	// TODO
	switch (mc.tv_formula)
	{
	case MergeCtrl::TVFormula::HIGHER_CONFIDENCE:
		return higher_confidence_merge(other);

	case MergeCtrl::TVFormula::PLN_BOOK_REVISION:
	{
		// Based on Section 5.10.2 (A heuristic revision rule for STV)
		// of the PLN book
		if (other->get_type() != EVIDENCE_COUNT_TRUTH_VALUE)
			throw RuntimeException(TRACE_INFO,
			                       "Don't know how to merge %s into a "
			                       "EvidenceCountTruthValue using the default style",
			                       typeid(*other).name());

		auto count = get_count();
		auto count2 = other->get_count();
#define CVAL  0.2f
		auto count_new = count + count2 - std::min(count, count2) * CVAL;
#undef CVAL
		auto other_pos =
			std::static_pointer_cast<const EvidenceCountTruthValue>(other)->getPositiveCount();
		auto pos_new = (getPositiveCount() + other_pos) * count_new
			/ (count + count2);
		return createECTV(count_new, pos_new);
	}
	default:
		throw RuntimeException(TRACE_INFO,
		                       "EvidenceCountTruthValue::merge: case not implemented");
		return nullptr;
	}
}

std::string EvidenceCountTruthValue::to_string(const std::string& indent) const
{
#define BUFSZ 102
	char buf[BUFSZ];
	snprintf(buf, BUFSZ, "(ectv %f %f)",
	         static_cast<float>(_value[POS_COUNT]),
	         static_cast<float>(_value[TOTAL_COUNT]));
	return indent + buf;
}

bool EvidenceCountTruthValue::operator==(const Value& rhs) const
{
	const EvidenceCountTruthValue *ectv
		= dynamic_cast<const EvidenceCountTruthValue *>(&rhs);
	if (NULL == ectv) return false;

#define FLOAT_ACCEPTABLE_ERROR 0.000001
	auto close_enough = [](count_t lhs, count_t rhs) {
		return is_within(lhs, rhs, FLOAT_ACCEPTABLE_ERROR);
	};
#undef FLOAT_ACCEPTABLE_ERROR

	return close_enough(getPositiveCount(), ectv->getPositiveCount())
		and is_count_valid() == ectv->is_count_valid()
		and (!is_count_valid() or
		     close_enough(get_count(), ectv->get_count()));
}

DEFINE_VALUE_FACTORY(EVIDENCE_COUNT_TRUTH_VALUE,
   createEvidenceCountTruthValue, std::vector<double>)
