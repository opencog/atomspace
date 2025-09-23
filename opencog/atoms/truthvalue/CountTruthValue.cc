/*
 * opencog/atoms/truthvalue/CountTruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Guilherme Lamacie
 *            Linas Vepstas <linasvepstas@gmail.com>
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

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/ValueFactory.h>
#include "CountTruthValue.h"

using namespace opencog;

CountTruthValue::CountTruthValue(const std::vector<double>& v)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value = v;
}

CountTruthValue::CountTruthValue(strength_t m, confidence_t n, count_t c)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = m;
    _value[CONFIDENCE] = n;
    _value[COUNT] = c;
}

CountTruthValue::CountTruthValue(const TruthValue& source)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.get_mean();
    _value[CONFIDENCE] = source.get_confidence();
    _value[COUNT] = source.get_count();
}

CountTruthValue::CountTruthValue(CountTruthValue const& source)
	: TruthValue(COUNT_TRUTH_VALUE)
{
    _value.resize(3);
    _value[MEAN] = source.get_mean();
    _value[CONFIDENCE] = source.get_confidence();
    _value[COUNT] = source.get_count();
}

CountTruthValue::CountTruthValue(const ValuePtr& source)
       : TruthValue(COUNT_TRUTH_VALUE)
{
    if (source->get_type() != COUNT_TRUTH_VALUE)
        throw RuntimeException(TRACE_INFO,
            "Source must be a CountTruthValue");

    FloatValuePtr fp(FloatValueCast(source));
    _value.resize(3);
    _value[MEAN] = fp->value()[MEAN];
    _value[CONFIDENCE] = fp->value()[CONFIDENCE];
    _value[COUNT] = fp->value()[COUNT];
}

strength_t CountTruthValue::get_mean() const
{
    return _value[MEAN];
}

count_t CountTruthValue::get_count() const
{
    return  _value[COUNT];
}

confidence_t CountTruthValue::get_confidence() const
{
    return _value[CONFIDENCE];
}

ValuePtr CountTruthValue::incrementCount(const std::vector<double>& delta) const
{
	// Make a copy
	std::vector<double> new_vect = _value;

	for (size_t idx=0; idx <= 2; idx++)
		new_vect[idx] += delta[idx];

	// Return a brand new copy
	return ValueCast(createCountTruthValue(new_vect));
}

ValuePtr CountTruthValue::incrementCount(size_t idx, double count) const
{
	// Make a copy
	std::vector<double> new_vect = _value;

	if (2 >= idx)
		new_vect[idx] += count;

	// Return a brand new copy
	return ValueCast(createCountTruthValue(new_vect));
}

std::string CountTruthValue::to_string(const std::string& indent) const
{
#define BUFSZ 102
    char buf[BUFSZ];
    snprintf(buf, BUFSZ, "(CountTruthValue %g %g %g)",
             static_cast<float>(get_mean()),
             static_cast<double>(get_confidence()),
             static_cast<float>(get_count()));
    return indent + buf;
}

bool CountTruthValue::operator==(const Value& rhs) const
{
    const CountTruthValue *ctv = dynamic_cast<const CountTruthValue *>(&rhs);
    if (NULL == ctv) return false;

    if (not nearly_equal(get_mean(), ctv->get_mean())) return false;
    if (not nearly_equal(get_confidence(), ctv->get_confidence())) return false;
    if (not nearly_equal(ctv->get_count(), get_count())) return false;

    return true;
}

DEFINE_VALUE_FACTORY(COUNT_TRUTH_VALUE,
	createCountTruthValue, std::vector<double>)
