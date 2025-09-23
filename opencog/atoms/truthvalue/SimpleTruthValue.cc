/*
 * opencog/atoms/truthvalue/SimpleTruthValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Welter Silva <welter@vettalabs.com>
 *            Guilherme Lamacie

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

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include <opencog/atoms/value/ValueFactory.h>
#include "SimpleTruthValue.h"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

count_t SimpleTruthValue::DEFAULT_K = 800.0;

SimpleTruthValue::SimpleTruthValue(const std::vector<double>& v)
	: TruthValue(SIMPLE_TRUTH_VALUE)
{
	_value = v;
}

SimpleTruthValue::SimpleTruthValue(strength_t m, confidence_t c)
	: TruthValue(SIMPLE_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = m;
    _value[CONFIDENCE] = c;
}

SimpleTruthValue::SimpleTruthValue(const TruthValue& source)
	: TruthValue(SIMPLE_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = source.get_mean();
    _value[CONFIDENCE] = source.get_confidence();
}

SimpleTruthValue::SimpleTruthValue(const SimpleTruthValue& source)
	: TruthValue(SIMPLE_TRUTH_VALUE)
{
    _value.resize(2);
    _value[MEAN] = source._value[MEAN];
    _value[CONFIDENCE] = source._value[CONFIDENCE];
}

SimpleTruthValue::SimpleTruthValue(const ValuePtr& source)
	: TruthValue(SIMPLE_TRUTH_VALUE)
{
	if (not nameserver().isA(source->get_type(), FLOAT_VALUE))
		throw RuntimeException(TRACE_INFO,
			"Source must be a FloatValue");

	FloatValuePtr fp(FloatValueCast(source));
	if (fp->value().size() < 2)
		throw RuntimeException(TRACE_INFO,
			"FloatValue must have at least two elements!");

	_value.resize(2);
	_value[MEAN] = fp->value()[MEAN];
	_value[CONFIDENCE] = fp->value()[CONFIDENCE];
}

strength_t SimpleTruthValue::get_mean() const
{
    return _value[MEAN];
}

count_t SimpleTruthValue::get_count() const
{
    // Formula from PLN book.
    confidence_t cf = std::min(_value[CONFIDENCE], 0.9999998);
    return static_cast<count_t>(DEFAULT_K * cf / (1.0 - cf));
}

confidence_t SimpleTruthValue::get_confidence() const
{
    return _value[CONFIDENCE];
}

std::string SimpleTruthValue::to_string(const std::string& indent) const
{
#define BUFSZ 100
    char buf[BUFSZ];
    // Stick to the old, historic short name, for backwards-compat.
    // snprintf(buf, BUFSZ, "(SimpleTruthValue %g %g)",
    snprintf(buf, BUFSZ, "(stv %g %g)",
             static_cast<float>(get_mean()),
             static_cast<float>(get_confidence()));
    return indent + buf;
}

bool SimpleTruthValue::operator==(const Value& rhs) const
{
    if (not rhs.is_type(SIMPLE_TRUTH_VALUE))
        return false;

    const SimpleTruthValue *stv = dynamic_cast<const SimpleTruthValue *>(&rhs);
    if (NULL == stv) return false;

    if (not nearly_equal(stv->_value[MEAN], _value[MEAN]))
        return false;

    if (not nearly_equal(stv->_value[CONFIDENCE], _value[CONFIDENCE])) 
        return false;
    return true;
}

DEFINE_VALUE_FACTORY(SIMPLE_TRUTH_VALUE,
	createSimpleTruthValue, std::vector<double>)
