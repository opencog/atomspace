/*
 * opencog/atoms/core/NumberNode.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <sstream>

#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/value/FloatValue.h>
#include "NumberNode.h"

using namespace opencog;

ValuePtr NumberNode::value_at_index(size_t idx) const
{
	double d = 0.0;
	if (_value.size() > idx) d = _value[idx];
	return createNumberNode(d);
}

/// Convert vector to json-format string
/// Why json? I dunno.
std::string NumberNode::vector_to_json(const std::vector<double>& vec)
{
	std::stringstream ss;
	ss << "[";
	for (double v: vec) ss << double_to_string(v) << ", ";
	ss << "]";
	return ss.str();
}

std::string NumberNode::vector_to_plain(const std::vector<double>& vec)
{
	std::stringstream ss;
	for (double v: vec) ss << double_to_string(v) << " ";

	// Drop the trailing blank.
	size_t len = ss.str().size();
	return ss.str().substr(0, len-1);
}

/// Support multiple formats:
///   plain)   "0.1 0.2 0.3"
///   csv)     "0.1, 0.2, 0.3"
///   tsv)     "0.1\t0.2\t0.3"
///   scheme)  "#(0.1 0.2 0.3)"
///   json)    "[0.1, 0.2, 0.3]"
/// Why multiple formats? I dunno. Seems like a reasonable thing to do.
std::vector<double> NumberNode::to_vector(const std::string& str)
{
	std::vector<double> vec;

	size_t pos = 0;
	size_t in = 0;
	if (0 == str.find("#(")) in = 2;      // is it a scheme vector?
	else if (0 == str.find("[")) in = 1;  // is it a json vector?

	// First condition in the loop will trim str.
	while ((in = str.find_first_not_of(", \t\r\n", in)) != std::string::npos
	       and (pos = str.find_first_of(", \t", in)) != std::string::npos)
	{
		vec.emplace_back(std::stod(str.substr(in, pos - in)));
		in = pos + 1;
	}
	if (in != std::string::npos) vec.emplace_back(std::stod(str.substr(in)));
	return vec;
}

// ============================================================
// Constructors

NumberNode::NumberNode(Type t, const std::string&& s)
	: Node(t, std::move(s))
{
	// Convert to number and back to string to avoid miscompares.
	_value = to_vector(s);
	_name = vector_to_plain(_value);

	OC_ASSERT(nameserver().isA(_type, NUMBER_NODE),
		"Bad NumberNode constructor!");
}

NumberNode::NumberNode(const std::string&& s)
	: Node(NUMBER_NODE, std::move(s))
{
	_value = to_vector(s);
	_name = vector_to_plain(_value);
}

NumberNode::NumberNode(const std::vector<double>& vec)
	: Node(NUMBER_NODE, "")
{
	_value = vec;
	_name = vector_to_plain(_value);
}

NumberNode::NumberNode(const FloatValuePtr& fv)
	: Node(NUMBER_NODE, "")
{
	_value = fv->value();
	_name = vector_to_plain(_value);
}

NumberNode::NumberNode(const ValuePtr& vp)
	: Node(NUMBER_NODE, "")
{
	if (nameserver().isA(vp->get_type(), NUMBER_NODE))
	{
		NumberNodePtr fv = NumberNodeCast(vp);
		_value = fv->value();
		_name = vector_to_plain(_value);
		return;
	}
	if (nameserver().isA(vp->get_type(), FLOAT_VALUE))
	{
		FloatValuePtr fv = FloatValueCast(vp);
		_value = fv->value();
		_name = vector_to_plain(_value);
		return;
	}
	throw RuntimeException(TRACE_INFO,
		"Bad NumberNode constructor, expecting FloatValue!");
}

// ============================================================

/// Scalar addition
ValuePtr opencog::plus(double f, const ValuePtr& vj, bool silent)
{
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vjtype)
		return plus(f, NumberNodeCast(vj));

	if (nameserver().isA(vjtype, FLOAT_VALUE))
		return plus(f, FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

/// Scalar subtraction
ValuePtr opencog::minus(double f, const ValuePtr& vj, bool silent)
{
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vjtype)
		return minus(f, NumberNodeCast(vj));

	if (nameserver().isA(vjtype, FLOAT_VALUE))
		return minus(f, FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

/// Scalar multiplication
ValuePtr opencog::times(double f, const ValuePtr& vj, bool silent)
{
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vjtype)
		return times(f, NumberNodeCast(vj));

	if (nameserver().isA(vjtype, FLOAT_VALUE))
		return times(f, FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

/// Scalar division
ValuePtr opencog::divide(double f, const ValuePtr& vj, bool silent)
{
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vjtype)
		return divide(f, NumberNodeCast(vj));

	if (nameserver().isA(vjtype, FLOAT_VALUE))
		return divide(f, FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

// ============================================================

/// Vector (point-wise) addition
ValuePtr opencog::plus(const ValuePtr& vi, const ValuePtr& vj, bool silent)
{
	Type vitype = vi->get_type();
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
		return plus(NumberNodeCast(vi), NumberNodeCast(vj));

	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		NumberNodePtr nn(NumberNodeCast(vi));
		if (1 == nn->size())
			return plus(nn->get_value(), FloatValueCast(vj));
		return plus(nn, FloatValueCast(vj));
	}

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
	{
		NumberNodePtr nn(NumberNodeCast(vj));
		if (1 == nn->size())
			return plus(nn->get_value(), FloatValueCast(vi));
		return plus(nn, FloatValueCast(vi));
	}

	if (nameserver().isA(vitype, FLOAT_VALUE) and
		 nameserver().isA(vjtype, FLOAT_VALUE))
		return plus(FloatValueCast(vi), FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

/// Vector (point-wise) subtraction
ValuePtr opencog::minus(const ValuePtr& vi, const ValuePtr& vj, bool silent)
{
	Type vitype = vi->get_type();
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
		return minus(NumberNodeCast(vi), NumberNodeCast(vj));

	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		NumberNodePtr nn(NumberNodeCast(vi));
		if (1 == nn->size())
			return minus(nn->get_value(), FloatValueCast(vj));
		return minus(nn, FloatValueCast(vj));
	}

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
	{
		NumberNodePtr nn(NumberNodeCast(vj));
		if (1 == nn->size())
			return minus(FloatValueCast(vi), nn->get_value());
		return minus(FloatValueCast(vi), nn);
	}

	if (nameserver().isA(vitype, FLOAT_VALUE) and
		 nameserver().isA(vjtype, FLOAT_VALUE))
		return minus(FloatValueCast(vi), FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

ValuePtr opencog::times(const ValuePtr& vi, const ValuePtr& vj, bool silent)
{
	Type vitype = vi->get_type();
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
		return times(NumberNodeCast(vi), NumberNodeCast(vj));

	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
		return times(NumberNodeCast(vi), FloatValueCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
		return times(FloatValueCast(vi), NumberNodeCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and
		 nameserver().isA(vjtype, FLOAT_VALUE))
		return times(FloatValueCast(vi), FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

ValuePtr opencog::divide(const ValuePtr& vi, const ValuePtr& vj, bool silent)
{
	Type vitype = vi->get_type();
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) addition.
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
		return divide(NumberNodeCast(vi), NumberNodeCast(vj));

	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
		return divide(NumberNodeCast(vi), FloatValueCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
		return divide(FloatValueCast(vi), NumberNodeCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and
		 nameserver().isA(vjtype, FLOAT_VALUE))
		return divide(FloatValueCast(vi), FloatValueCast(vj));

	if (silent) throw SilentException();

	throw RuntimeException(TRACE_INFO,
		"Expecting NumberNode or FloatValue!");
}

// ============================================================

DEFINE_NODE_FACTORY(NumberNode, NUMBER_NODE)
