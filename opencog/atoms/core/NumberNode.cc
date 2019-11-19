/*
 * opencog/atoms/core/NumberNode.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <sstream>

#include <opencog/util/exceptions.h>

#include <opencog/atoms/value/FloatValue.h>
#include "NumberNode.h"

using namespace opencog;

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
///   scheme)  "#(0.1 0.2 0.3)"
///   json)    "[0.1, 0.2, 0.3]"
/// Why multiple formats? I dunno.
/// Currently, only "plain" is supported.
std::vector<double> NumberNode::to_vector(const std::string& str)
{
	std::vector<double> vec;

	size_t pos = 0;
	size_t len = str.size();
	while (true)
	{
		pos = str.find_first_of("+-0123456789.", pos);
		if (pos == std::string::npos) return vec;
		size_t last;
		vec.emplace_back(std::stod(str.substr(pos), &last));
		if (pos == std::string::npos) return vec;
		pos += last;
		if (len <= pos) return vec;
	}
	return vec;
}

// ============================================================
// Constructors

NumberNode::NumberNode(Type t, const std::string& s)
	: Node(t, s)
{
	// Convert to number and back to string to avoid miscompares.
	_value = to_vector(s);
	_name = vector_to_plain(_value);

	OC_ASSERT(nameserver().isA(_type, NUMBER_NODE),
		"Bad NumberNode constructor!");
}

NumberNode::NumberNode(const std::string& s)
	: Node(NUMBER_NODE, s)
{
	_value = to_vector(s);
	_name = vector_to_plain(_value);
}

NumberNode::NumberNode(Node &n)
	: Node(n)
{
	OC_ASSERT(nameserver().isA(_type, NUMBER_NODE),
		"Bad NumberNode constructor!");

	_value = to_vector(n.get_name());
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
// Vector ops
// There's a lot of cut-n-paste here, maybe it can be reduced.

/// Scalar multiplication
ValuePtr opencog::times(double scalar, const NumberNodePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = scalar * fv[i];

	return createFloatValue(prod);
}

/// Scalar addition
ValuePtr opencog::plus(double scalar, const NumberNodePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = scalar + fv[i];

	return createFloatValue(sum);
}

/// Scalar division
ValuePtr opencog::divide(double scalar, const NumberNodePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = scalar / fv[i];

	return createFloatValue(ratio);
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
		return plus(NumberNodeCast(vi), FloatValueCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
		return plus(FloatValueCast(vi), NumberNodeCast(vj));

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
		return minus(NumberNodeCast(vi), FloatValueCast(vj));

	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
		return minus(FloatValueCast(vi), NumberNodeCast(vj));

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
