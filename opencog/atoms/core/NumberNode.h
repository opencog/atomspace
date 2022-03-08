/*
 * opencog/atoms/core/NumberNode.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_NUMBER_NODE_H
#define _OPENCOG_NUMBER_NODE_H

#include <boost/lexical_cast.hpp>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/FloatValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 *
 * NumberNode implementation. This is a vector of floats; thus, just
 * like a FloatValue, except that it's a Node. We don't want to actually
 * do multiple inheritance here, as that tends to blow up C++.
 */

class NumberNode : public Node
{
private:

	// It turns out that std::to_string(double x) is locale-dependent
	// See, for example, Martin B.'s comment here:
	// http://comp.lang.cpp.moderated.narkive.com/dWGdM0Od/std-to-string-int-deviates-from-iostreams-result-deliberate
	// Also this:
	// https://github.com/nlohmann/json/issues/51
	// And this:
	// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/p0067r0.html
	// This was a painful discovery that ate the chatbot's lunch.
	// So we perform a hack here.  The core issue is that the rest of the
	// system is explicitly defined to be locale-independent, including
	// the natural-language pipeline in guile/scheme. Thus, printing
	// the European comma as a decimal separator blows up the code.
	// Using boost::lexical_cast<> avoids this issue.
	static std::string double_to_string(double x)
	{
		return boost::lexical_cast<std::string>(x);
	}

protected:
	std::vector<double> _value;

public:
	// Please to NOT use this constructor!
	NumberNode(Type, const std::string&&);

public:
	NumberNode(const std::string&&);
	NumberNode(const std::vector<double>&);
	NumberNode(const FloatValuePtr&);
	NumberNode(const ValuePtr&);

	NumberNode(double vvv)
		: Node(NUMBER_NODE, double_to_string(vvv))
	{ _value.push_back(vvv); }

	NumberNode(NumberNode&) = delete;
	NumberNode& operator=(const NumberNode&) = delete;

	static std::vector<double> to_vector(const std::string&);
	static std::string vector_to_json(const std::vector<double>&);
	static std::string vector_to_plain(const std::vector<double>&);

	static std::string validate(const std::string& str)
	{
		return vector_to_plain(to_vector(str));
	}

	size_t size() const { return _value.size(); }
	const std::vector<double>& value(void) const { return _value; }
	double get_value(void) const { return _value[0]; }

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(NumberNode)
#define createNumberNode CREATE_DECL(NumberNode)

static inline NumberNodePtr NumberNodeCast(const ValuePtr& vp)
    { return std::dynamic_pointer_cast<NumberNode>(vp); }

// --------------------
// Scalar multiplication and addition
inline
ValuePtr plus(double f, const NumberNodePtr& fvp) {
	return createFloatValue(plus(f, fvp->value())); }
inline
ValuePtr minus(double f, const NumberNodePtr& fvp) {
	return createFloatValue(minus(f, fvp->value())); }
inline
ValuePtr times(double f, const NumberNodePtr& fvp) {
	return createFloatValue(times(f, fvp->value())); }
inline
ValuePtr divide(double f, const NumberNodePtr& fvp) {
	return createFloatValue(divide(f, fvp->value())); }

ValuePtr plus(double, const ValuePtr&, bool silent=false);
ValuePtr minus(double, const ValuePtr&, bool silent=false);
ValuePtr times(double, const ValuePtr&, bool silent=false);
ValuePtr divide(double, const ValuePtr&, bool silent=false);

// Vector multiplication and addition
inline
ValuePtr plus(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr minus(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(minus(fvpa->value(), fvpb->value())); }
inline
ValuePtr times(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

inline
ValuePtr plus(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr minus(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(minus(fvpa->value(), fvpb->value())); }
inline
ValuePtr times(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

inline
ValuePtr plus(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr minus(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(minus(fvpa->value(), fvpb->value())); }
inline
ValuePtr times(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

ValuePtr plus(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr minus(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr times(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr divide(const ValuePtr&, const ValuePtr&, bool silent=false);

/** @}*/
}

#endif // _OPENCOG_NUMBER_NODE_H
