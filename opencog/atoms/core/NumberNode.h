/*
 * opencog/atoms/core/NumberNode.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
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

#ifndef _OPENCOG_NUMBER_NODE_H
#define _OPENCOG_NUMBER_NODE_H

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
	// system is explcitly defined to be locale-independent, including
	// the natural-language pipeline in guile/scheme. Thus, printing
	// the European comma as a decimal separator blows up the code.
	static std::string double_to_string(double x)
	{
		std::string vs(std::to_string(x));
		std::size_t found = vs.find(',');
		if (std::string::npos != found)
			vs[found] = '.';
		return vs;
	}

protected:
	std::vector<double> _value;

public:
	// Please to NOT use this constructor!
	NumberNode(Type, const std::string&);

public:
	NumberNode(const std::string&);
	NumberNode(const std::vector<double>&);
	NumberNode(const FloatValuePtr&);
	NumberNode(const ValuePtr&);

	NumberNode(double vvv)
		: Node(NUMBER_NODE, double_to_string(vvv))
	{ _value.push_back(vvv); }

	// TODO Should be a move assignment...
	NumberNode(Node &);

	static std::vector<double> to_vector(const std::string&);
	static std::string vector_to_json(const std::vector<double>&);
	static std::string vector_to_plain(const std::vector<double>&);

	static std::string validate(const std::string& str)
	{
		return vector_to_plain(to_vector(str));
	}

	std::vector<double> value(void) { return _value; }
	double get_value(void) { return _value[0]; }

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<NumberNode> NumberNodePtr;
static inline NumberNodePtr NumberNodeCast(const Handle& h)
	{ return std::dynamic_pointer_cast<NumberNode>(h); }
static inline NumberNodePtr NumberNodeCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<NumberNode>(a); }
static inline NumberNodePtr NumberNodeCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<NumberNode>(a); }

#define createNumberNode std::make_shared<NumberNode>

// Scalar multiplication and addition
ValuePtr times(double, const NumberNodePtr&);
ValuePtr plus(double, const NumberNodePtr&);
ValuePtr divide(double, const NumberNodePtr&);

// Vector multiplication and addition
inline
ValuePtr times(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr plus(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

inline
ValuePtr times(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr plus(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

inline
ValuePtr times(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(times(fvpa->value(), fvpb->value())); }
inline
ValuePtr plus(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(plus(fvpa->value(), fvpb->value())); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
	return createFloatValue(divide(fvpa->value(), fvpb->value())); }

ValuePtr times(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr plus(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr divide(const ValuePtr&, const ValuePtr&, bool silent=false);

/** @}*/
}

#endif // _OPENCOG_NUMBER_NODE_H
