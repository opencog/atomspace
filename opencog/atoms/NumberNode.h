/*
 * opencog/atoms/NumberNode.h
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

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 *
 * Experimental NumberNode class. This is a rough sketch for how things
 * like this might be done. It is not necessarily a good idea, and might
 * be replaced by something completely different, someday ...
 *
 * Perhaps this should be a vector of numbers???
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
	// the natural-langauge pipeline in guile/scheme. Thus, printing
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
	double value;

public:
	NumberNode(const std::string& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(NUMBER_NODE, double_to_string(std::stod(s))),
		  value(std::stod(s))
	{}

	NumberNode(double vvv)
		: Node(NUMBER_NODE, double_to_string(vvv)),
		  value(vvv)
	{}

	NumberNode(Node &n)
		: Node(n.getType(), double_to_string(std::stod(n.getName()))),
		  value(std::stod(n.getName()))
	{
		OC_ASSERT(classserver().isA(_type, NUMBER_NODE),
			"Bad NumberNode constructor!");
	}

	static std::string validate(const std::string& str)
	{
		return double_to_string(std::stod(str));
	}

	double get_value(void) { return value; }
};

typedef std::shared_ptr<NumberNode> NumberNodePtr;
static inline NumberNodePtr NumberNodeCast(const Handle& h)
	{ return std::dynamic_pointer_cast<NumberNode>(AtomCast(h)); }
static inline NumberNodePtr NumberNodeCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<NumberNode>(a); }

// XXX temporary hack ...
#define createNumberNode std::make_shared<NumberNode>

/** @}*/
}

#endif // _OPENCOG_NUMBER_NODE_H
