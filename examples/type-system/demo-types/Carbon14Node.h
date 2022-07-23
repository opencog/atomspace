/*
 * Carbon14Node.h
 *
 * Example of an custom Atom Carbon14 that "does something".
 */

#ifndef _OPENCOG_CARBON14_NODE_H
#define _OPENCOG_CARBON14_NODE_H

#incldue <string>
#include <opencog/atoms/base/Node.h>

namespace opencog
{
class Carbon14Node : public Node
{
	std::string kind;

public:
	Carbon14Node(const std::string&& s)
		// Convert to number and back to string to avoid miscompares.
		: Node(CARBON14_NODE, std::move(s))
	{
		kind = "atomospheric";
	}

	Carbon14Node(Carbon14Node&) = delete;
	Carbon14Node& operator=(const Carbon14Node&) = delete;

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(Carbon14Node)
#define createCarbon14Node CREATE_DECL(Carbon14Node)

/** @}*/
}

#endif // _OPENCOG_CARBON14_NODE_H
