/*
 * Carbon14Node.h
 *
 * Example of an custom Atom Carbon14 that "does something".
 */

#ifndef _OPENCOG_CARBON14_NODE_H
#define _OPENCOG_CARBON14_NODE_H

#include <string>
#include <opencog/atoms/base/Node.h>

// This file is not install, but is taken from the build directory.
#include "examples/type-system/demo-types/chem_types.h"

namespace opencog
{
class Carbon14Node : public Node
{
protected:
	std::string kind;

public:
	// The "usual" constructor.
	Carbon14Node(const std::string&& s)
		: Node(CARBON14_NODE, std::move(s))
	{
		kind = "unknown";
	}

	// The constructor that dervied types need.
	Carbon14Node(Type t, const std::string&& s)
		: Node(t, std::move(s))
	{
		kind = "atomospheric";
	}

	// Atoms are globally unique and not assignable or copyable.
	Carbon14Node(Carbon14Node&) = delete;
	Carbon14Node& operator=(const Carbon14Node&) = delete;

	// A function that "actually does something".
	virtual ValuePtr execute(AtomSpace*, bool silent=true);

	// Let everyone know that we can do the above.
	virtual bool is_executable() const { return true; }

	// A factory to create this atom. This is just more automated
	// boilerplate needed by the type subsystem.
	static Handle factory(const Handle&);
};

NODE_PTR_DECL(Carbon14Node)
#define createCarbon14Node CREATE_DECL(Carbon14Node)

/** @}*/
}

#endif // _OPENCOG_CARBON14_NODE_H
