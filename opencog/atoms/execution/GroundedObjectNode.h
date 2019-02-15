/*
 * opencog/atoms/execution/GroundedObjectNode.h
 *
 * Copyright (C) 2019 OpenCog Foundation
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

#ifndef _OPENCOG_GROUNDED_OBJECT_NODE_H
#define _OPENCOG_GROUNDED_OBJECT_NODE_H

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/PtrValue.h>
#include <opencog/atoms/execution/GroundedObject.h>

namespace opencog
{

class GroundedObjectNode : public Node
{
private:
	static Handle ptrKey;

public:

	GroundedObjectNode(Type type, const std::string& name)
		: Node(type, name) { }

	GroundedObjectNode(const std::string& name)
		: Node(GROUNDED_OBJECT_NODE, name) { }

	GroundedObjectNode(const std::string& name,
			const std::shared_ptr<GroundedObject>& object);

	bool has_object() const;
	void set_object(const std::shared_ptr<GroundedObject>& object);
	GroundedObject* get_object() const;

	static Handle factory(const Handle&);
};

using GroundedObjectNodePtr = std::shared_ptr<GroundedObjectNode>;

template<typename ... Args>
static inline GroundedObjectNodePtr createGroundedObjectNode(Args&&... args)
{
	return std::make_shared<GroundedObjectNode>(std::forward<Args>(args)...);
}

}

#endif // _OPENCOG_GROUNDED_OBJECT_NODE_H

