/*
 * opencog/atoms/execution/GroundedObjectNode.cc
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

#include <opencog/atoms/value/PtrValue.h>

#include "GroundedObjectNode.h"

using namespace opencog;

Handle GroundedObjectNode::ptrKey = createNode(NODE, "GroundedObjectNodeValuePtrKey");

GroundedObjectNode::GroundedObjectNode(const std::string& name,
		const std::shared_ptr<GroundedObject>& object)
	: Node(GROUNDED_OBJECT_NODE, name)
{
	set_object(object);
}

bool GroundedObjectNode::has_object() const
{
	ValuePtr ptr_value = getValue(GroundedObjectNode::ptrKey);
	return (bool)ptr_value;
}

void GroundedObjectNode::set_object(const std::shared_ptr<GroundedObject>& object)
{
	PtrValuePtr ptrValue = createPtrValue(object);
	setValue(GroundedObjectNode::ptrKey, ValueCast(ptrValue));
}

GroundedObject* GroundedObjectNode::get_object() const
{
	ValuePtr ptr_value = getValue(GroundedObjectNode::ptrKey);
	if (!ptr_value)
		throw RuntimeException(TRACE_INFO,
				"Trying to get object from empty GroundedObjectNode");
	return static_cast<GroundedObject*>(
			CastFromValue<PtrValue>(ptr_value)->value());
}

auto GroundedObjectNodeCast = CastFromHandle<GroundedObjectNode>;

DEFINE_NODE_FACTORY(GroundedObjectNode, GROUNDED_OBJECT_NODE)

