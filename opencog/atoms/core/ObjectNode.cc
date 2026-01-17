/*
 * opencog/atoms/core/ObjectNode.cc
 *
 * Copyright (C) 2025 Linas Vepstas
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
 */

#include "ObjectNode.h"

using namespace opencog;

ObjectNode::ObjectNode(Type t, const std::string&& uri) :
   Node(t, std::move(uri))
{
   if (not nameserver().isA(t, OBJECT_NODE))
      throw RuntimeException(TRACE_INFO, "Bad inheritance!");
}

std::string ObjectNode::monitor(void) const
{
	return "This ObjectNode does not implement a monitor.";
}

// ============================== END OF FILE =========================
