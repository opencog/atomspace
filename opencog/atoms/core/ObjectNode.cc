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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "ObjectNode.h"

using namespace opencog;

/// Implement Jenkins' One-at-a-Time hash.
/// For these very short strings, I cannot think of a faster hash.
/// The 4-byte-at-a-time hashes require knowng the string length :-(
uint32_t constexpr ObjectNode::dispatch_hash(const char* s)
{
	uint32_t hash = 0;

	for(; *s; ++s)
	{
		hash += *s;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);

	return hash;
}

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
