/*
 * opencog/atoms/core/ObjectNode.h
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

#ifndef _OPENCOG_OBJECT_NODE_H
#define _OPENCOG_OBJECT_NODE_H

#include <opencog/atoms/base/Node.h>

#include <string>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// Dispatcher for setValue, getValue.
class ObjectNode : public Node
{
protected:
	static uint32_t constexpr dispatch_hash(const char*);

	/**
	 * Return debug diagnostics and/or performance monitoring stats.
	 */
   virtual std::string monitor(void) const;

public:
	virtual ~ObjectNode();

	virtual void setValue(const Handle& key, const ValuePtr& value);
	virtual ValuePtr getValue(const Handle& key) const;
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_OBJECT_NODE_H
