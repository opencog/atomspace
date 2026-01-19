/*
 * opencog/atoms/grant/NameNode.h
 *
 * Copyright (C) 2020, 2024 Linas Vepstas
 * Copyright (C) 2026 BrainyBlaze Dynamics, LLC
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#ifndef _OPENCOG_NAME_NODE_H
#define _OPENCOG_NAME_NODE_H

#include <opencog/atoms/base/Node.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

class NameNode : public Node
{
public:
	NameNode(Type, const std::string&&);
public:
	NameNode(const std::string&&);
	NameNode(const NameNode&) = delete;
	NameNode& operator=(const NameNode&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(NameNode)
#define createNameNode CREATE_DECL(NameNode)

/** @}*/
}

#endif // _OPENCOG_NAME_NODE_H
