/*
 * opencog/atoms/execution/GroundedPredicateNode.cc
 *
 * Copyright (C) 2009, 2013, 2015, 2020 Linas Vepstas
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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/execution/Force.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include <opencog/atoms/grounded/GroundedPredicateNode.h>
#include "DLScheme.h"
#include "LibraryManager.h"


using namespace opencog;

GroundedPredicateNode::GroundedPredicateNode(std::string s)
	: GroundedProcedureNode(GROUNDED_PREDICATE_NODE, std::move(s))
{
}

GroundedPredicateNode::GroundedPredicateNode(Type t, std::string s)
	: GroundedProcedureNode(t, std::move(s))
{
	if (not nameserver().isA(t, GROUNDED_PREDICATE_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GroundedProcedureNode, got %s", tname.c_str());
	}
}

ValuePtr GroundedPredicateNode::execute(AtomSpace* as,
                                        const Handle& cargs,
                                        bool silent)
{
	return nullptr;
}

DEFINE_NODE_FACTORY(GroundedPredicateNode, GROUNDED_PREDICATE_NODE)
