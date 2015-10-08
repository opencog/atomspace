/*
 * ForAllLink.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Nil Geisweiller
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/LambdaLink.h>

#include "ForAllLink.h"

using namespace opencog;

ForAllLink::ForAllLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: LambdaLink(FORALL_LINK, oset, tv, av)
{
	ScopeLink::init();
}

ForAllLink::ForAllLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: LambdaLink(FORALL_LINK, HandleSeq({vars, body}), tv, av)
{
	ScopeLink::init();
}

ForAllLink::ForAllLink(Type t, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: LambdaLink(t, HandleSeq({body}), tv, av)
{
	ScopeLink::init();
}

ForAllLink::ForAllLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: LambdaLink(t, oset, tv, av)
{
	ScopeLink::init();
}

ForAllLink::ForAllLink(Link &l)
	: LambdaLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, FORALL_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ForAllLink, got %s", tname.c_str());
	}

	ScopeLink::init();
}
