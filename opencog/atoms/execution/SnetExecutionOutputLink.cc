/*
 * opencog/atoms/execution/SnetExecutionOutputLink.cc
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

#include <opencog/atoms/execution/GroundedFunctionLink.h>

#include "SnetExecutionOutputLink.h"

using namespace opencog;

SnetExecutionOutputLink::SnetExecutionOutputLink(const HandleSeq& oset, Type t)
	: ExecutionOutputLink(oset, t)
{
}

ValuePtr SnetExecutionOutputLink::execute(AtomSpace* as, bool silent)
{
	if (!nameserver().isA(get_schema()->get_type(), GROUNDED_FUNCTION_LINK))
		return ExecutionOutputLink::execute(as, silent);

	GroundedFunctionLinkPtr grounded_link = CastFromHandle<GroundedFunctionLink>(getOutgoingAtom(0));
	ValuePtr args = getOutgoingAtom(1);
	return grounded_link->get_function()(as, args);
}

auto SnetExecutionOutputLinkCast = CastFromHandle<SnetExecutionOutputLink>;

template<typename ... Args>
static inline SnetExecutionOutputLinkPtr createSnetExecutionOutputLink(Args&&... args)
{
	return std::make_shared<SnetExecutionOutputLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(SnetExecutionOutputLink, SNET_EXECUTION_OUTPUT_LINK)

