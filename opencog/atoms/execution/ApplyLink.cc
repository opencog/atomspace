/*
 * opencog/atoms/execution/ApplyLink.cc
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

#include "ApplyLink.h"
#include "Force.h"

using namespace opencog;

ApplyLink::ApplyLink(const HandleSeq& oset, Type t)
	: ExecutionOutputLink(oset, t)
{
	forward_to_execution_output_link =
		!nameserver().isA(get_schema()->get_type(), GROUNDED_FUNCTION_LINK);
	if (!forward_to_execution_output_link)
	{
		check_outgoing_type(0, GROUNDED_FUNCTION_LINK);
		check_outgoing_type(1, LIST_LINK);
	}
}

ValuePtr ApplyLink::execute(AtomSpace* as, bool silent)
{
	if (forward_to_execution_output_link)
		return ExecutionOutputLink::execute(as, silent);

	GroundedFunctionLinkPtr grounded_link =
		CastFromHandle<GroundedFunctionLink>(getOutgoingAtom(0));
	Handle args = getOutgoingAtom(1);
	args = opencog::force_execute(as, args, silent);
	return grounded_link->get_function()(as, args);
}

auto ApplyLinkCast = CastFromHandle<ApplyLink>;

template<typename ... Args>
static inline ApplyLinkPtr createApplyLink(Args&&... args)
{
	return std::make_shared<ApplyLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(ApplyLink, APPLY_LINK)

