/*
 * TimeLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <time.h>
#include <sys/time.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/NumberNode.h>

#include "TimeLink.h"

using namespace opencog;

TimeLink::TimeLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(TIME_LINK, oset, tv, av)
{
	if (0 < oset.size())
		throw SyntaxException(TRACE_INFO,
			"TimeLink does not expect any arguments");
}

TimeLink::TimeLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, TIME_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an TimeLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

Handle TimeLink::execute(AtomSpace * as) const
{
	// time_t now = time(nullptr);
	struct timeval tv;
	gettimeofday(&tv, nullptr);
	double now = tv.tv_sec + 1.0e-6 * tv.tv_usec;

	// XXX This is probably wrong ... if the as is null, we should
	// probably use the atomspace that this link is in, right?
	// We need to make a decision here and in many other places...
	if (NULL == as)
		return Handle(createNumberNode(now));

	return as->add_atom(createNumberNode(now));
}

/* ===================== END OF FILE ===================== */
