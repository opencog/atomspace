/*
 * SleepLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <math.h>
#include <time.h>
#include <sys/time.h>

#include <opencog/atoms/core/NumberNode.h>

#include "SleepLink.h"

using namespace opencog;

SleepLink::SleepLink(const HandleSeq& oset, Type t)
	: FunctionLink(oset, t)
{
	// Type must be as expected
	if (not nameserver().isA(t, SLEEP_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SleepLink, got %s", tname.c_str());
	}

	if (1 != oset.size())
		throw SyntaxException(TRACE_INFO,
			"SleepLink expects only one argument");

	Type tf = oset[0]->get_type();
	if (NUMBER_NODE != tf and nameserver().isA(tf, FUNCTION_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting a NumberNode or something that returns a NumberNode");
}

SleepLink::SleepLink(const Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.get_type();
	if (not nameserver().isA(tscope, SLEEP_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SleepLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return number of seconds left to sleep.
/// Normally, this is zero, unless the sleep was interrupted.
ProtoAtomPtr SleepLink::execute() const
{
	Handle time(_outgoing[0]);
	FunctionLinkPtr flp(FunctionLinkCast(time));
	if (flp)
		time = HandleCast(flp->execute());

	NumberNodePtr nsle = NumberNodeCast(time);
	if (nullptr == nsle)
		throw RuntimeException(TRACE_INFO,
			"Expecting an NumberNode, got %s",
				(nullptr == time) ? "<invalid handle>" :
					nameserver().getTypeName(time->get_type()).c_str());

	double length = nsle->get_value();
	unsigned int secs = floor(length);
	useconds_t usec = 1000000 * (length - secs);
	
	secs = sleep(secs);
	if (0 == secs)
		usleep (usec);

	return ProtoAtomPtr(createNumberNode(secs));
}

DEFINE_LINK_FACTORY(SleepLink, SLEEP_LINK)

/* ===================== END OF FILE ===================== */
