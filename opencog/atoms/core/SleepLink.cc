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

#include <opencog/atoms/NumberNode.h>
#include <opencog/atomspace/AtomSpace.h>

#include "SleepLink.h"

using namespace opencog;

SleepLink::SleepLink(const HandleSeq& oset, Type t)
	: FunctionLink(oset, t)
{
	// Type must be as expected
	if (not classserver().isA(t, SLEEP_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SleepLink, got %s", tname.c_str());
	}

	if (1 != oset.size())
		throw SyntaxException(TRACE_INFO,
			"SleepLink expects only one argument");

	Type tf = oset[0]->getType();
	if (NUMBER_NODE != tf and classserver().isA(tf, FUNCTION_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting a NumberNode or something that returns a NumberNode");
}

SleepLink::SleepLink(const Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, SLEEP_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SleepLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

Handle SleepLink::execute(AtomSpace * as) const
{
	Handle time(_outgoing[0]);
	FunctionLinkPtr flp(FunctionLinkCast(time));
	if (flp)
		time = flp->execute();

	NumberNodePtr nsle = NumberNodeCast(time);
	if (nullptr == nsle)
		throw RuntimeException(TRACE_INFO,
			"Expecting an NumberNode, got %s",
				(nullptr == time) ? "<invalid handle>" :
					classserver().getTypeName(time->getType()).c_str());

	double length = nsle->get_value();
	unsigned int secs = floor(length);
	useconds_t usec = 1000000 * (length - secs);
	
	secs = sleep(secs);
	if (0 == secs)
		usleep (usec);

	// XXX This is probably wrong ... if the as is null, we should
	// probably use the atomspace that this link is in, right?
	// We need to make a decision here and in many other places...
	if (NULL == as)
		return Handle(createNumberNode(secs));

	return as->add_atom(createNumberNode(secs));
}

DEFINE_LINK_FACTORY(SleepLink, SLEEP_LINK)

/* ===================== END OF FILE ===================== */
