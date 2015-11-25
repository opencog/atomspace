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

#include <math.h>
#include <time.h>
#include <sys/time.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/NumberNode.h>

#include "SleepLink.h"

using namespace opencog;

SleepLink::SleepLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(SLEEP_LINK, oset, tv, av)
{
	if (1 != oset.size())
		throw SyntaxException(TRACE_INFO,
			"SleepLink expects only one argument");
}

SleepLink::SleepLink(Link &l)
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
	NumberNodePtr nsle = NumberNodeCast(_outgoing[0]);
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

/* ===================== END OF FILE ===================== */
