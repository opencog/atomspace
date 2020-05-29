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

SleepLink::SleepLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
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

// ---------------------------------------------------------------

/// Return number of seconds left to sleep.
/// Normally, this is zero, unless the sleep was interrupted.
ValuePtr SleepLink::execute(AtomSpace*as, bool silent)
{
	// Try to come up with a number, either from executing, or directly
	Handle time(_outgoing.at(0));
	double length = 0.0;
	if (time->is_executable())
	{
		ValuePtr vp = time->execute(as, silent);
		if (vp->is_atom())
		{
			time = HandleCast(vp);
		}
		else if (nameserver().isA(vp->get_type(), FLOAT_VALUE))
		{
			time = nullptr;
			length = FloatValueCast(vp)->value().at(0);
		}
	}

	if (time)
	{
		NumberNodePtr nsle = NumberNodeCast(time);
		if (nullptr == nsle)
			throw RuntimeException(TRACE_INFO,
				"Expecting a number, got %s",
					(nullptr == _outgoing.at(0)) ? "<invalid handle>" :
						_outgoing.at(0)->to_string().c_str());

		length = nsle->get_value();
	}
	unsigned int secs = floor(length);
	useconds_t usec = 1000000 * (length - secs);
	
	secs = sleep(secs);
	if (0 == secs)
		usleep (usec);

	return ValuePtr(createNumberNode(secs));
}

DEFINE_LINK_FACTORY(SleepLink, SLEEP_LINK)

/* ===================== END OF FILE ===================== */
