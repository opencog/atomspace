/*
 * MeetLink.cc
 *
 * Copyright (C) 2019 Linas Vepstas
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

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/query/Satisfier.h>

#include "MeetLink.h"

using namespace opencog;

void MeetLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, MEET_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a MeetLink, got %s", tname.c_str());
	}
}

MeetLink::MeetLink(const HandleSeq&& hseq, Type t)
	: PatternLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */

QueueValuePtr MeetLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;

	// Where shall we place results? Why, right here!
	ValuePtr vp(getValue(get_handle()));
	QueueValuePtr qvp(QueueValueCast(vp));
	if (nullptr == qvp)
		throw RuntimeException(TRACE_INFO,
			"Expecting QueueValue for results!");

	try
	{
		SatisfyingSet sater(as, qvp);
		sater.satisfy(PatternLinkCast(get_handle()));
		return qvp;
	}
	catch(const StandardException& ex)
	{
		std::string msg =
			"Exception during execution of pattern\n";
		msg += to_string();
		msg += "\nException was:\n";
		msg += ex.get_message();
		ex.set_message(msg.c_str());
		throw;
	}
}

ValuePtr MeetLink::execute(AtomSpace* as, bool silent)
{
	return do_execute(as, silent);
}

DEFINE_LINK_FACTORY(MeetLink, MEET_LINK)

/* ===================== END OF FILE ===================== */
