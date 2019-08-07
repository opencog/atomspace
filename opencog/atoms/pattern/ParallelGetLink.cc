/*
 * ParallelGetLink.cc
 *
 * Copyright (C) 2019 Linas Vepstas
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

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/query/Satisfier.h>

#include "ParallelGetLink.h"

using namespace opencog;

void ParallelGetLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, PARALLEL_GET_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ParallelGetLink, got %s", tname.c_str());
	}
}

ParallelGetLink::ParallelGetLink(const HandleSeq& hseq, Type t)
	: PatternLink(hseq, t)
{
	init();
}

ParallelGetLink::ParallelGetLink(const Link &l)
	: PatternLink(l)
{
	init();
}

/* ================================================================= */

HandleSet ParallelGetLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;

	SatisfyingSet sater(as);
	this->satisfy(sater);

	return sater._satisfying_set;
}

ValuePtr ParallelGetLink::execute(AtomSpace* as, bool silent)
{

	HandleSet handle_set = do_execute(as, silent);
	Handle satset(createUnorderedLink(handle_set, SET_LINK));

	for (auto h: handle_set)
	{
		HandleSeq handle_seq;
		handle_seq.push_back(h);
		handle_seq.push_back(_target);
		Handle member_link = createLink(handle_seq, MEMBER_LINK);

#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
		// Shoot. XXX FIXME. Most of the unit tests require that the atom
		// that we return is in the atomspace. But it would be nice if we
		// could defer this indefinitely, until its really needed.
		if (as) member_link = as->add_atom(member_link);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */
	}

	return _target;
}

DEFINE_LINK_FACTORY(ParallelGetLink, PARALLEL_GET_LINK)

/* ===================== END OF FILE ===================== */
