/*
 * JoinLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
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

#include <opencog/atoms/atom_types/NameServer.h>

#include "JoinLink.h"

using namespace opencog;

void JoinLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, JOIN_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a JoinLink, got %s", tname.c_str());
	}
}

JoinLink::JoinLink(const HandleSeq&& hseq, Type t)
	: PrenexLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */

HandleSet JoinLink::min_container(bool silent)
{
	Handle starter;

	// If there's only one variable, things should be easy...
	if (_variables.varseq.size() == 1)
	{
		Handle var(_variables.varseq[0]);

		if (_variables._simple_typemap.size() != 0)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		// Get the type.
		HandleSet dtset = _variables._deep_typemap.at(var);
		if (dtset.size() != 1)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		Handle dt = *dtset.begin();
		Type dtype = dt->get_type();

		if (SIGNATURE_LINK != dtype)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		starter = dt->getOutgoingAtom(0);

		_replacements.insert({starter, var});
	}

	if (_variables.varseq.size() != 1)
		throw RuntimeException(TRACE_INFO, "Not supported yet!");

	HandleSet containers;
	containers.insert(starter);

	return containers;
}

/* ================================================================= */

/// find_top() - walk upwards from h and insert topmost atoms into set.
/// This recursively walks to the top, till there is no more.
void JoinLink::find_top(HandleSet& containers, const Handle& h) const
{
	// Ignore other containers!
	if (nameserver().isA(h->get_type(), JOIN_LINK))
		return;

	IncomingSet is(h->getIncomingSet());
	if (0 == is.size())
	{
		containers.insert(h);
		return;
	}

	for (const Handle& ih: is)
	{
		find_top(containers, ih);
	}
}

/* ================================================================= */

HandleSet JoinLink::max_container(bool silent)
{
	HandleSet hs = min_container(silent);
	HandleSet containers;
	for (const Handle& h: hs)
	{
		find_top(containers, h);
	}

	// FreeVariables::substitute_scoped() uses a weird API.
	// Create the two things that API wants.
	HandleSeq to_insert;
	FreeVariables::IndexMap insert_index;
	size_t idx = 0;
	for (const auto& pr : _replacements)
	{
		to_insert.push_back(pr.second);
		insert_index.insert({pr.first, idx});
		idx++;
	}

	// Use the FreeVariables utility, so that all scoping and
	// quoting is handled correctly.
	HandleSet replaced;
	for (const Handle& top: containers)
	{
		Handle rep = FreeVariables::substitute_scoped(top,
		                        to_insert, silent, insert_index);
		replaced.insert(rep);
	}

	return replaced;
}

/* ================================================================= */

QueueValuePtr JoinLink::do_execute(AtomSpace* as, bool silent)
{
	// if (nullptr == as) as = _atom_space;
	QueueValuePtr qvp(createQueueValue());

printf("duude vardecls=%s\n", _vardecl->to_string().c_str());
printf("duude body=%s\n", _body->to_string().c_str());
printf("duude vars=%s\n", oc_to_string(_variables).c_str());

	HandleSet hs = max_container(silent);

	// XXX FIXME this is really dumb, using a queue and then
	// copying things into it. Whatever. Fix this.
	for (const Handle& h : hs)
	{
		qvp->push(h);
	}

	qvp->close();
	return qvp;
}

ValuePtr JoinLink::execute(AtomSpace* as, bool silent)
{
	return do_execute(as, silent);
}

DEFINE_LINK_FACTORY(JoinLink, JOIN_LINK)

/* ===================== END OF FILE ===================== */
