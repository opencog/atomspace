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
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>

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
	if (JOIN_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"JoinLinks are private and cannot be instantiated.");

	setup_variables();
	setup_replacements();
}

JoinLink::JoinLink(const HandleSeq&& hseq, Type t)
	: PrenexLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */

void JoinLink::setup_variables(void)
{
	for (const Handle& var : _variables.varseq)
	{
		if (_variables._simple_typemap.size() != 0)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		// Get the type.
		HandleSet dtset = _variables._deep_typemap.at(var);
		if (dtset.size() != 1)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		Handle deet = *dtset.begin();
		Type dtype = deet->get_type();

		if (SIGNATURE_LINK != dtype)
			throw RuntimeException(TRACE_INFO, "Not supported yet!");

		Handle starter = deet->getOutgoingAtom(0);
		_replacements.insert({starter, var});
	}
}

/* ================================================================= */

/// Scan for ReplacementLinks in the body of the JoinLink.
/// Each of these should have a corresponding variable declaration.
/// Update the replacement map so that the "from" part of the variable
/// (obtained from the signature) gets replaced by the ... replacement.
void JoinLink::setup_replacements(void)
{
	for (size_t i=1; i<_outgoing.size(); i++)
	{
		const Handle& h(_outgoing[i]);
		if (h->get_type() != REPLACEMENT_LINK) continue;
		if (h->get_arity() != 2)
			throw SyntaxException(TRACE_INFO,
				"ReplacementLink expecting two arguments, got %s",
				h->to_short_string().c_str());

		const Handle& from(h->getOutgoingAtom(0));
		bool found = false;
		for (const auto& pr : _replacements)
		{
			if (pr.second != from) continue;
			_replacements[pr.first] = h->getOutgoingAtom(1);
			found = true;
			break;
		}

		if (not found)
			throw SyntaxException(TRACE_INFO,
				"No matching variable declaration for: %s",
				h->to_short_string().c_str());
	}
}

/* ================================================================= */

HandleSet JoinLink::min_container(bool silent)
{
	if (_replacements.size() != 1)
		throw RuntimeException(TRACE_INFO, "Not supported yet!");

	Handle starter = _replacements.begin()->first;

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

	return containers;
}

/* ================================================================= */

/// Given a top-level set of containing links, perform
/// replacements, substituting the bottom-most atoms as requested,
/// while honoring all scoping and quoting.
HandleSet JoinLink::replace(const HandleSet& containers, bool silent) const
{
	// Use the FreeVariables utility, so that all scoping and
	// quoting is handled correctly.
	HandleSet replaced;
	for (const Handle& top: containers)
	{
		Handle rep = FreeVariables::replace_nocheck(top, _replacements);
		replaced.insert(rep);
	}

	return replaced;
}

/* ================================================================= */

QueueValuePtr JoinLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;
	QueueValuePtr qvp(createQueueValue());

printf("duude vars=%s\n", oc_to_string(_variables).c_str());

	HandleSet hs;
	if (MAXIMAL_JOIN_LINK == get_type())
		hs = max_container(silent);
	else
		hs = min_container(silent);

	hs = replace(hs, silent);

	// XXX FIXME this is really dumb, using a queue and then
	// copying things into it. Whatever. Fix this.
	for (const Handle& h : hs)
	{
		qvp->push(as->add_atom(h));
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
