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

#include <algorithm>
#include <iterator>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/value/LinkValue.h>
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

	validate();
	setup_meets();
}

JoinLink::JoinLink(const HandleSeq&& hseq, Type t)
	: PrenexLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */

/// Temporary scaffolding to validate what we can do, so far.
void JoinLink::validate(void)
{
}

/* ================================================================= */

/// setup_meets() -- create a search that can find the all of
/// the locations that will be joined together.
void JoinLink::setup_meets(void)
{
	HandleSet done;
	for (size_t i=1; i<_outgoing.size(); i++)
	{
		const Handle& clause(_outgoing[i]);

		// Create a mandatory clause for each PresentLink
		if (clause->get_type() != PRESENT_LINK) continue;

		// Find the variables in the clause
		FreeVariables fv;
		fv.find_variables(clause);
		_mandatory.insert({clause, fv.varset});

		// Create a MeetLink for each mandatory clause.
		setup_clause(clause, fv.varset);

		done.merge(fv.varset);
	}

	// Are there any variables that are NOT in a PresentLink? If so,
	// then create a PresentLink for each; we need that to get started.
	for (const Handle& var: _variables.varseq)
	{
		if (done.find(var) != done.end()) continue;
		Handle pres(createLink(PRESENT_LINK, var));
		_mandatory.insert({pres, {var}});
		setup_clause(pres, {var});
	}
}

/* ================================================================= */

/// Given one PresentLink in the body of the JoinLink, create
/// a map with all of the variables that appear in it, and create
/// a MeetLink that can be used to find the atoms to be joined.
///
void JoinLink::setup_clause(const Handle& clause,
                            const HandleSet& varset)
{
	// Build a Meet
	HandleSeq vardecls;
	for (const Handle& var : varset)
	{
		Handle typedecl(_variables.get_type_decl(var, var));
		vardecls.emplace_back(typedecl);
	}

	Handle hdecls(createLink(std::move(vardecls), VARIABLE_LIST));
	Handle meet(createLink(MEET_LINK, hdecls, clause));
	_meets.insert({clause, meet});
}

/* ================================================================= */

/// Scan for ReplacementLinks in the body of the JoinLink.
/// Each of these should have a corresponding variable declaration.
/// Update the replacement map so that the "from" part of the variable
/// (obtained from the signature) gets replaced by the ... replacement.
void JoinLink::fixup_replacements(HandleMap& replace_map) const
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
		for (const auto& pr : replace_map)
		{
			if (pr.second != from) continue;
			replace_map[pr.first] = h->getOutgoingAtom(1);
			found = true;
		}

		if (not found)
			throw SyntaxException(TRACE_INFO,
				"No matching variable declaration for: %s",
				h->to_short_string().c_str());
	}
}

/* ================================================================= */

/// Given one mandatory-presence term in the body of the JoinLink,
/// obtain every atom that satisfies the type constraints it specifies.
/// Its a map because ... argh ... this will change/needs refactoring
/// maybe, depending on the structure of the presence term.
/// This returns a "replacement map" - a map of pairs, from a
/// concrete atom in the atomspace, to the variable in the
/// the PresentLink. For example, suppose that
///
///    (Join
///       (TypedVariable (Variable "X")
///          (Signature (Member (Variable "X") (Concept "beach"))))
///       (Present (Variable "X"))
///
/// and that
///
///    (Member (Concept "sea") (Concept "beach"))
///    (Member (Concept "sand") (Concept "beach"))
///
/// then the returned HandleMap will have two pairs, one for each of
/// of these MemberLinks (as the first elt of the pair) and will have
/// `(Variable "X")` as the second elt of both pairs.
///
/// This is the formally speaking the "supremum" of the presence term:
/// its the smallest set of atoms that satisfy the constraints...
///
HandleMap JoinLink::supremum_map(AtomSpace* as, const Handle& clause) const
{
	const bool TRANSIENT_SPACE = true;

	Handle meet = _meets.at(clause);
	AtomSpace temp(as, TRANSIENT_SPACE);
	meet = temp.add_atom(meet);
	ValuePtr vp = meet->execute();

	// The MeetLink returned everything that the variables in the
	// clause could ever be...
	const HandleSet& varset(_mandatory.at(clause));
	if (1 != varset.size())
		throw RuntimeException(TRACE_INFO, "Not supported yet!");
	const Handle& var(*varset.begin());

	HandleMap replace_map;
	for (const Handle& hst : LinkValueCast(vp)->to_handle_seq())
	{
		replace_map.insert({hst, var});
	}
	return replace_map;
}

/* ================================================================= */

/// get_principal_filter() - Get everything that contains `h`.
/// This is the "principal filter" on the "principal element" `h`.
/// Algorithmically: walk upwards from h and insert everything in
/// it's  incoming tree into the handle-set. This recursively walks to
/// the top, till there is no more. Of course, this can get large.
void JoinLink::get_principal_filter(HandleSet& containers,
                                    const Handle& h) const
{
	// Ignore type sepcifications, other containers!
	if (nameserver().isA(h->get_type(), TYPE_OUTPUT_LINK) or
	    nameserver().isA(h->get_type(), JOIN_LINK))
		return;

	IncomingSet is(h->getIncomingSet());
	containers.insert(h);

	for (const Handle& ih: is)
		get_principal_filter(containers, ih);
}

/* ================================================================= */

/// Compute the upper set -- the intersection of all of the
/// principal filters for each clause. The replacements are
/// collected up as well.
HandleSet JoinLink::upper_set(AtomSpace* as, bool silent,
                              HandleMap& replace_map) const
{
	// Insect will hold the intersection of all the supremums.
	HandleSet insect;
	bool first_time = true;
	for (const auto& memb : _mandatory)
	{
		// For a single presence-clause, find the supremum.
		// The "smallest" atoms that satisfy the type constraints.
		const Handle& h(memb.first);
		HandleMap start_map(supremum_map(as, h));

		// Get all atoms above the supremum.
		// Record the re-map, as needed.
		// XXX right now only single-variable presence clauses
		// are supported, there is a throw otherwise.
		// I think this is buggy when ther are two variables...
		// so clarify and fix...
		HandleSet containers;
		for (const auto& pr: start_map)
		{
			get_principal_filter(containers, pr.first);
			replace_map.insert(pr);
		}

		// First time only...
		if (first_time)
		{
			first_time = false;
			insect.swap(containers);
			continue;
		}

		// Not the first time. Perform set intersection.
		HandleSet smaller;
		std::set_intersection(insect.begin(), insect.end(),
		                      containers.begin(), containers.end(),
		                      std::inserter(smaller, smaller.begin()));
		insect.swap(smaller);
	}

	return insect;
}

/* ================================================================= */

/// Compute the supremum of the upper set -- the smallest set of
/// elements that aren't contained in any other elements.
HandleSet JoinLink::supremum(AtomSpace* as, bool silent,
                             HandleMap& replace_map) const
{
	if (_mandatory.size() == 1)
		return supr_one(as, silent, replace_map);

	HandleSet upset = upper_set(as, silent, replace_map);

	HandleSet non_minimal;
	for (const Handle& h : upset)
	{
		if (h->is_node()) continue;
		for (const Handle& ho : h->getOutgoingSet())
		{
			if (upset.find(ho) != upset.end())
			{
				non_minimal.insert(h);
				break;
			}
		}
	}
	HandleSet minimal;
	std::set_difference(upset.begin(), upset.end(),
	                    non_minimal.begin(), non_minimal.end(),
	                    std::inserter(minimal, minimal.begin()));
	return minimal;
}

/* ================================================================= */

HandleSet JoinLink::supr_one(AtomSpace* as, bool silent,
                             HandleMap& replace_map) const
{
	OC_ASSERT(_mandatory.size() == 1);

	const Handle& h(_mandatory.begin()->first);
	HandleMap start_map(supremum_map(as, h));

	HandleSet containers;
	for (const auto& pr: start_map)
	{
		containers.insert(pr.first);
		replace_map.insert(pr);
	}
	return containers;
}
/* ================================================================= */

HandleSet JoinLink::min_container(AtomSpace* as, bool silent,
                                  HandleMap& replace_map) const
{
	HandleSet containers(supremum(as, silent, replace_map));
	fixup_replacements(replace_map);
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

HandleSet JoinLink::max_container(AtomSpace* as, bool silent,
                                  HandleMap& replace_map) const
{
	HandleSet hs = min_container(as, silent, replace_map);
	HandleSet containers;
	for (const Handle& h: hs)
		find_top(containers, h);

	return containers;
}

/* ================================================================= */

/// Given a top-level set of containing links, perform
/// replacements, substituting the bottom-most atoms as requested,
/// while honoring all scoping and quoting.
HandleSet JoinLink::replace(const HandleSet& containers,
                            const HandleMap& replace_map) const
{
	// Use the FreeVariables utility, so that all scoping and
	// quoting is handled correctly.
	HandleSet replaced;
	for (const Handle& top: containers)
	{
		Handle rep = FreeVariables::replace_nocheck(top, replace_map);
		replaced.insert(rep);
	}

	return replaced;
}

/* ================================================================= */

QueueValuePtr JoinLink::do_execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;
	QueueValuePtr qvp(createQueueValue());

// printf("duude vars=%s\n", oc_to_string(_variables).c_str());

	HandleMap replace_map;
	HandleSet hs;
	if (MAXIMAL_JOIN_LINK == get_type())
		hs = max_container(as, silent, replace_map);
	else
		hs = min_container(as, silent, replace_map);

	hs = replace(hs, replace_map);

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
