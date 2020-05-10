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
		_mandatory.insert({clause, fv.varseq});

		// Create a MeetLink for each mandatory clause.
		setup_clause(clause, fv.varseq);

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
                            const HandleSeq& varseq)
{
	// Build a Meet
	HandleSeq vardecls;
	for (const Handle& var : varseq)
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
/// obtain every atom that satisfies it, including all relevant type
/// constraints it specifies. Return a "replacement map", which
/// pairs up atoms in the atomspace with variables in the PresentLink.
///
/// An example: one is looking for MemberLinks:
///
///    (Join
///       (VariableList
///          (TypedVariable (Variable "X") (Type 'ConceptNode))
///          (TypedVariable (Variable "Y") (Type 'ConceptNode)))
///       (Present (Member (Variable "X") (Variable "Y"))))
///
/// and that the atomspace contains:
///
///    (Member (Concept "sea") (Concept "beach"))
///    (Member (Concept "sand") (Concept "beach"))
///
/// then the returned HandleMap will have three pairs, of the form
///
///    { (Concept "sea"),   (Variable "X") }
///    { (Concept "sand"),  (Variable "X") }
///    { (Concept "beach"), (Variable "Y") }
///
/// This is the "supremum" of the PresentLink: it is the smallest set of
/// atoms that satisfy the constraints given by the PresentLink and the
/// type constraints on the variables. The returned elements are the
/// "principal elements" from which the "principal filters" will be
/// constructed.
///
/// Explained a different way: this performs a wild-card search, to find
/// all of the principal elements specified by the wild-cards.
///
HandleMap JoinLink::principal_map(AtomSpace* as, const Handle& clause) const
{
	const bool TRANSIENT_SPACE = true;

	Handle meet = _meets.at(clause);
	AtomSpace temp(as, TRANSIENT_SPACE);
	meet = temp.add_atom(meet);
	ValuePtr vp = meet->execute();

	// The MeetLink returned everything that the variables in the
	// clause could ever be...
	const HandleSeq& varseq(_mandatory.at(clause));
	size_t vsize = varseq.size();
	if (1 == vsize)
	{
		const Handle& var(varseq[0]);
		HandleMap replace_map;
		for (const Handle& hst : LinkValueCast(vp)->to_handle_seq())
			replace_map.insert({hst, var});
		return replace_map;
	}

	// If we are here, then the MeetLink has returned a collection
	// of ListLinks, holding the variable values in the lists.
	HandleMap replace_map;
	for (const Handle& hst : LinkValueCast(vp)->to_handle_seq())
	{
		const HandleSeq& glist(hst->getOutgoingSet());
		for (size_t i=0; i<vsize; i++)
			replace_map.insert({glist[i], varseq[i]});
	}
	return replace_map;
}

/* ================================================================= */

/// principal_filter() - Get everything that contains `h`.
/// This is the "principal filter" on the "principal element" `h`.
/// Algorithmically: walk upwards from h and insert everything in
/// it's  incoming tree into the handle-set. This recursively walks to
/// the top, till there is no more. Of course, this can get large.
void JoinLink::principal_filter(HandleSet& containers,
                                    const Handle& h) const
{
	// Ignore type sepcifications, other containers!
	if (nameserver().isA(h->get_type(), TYPE_OUTPUT_LINK) or
	    nameserver().isA(h->get_type(), JOIN_LINK))
		return;

	IncomingSet is(h->getIncomingSet());
	containers.insert(h);

	for (const Handle& ih: is)
		principal_filter(containers, ih);
}

/* ================================================================= */

/// Compute the upper set -- the intersection of all of the principal
/// filters for each mandatory clause.  This is a three-step process.
///
/// (1) Each mandatory clause is a PresentLink, with type constraints.
///     It defines a wild-card search on the AtomSpace, and the results
///     of that search is a set of principal elements. (See
///     `principal_map()` above, for details).
/// (2) For each principal element, obtain the corresponding "principal
///     filter".  This is the set of all elements above the given
///     principal element, "above" in the sense of "lies in the incoming
///     tree". (See `principal_filter()` above for details.)
///     Thus, each mandatory clause generates a set of principal filters.
///     Union these all together; the union of filters is still a
///     filter.
/// (3) Intersect all of the filters, to obtain an upper set.
///
/// That's a mouthful, so lets unpack it.
/// (1) For each mandatory clause c_i, obtain a set of principal
///     elements {p_i}. If there are n clauses, this forms a Cartesian
///     product ({p_1}, ... , {p_n}).
/// (2) For each p_ij in {p_i} construct the corresponding filter f_ij.
///     Compute the union filter f_i = f_i1 v f_i2 v ... v f_in
///     This forms a Cartesian product (f_1, ... , f_n)
/// (3) Compute the intersection f_1 & ... & f_n. This intersection
///     is an upper set, because it contains one element from each
///     of the {p_i}.
///
/// I think this is technically a pushout (fibered sum, co-cartesian
/// square). (Not quite sure, I'm doing this in my head...) So...
/// Start with the JoinLink .. call this "Z". Suppose it has two
/// clauses, call them X and Y, so this is a "span" (category theory).
/// The morphisms compute the union-filters. The co-span is the
/// intersection ... is a disjoint union of containers of tuples ...
/// Ick. That's roughly the idea. Caveat emptor. The code works.
///
/// See wikipedia for definitions of "upper set", "principal filter",
/// "principal element", "disjoint union" and "pushout". Or just read
/// the code -- the code is easier to understand.
///
HandleSet JoinLink::upper_set(AtomSpace* as, bool silent,
                              HandleMap& replace_map) const
{
	// Insect will hold the intersection of all the supremums.
	HandleSet insect;
	bool first_time = true;
	for (const auto& memb : _mandatory)
	{
		// For a single PresentLink clause, find the set of
		// principal elements. These are the "smallest" atoms
		// that satisfy the PresentLink constraints and the type
		// constraints.
		const Handle& h(memb.first);
		HandleMap start_map(principal_map(as, h));

		// Get a principal filter for each principal element,
		// and union all of them together.
		HandleSet containers;
		for (const auto& pr: start_map)
		{
			principal_filter(containers, pr.first);
			replace_map.insert(pr);
		}

		// Starting filter of the sequence, first time only...
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

/// Return the supremum of all the clauses. If there is only one
/// clause, it's easy, just get the set of principal elements for
/// that one clause, and we are done. If there is more than one
/// clause, then it's harder: we have to:
///
/// (1) Get the principal elements for each clause.
/// (2) Get the principal filters for each principal element.
/// (3) Intersect the filters to get the upper set of the clauses.
/// (4) Remove all elements that are not minimal.
///
/// The general concern here is that this algo is inefficient, but
/// I cannot think of any better way of doing it. In particular,
/// walking to the top for step (2) seems unavoidable, and I cannot
/// think of any way of combining steps (2) and (3) that would avoid
/// step (4) ... or even would reduce the work for stpe (4). Oh well.
///
/// TODO: it might be faster to use hash tables instead of rb-trees
/// i.e. to use UnorderedHandleSet instead of HandleSet. XXX FIXME.
HandleSet JoinLink::supremum(AtomSpace* as, bool silent,
                             HandleMap& replace_map) const
{
	// If there is only one clause, we do not have to get
	// upper sets, and trim them back down. Avoid extra work.
	if (_mandatory.size() == 1)
		return supr_one(as, silent, replace_map);

	HandleSet upset = upper_set(as, silent, replace_map);

	// Create a set of non-minimal elements.
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

	// Remove the non-minimal elements.
	HandleSet minimal;
	std::set_difference(upset.begin(), upset.end(),
	                    non_minimal.begin(), non_minimal.end(),
	                    std::inserter(minimal, minimal.begin()));
	return minimal;
}

/* ================================================================= */

/// If there is only one clause, then the supremum is just the
/// principal element for that clause. Special case, for speed.
HandleSet JoinLink::supr_one(AtomSpace* as, bool silent,
                             HandleMap& replace_map) const
{
	OC_ASSERT(_mandatory.size() == 1);

	const Handle& h(_mandatory.begin()->first);
	HandleMap start_map(principal_map(as, h));

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

/// find_top() - walk upwards from `h` and insert topmost atoms into
/// the container set.  This recursively walks to the top, until there
/// is nothing more above.
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
		find_top(containers, ih);
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

	HandleMap replace_map;
	HandleSet hs;
	if (MAXIMAL_JOIN_LINK == get_type())
		hs = max_container(as, silent, replace_map);
	else
		hs = min_container(as, silent, replace_map);

	// Perform the actual rewriting.
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
