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
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Transient.h>

#include "JoinLink.h"

using namespace opencog;

class DefaultJoinCallback : public JoinCallback
{
	IncomingSet get_incoming_set(const Handle& h)
	{
		return h->getIncomingSet();
	}
};


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
	setup_top_clauses();
	setup_top_types();
	setup_meet();
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
	for (size_t i=0; i<_outgoing.size(); i++)
	{
		const Handle& clause(_outgoing[i]);
		Type t = clause->get_type();

		// Replacement links get special treatment, here.
		if (REPLACEMENT_LINK == t) continue;

		// Anything evaluatable goes into the MeetLink
		if (PRESENT_LINK == t) continue;
		if (clause->is_evaluatable()) continue;
		if (nameserver().isA(t, EVALUATABLE_LINK)) continue;

		// Type type nodes get applied to the container.
		if (nameserver().isA(t, TYPE_NODE)) continue;
		if (nameserver().isA(t, TYPE_INPUT_LINK)) continue;
		if (nameserver().isA(t, TYPE_OUTPUT_LINK)) continue;

		// Variable decls are allowed only in the first location.
		if (0 == i and nameserver().isA(t, VARIABLE_LIST)) continue;
		if (0 == i and nameserver().isA(t, VARIABLE_SET)) continue;
		if (0 == i and nameserver().isA(t, TYPED_VARIABLE_LINK)) continue;

		throw SyntaxException(TRACE_INFO, "Not supported (yet?) Got %s",
			clause->to_string().c_str());
	}
}

/* ================================================================= */

/// setup_meet() -- create a search that can find the all of
/// the locations that will be joined together.
void JoinLink::setup_meet(void)
{
	HandleSeq jclauses;
	HandleSet done;
	for (size_t i=0; i<_outgoing.size(); i++)
	{
		const Handle& clause(_outgoing[i]);

		// Clauses handled at the container level
		Type t = clause->get_type();
		if (REPLACEMENT_LINK == t) continue;
		if (nameserver().isA(t, TYPE_NODE)) continue;
		if (nameserver().isA(t, TYPE_INPUT_LINK)) continue;
		if (nameserver().isA(t, TYPE_OUTPUT_LINK)) continue;

		// If variable declarations are missing, then
		// we insist on the first link being a PresentLink
		if (i == 0 and not (PRESENT_LINK == t)) continue;

		// The top-var clauses cannot be passed to the meet.
		if (_top_var and is_free_in_tree(clause, _top_var)) continue;

		jclauses.push_back(clause);

		// Find the variables in the clause
		FreeVariables fv;
		fv.find_variables(clause);
		if (0 < fv.varset.size())
		{
			done.merge(fv.varset);
			continue;
		}

		// If we are here, there are no variables. Its a constant
		if (PRESENT_LINK != t)
			throw SyntaxException(TRACE_INFO,
				"Constant terms should be wrapped in a PresentLink, got %s",
				clause->to_short_string().c_str());

		_const_terms.insert(clause->getOutgoingAtom(0));
	}

	_vsize = _variables.varseq.size();
	if (_top_var) _vsize--;
	_jsize = _vsize + _const_terms.size();
	if (0 == _vsize) return;

	// Are there any variables that are NOT in some clause? If so,
	// then create a PresentLink for each.
	for (const Handle& var: _variables.varseq)
	{
		if (done.find(var) != done.end()) continue;
		if (var == _top_var) continue;
		Handle pres(createLink(PRESENT_LINK, var));
		jclauses.emplace_back(pres);
	}

	// Build a Meet
	HandleSeq vardecls;
	for (const Handle& var : _variables.varseq)
	{
		if (var == _top_var) continue;
		Handle typedecl(_variables.get_type_decl(var, var));
		vardecls.emplace_back(typedecl);
	}

	Handle hdecls(createLink(std::move(vardecls), VARIABLE_LIST));
	Handle hbody(createLink(std::move(jclauses), AND_LINK));
	_meet = createLink(MEET_LINK, hdecls, hbody);
}

/* ================================================================= */

/// Setup the top variable, if one is asked for, and
/// any constraints applied to it.
void JoinLink::setup_top_clauses(void)
{
	_need_top_map = false;

	// Search for a named top var.
	for (const Handle& var : _variables.varseq)
	{
		// If its anywhere, its in the simple typemap.
		const auto& vtyp = _variables._typemap.find(var);
		if (_variables._typemap.end() == vtyp) continue;

		// If it's specified, its a plain single type.
		const TypeSet& tset = vtyp->second->get_simple_typeset();
		if (tset.size() != 1) continue;

		// Its got to be JoinLink, or a derived type.
		Type vt = *(tset.begin());
		if (nameserver().isA(vt, JOIN_LINK))
		{
			_top_var = var;
			break;
		}
	}

	// If there is no top variable, we are done.
	if (nullptr == _top_var) return;

	// Find all the clauses that need/use the top variable.
	// In general, there should be at least one, but I suppose
	// that devious users could do something unexpected.
	for (size_t i=1; i<_outgoing.size(); i++)
	{
		const Handle& clause(_outgoing[i]);
		if (is_free_in_tree(clause, _top_var))
			_top_clauses.push_back(clause);
	}

	// Do any of thes clauses require any of the other variables
	// that were specified? If so then we have to burn a fair bit
	// of extra RAM to keep track of thier groundings. This is
	// unpleasant but unavoidable.
	for (const Handle& tclause : _top_clauses)
	{
		for (const Handle& var : _variables.varseq)
		{
			if (var == _top_var) continue;
			if (is_free_in_tree(tclause, var))
			{
				_need_top_map = true;
				break;
			}
		}
	}
}

/* ================================================================= */

/// Setup the type constraints that will be applied to the top.
void JoinLink::setup_top_types(void)
{
	for (size_t i=1; i<_outgoing.size(); i++)
	{
		const Handle& clause(_outgoing[i]);
		Type t = clause->get_type();

		// Type type nodes get applied to the container.
		if (nameserver().isA(t, TYPE_NODE) or
		    nameserver().isA(t, TYPE_INPUT_LINK) or
		    nameserver().isA(t, TYPE_OUTPUT_LINK))
		{
			_top_types.push_back(clause);
		}
	}
}

/* ================================================================= */

/// Scan for ReplacementLinks in the body of the JoinLink.
/// Each of these should have a corresponding variable declaration.
/// Update the replacement map so that the "from" part of the variable
/// (obtained from the signature) gets replaced by the ... replacement.
void JoinLink::fixup_replacements(Traverse& trav) const
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
		for (const auto& pr : trav.replace_map)
		{
			if (pr.second != from) continue;
			trav.replace_map[pr.first] = h->getOutgoingAtom(1);
			found = true;
		}

		if (not found)
			throw SyntaxException(TRACE_INFO,
				"No matching variable declaration for: %s",
				h->to_short_string().c_str());
	}
}

/* ================================================================= */

/// Given the JoinLink, obtain a set of atoms that lie "below" the join.
/// Below, in the sense that the join is guaranteed to be contained
/// in the incoming trees of these atoms. The minimal join (the
/// supremum) is guaranteed to be a slice through the above trees,
/// and its the minimal one that joins the atoms. Basically, we let
/// the pattern engine do all the hard work of checking for the
/// satisfiability of all the various clauses.
///
/// Explained a different way: this performs a wild-card search, to find
/// all of the principal elements specified by the wild-cards.
///
/// During construction, several maps are built. One is a "replacement
/// map", which is needed to perform substitution on the discovered
/// results.  It pairs up atoms in the atomspace with variables in
/// the JoinLink. Another map is the "join map"; it reverses the
/// pairing. It is needed to rule out unjoined responses from the
/// pattern engine.
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
/// then the replacement map will have three pairs, of the form
///
///    { (Concept "sea"),   (Variable "X") }
///    { (Concept "sand"),  (Variable "X") }
///    { (Concept "beach"), (Variable "Y") }
///
/// The join-map will contain
///
///    { (Variable "X"), { (Concept "sea"), (Concept "sand") }}
///    { (Variable "Y"), { (Concept "beach") }}
///
HandleSet JoinLink::principals(AtomSpace* as,
                               Traverse& trav) const
{
	// No variables, no search needed.
	if (0 == _vsize)
	{
		// Trivial replace-map.
		for (const Handle& h : _const_terms)
			trav.replace_map.insert({h, h});

		// Not quite trivial join map
		trav.join_map.resize(_jsize);
		size_t i=0;
		for (const Handle& hc : _const_terms)
		{
			trav.join_map[i].insert(hc);
			i++;
		}

		return _const_terms;
	}

	// If we are here, the expression had variables in it.
	// Perform a search to ground those.
	AtomSpace* temp = grab_transient_atomspace(as);
	Handle meet = temp->add_atom(_meet);
	ValuePtr vp = meet->execute();
	release_transient_atomspace(temp);

	// The MeetLink returned everything that the variables in the
	// clause could ever be...
	const HandleSeq& varseq(_variables.varseq);
	if (1 == _vsize)
	{
		const Handle& var(varseq[0]);
		HandleSet princes(_const_terms);
		for (const Handle& hst : LinkValueCast(vp)->to_handle_seq())
		{
			princes.insert(hst);
			trav.replace_map.insert({hst, var});
			if (_need_top_map) trav.top_map.insert({hst, {hst}});
		}

		// Place constants into the join map, first.
		trav.join_map.resize(_jsize);
		size_t i=0;
		for (const Handle& hc : _const_terms)
		{
			trav.join_map[i].insert(hc);
			i++;
		}
		trav.join_map[i] = princes;
		return princes;
	}

	// If we are here, then the MeetLink has returned a collection
	// of ListLinks, holding the variable values in the lists.

	// But first, deal with the constant terms
	HandleSet princes(_const_terms);
	trav.join_map.resize(_jsize);
	size_t n=0;
	for (const Handle& hc : _const_terms)
	{
		trav.join_map[n].insert(hc);
		n++;
	}

	for (const Handle& hst : LinkValueCast(vp)->to_handle_seq())
	{
		const HandleSeq& glist(hst->getOutgoingSet());
		for (size_t i=0; i<_vsize; i++)
		{
			princes.insert(glist[i]);
			trav.replace_map.insert({glist[i], varseq[i]});
			trav.join_map[n+i].insert(glist[i]);
			if (_need_top_map) trav.top_map.insert({glist[i], glist});
		}
	}
	return princes;
}

/* ================================================================= */

/// principal_filter() - Get everything that contains `h`.
/// This is the "principal filter" on the "principal element" `h`.
/// Algorithmically: walk upwards from h and insert everything in
/// it's incoming tree into the handle-set. This recursively walks to
/// the top, till there is no more. Of course, this can get large.
void JoinLink::principal_filter(Traverse& trav,
                                HandleSet& containers,
                                const Handle& h) const
{
	// Ignore type specifications, other containers!
	Type t = h->get_type();
	if (nameserver().isA(t, PRESENT_LINK) or
	    nameserver().isA(t, TYPE_OUTPUT_LINK) or
	    nameserver().isA(t, JOIN_LINK))
		return;

	containers.insert(h);

	IncomingSet is(trav.jcb->get_incoming_set(h));
	for (const Handle& ih: is)
		principal_filter(trav, containers, ih);
}

void JoinLink::principal_filter_map(Traverse& trav,
                                    const HandleSeq& base,
                                    HandleSet& containers,
                                    const Handle& h) const
{
	// Ignore type specifications, other containers!
	Type t = h->get_type();
	if (nameserver().isA(t, PRESENT_LINK) or
	    nameserver().isA(t, TYPE_OUTPUT_LINK) or
	    nameserver().isA(t, JOIN_LINK))
		return;

	containers.insert(h);
	trav.top_map.insert({h, base});

	IncomingSet is(trav.jcb->get_incoming_set(h));
	for (const Handle& ih: is)
		principal_filter_map(trav, base, containers, ih);
}

/* ================================================================= */

/// Compute the upper set -- the intersection of all of the principal
/// filters for each mandatory clause.
///
HandleSet JoinLink::upper_set(AtomSpace* as, bool silent,
                              Traverse& trav) const
{
	HandleSet princes(principals(as, trav));

	// Get a principal filter for each principal element,
	// and union all of them together.
	HandleSet containers;
	if (not _need_top_map)
	{
		for (const Handle& pr: princes)
			principal_filter(trav, containers, pr);
	}
	else
	{
		// Argh. This is complicated. Un-named, anonymous terms
		// are just like above.
		size_t ncon = _const_terms.size();
		for (size_t i=0; i<ncon; i++)
		{
			for (const Handle& prc: trav.join_map[i])
				principal_filter(trav, containers, prc);
		}

		// Named terms -- we need to build a lookup table,
		// so that we can pass them into any evaluatable predicates.
		HandleSeqMap base_map(trav.top_map);
		for (const auto& pare: base_map)
			principal_filter_map(trav, pare.second, containers, pare.first);
	}

	if (1 >= _jsize)
		return containers;

	// The meet link provided us with elements that are "too low",
	// fail to be joins. Remove them. Ther shouldn't be all that
	// many of them; it depends on how the join got written.
	// Well, this could be rather CPU intensive... there's a lot
	// of fishing going on here.
	//
	// So - two steps. First, create a set of unjoined elements.
	HandleSet unjoined;
	for (const Handle& h : containers)
	{
		for (size_t i=0; i<_jsize; i++)
		{
			if (not any_atom_in_tree(h, trav.join_map[i]))
			{
				unjoined.insert(h);
				break;
			}
		}
	}

	// and now banish them
	HandleSet joined;
	std::set_difference(containers.begin(), containers.end(),
	                    unjoined.begin(), unjoined.end(),
	                    std::inserter(joined, joined.begin()));
	return joined;
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
                             Traverse& trav) const
{
	HandleSet upset = upper_set(as, silent, trav);

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

/// find_top() - walk upwards from `h` and insert topmost atoms into
/// the container set.  This recursively walks to the top, until there
/// is nothing more above.
void JoinLink::find_top(Traverse& trav, const Handle& h) const
{
	// Ignore other containers!
	Type t = h->get_type();
	if (nameserver().isA(t, JOIN_LINK))
		return;

	IncomingSet is(trav.jcb->get_incoming_set(h));
	if (0 == is.size())
	{
		trav.containers.insert(h);
		return;
	}

	for (const Handle& ih: is)
		find_top(trav, ih);
}

/* ================================================================= */

/// Apply constraints that involve the top-most, containing
/// term.  This include type constraints, as well as evaluatable
/// terms that name the top variable.
HandleSet JoinLink::constrain(AtomSpace* as, bool silent,
                              Traverse& trav) const
{
	HandleSet rejects;

	AtomSpace* temp = nullptr;
	if (0 < _top_clauses.size())
		temp = grab_transient_atomspace(as);

	for (const Handle& h : trav.containers)
	{
		// Weed out anything that is the wrong type
		for (const Handle& toty : _top_types)
		{
			if (value_is_type(toty, h)) continue;
			rejects.insert(h);
			break;
		}

		// Run the evaluatable constraint clauses
		for (const Handle& toc : _top_clauses)
		{
			HandleMap plugs;
			plugs.insert({_top_var, h});

			// If the top clauses have other variables in them :-/
			if (_need_top_map)
			{
				const HandleSeq& gnds(trav.top_map.at(h));
				for (size_t i=0; i<_vsize; i++)
					plugs.insert({_variables.varseq[i], gnds[i]});
			}

			// Plug in any variables ...
			Handle topper = Replacement::replace_nocheck(toc, plugs);
			topper = temp->add_atom(topper);
			TruthValuePtr tvp =
				EvaluationLink::do_evaluate(temp, topper, silent);
			if (tvp->get_mean() < 0.5)
			{
				rejects.insert(h);
				break;
			}
		}
	}
	if (temp) release_transient_atomspace(temp);

	// Remove the rejects
	HandleSet accept;
	std::set_difference(trav.containers.begin(), trav.containers.end(),
	                    rejects.begin(), rejects.end(),
	                    std::inserter(accept, accept.begin()));
	return accept;
}

/* ================================================================= */

HandleSet JoinLink::container(AtomSpace* as, JoinCallback* jcb,
                              bool silent) const
{
	Traverse trav;
	trav.jcb = jcb;

	Type t = get_type();
	if (MINIMAL_JOIN_LINK == t)
		trav.containers = supremum(as, silent, trav);
	else if (UPPER_SET_LINK == t)
		trav.containers = upper_set(as, silent, trav);
	else if (MAXIMAL_JOIN_LINK == t)
	{
		HandleSet supset(supremum(as, silent, trav));
		for (const Handle& h: supset)
			find_top(trav, h);
		if (0 == trav.containers.size())
			trav.containers = supset;
	}

	// Apply constraints on the top type, if any
	if (0 < _top_types.size() or 0 < _top_clauses.size())
		trav.containers = constrain(as, silent, trav);

	// Perform the actual rewriting.
	fixup_replacements(trav);
	return replace(trav);
}

/* ================================================================= */

/// Given a top-level set of containing links, perform
/// replacements, substituting the bottom-most atoms as requested,
/// while honoring all scoping and quoting.
HandleSet JoinLink::replace(const Traverse& trav) const
{
	// Use the Replacement utility, so that all scoping and
	// quoting is handled correctly.
	HandleSet replaced;
	for (const Handle& top: trav.containers)
	{
		Handle rep = Replacement::replace_nocheck(top, trav.replace_map);
		replaced.insert(rep);
	}

	return replaced;
}

/* ================================================================= */

QueueValuePtr JoinLink::do_execute(AtomSpace* as,
                                   JoinCallback* jcb, bool silent)
{
	if (nullptr == as) as = _atom_space;

	HandleSet hs = container(as, jcb, silent);

	// XXX FIXME this is really dumb, using a queue and then
	// copying things into it. Whatever. Fix this.
	QueueValuePtr qvp(createQueueValue());
	for (const Handle& h : hs)
		qvp->push(as->add_atom(h));

	qvp->close();
	return qvp;
}

ValuePtr JoinLink::execute(AtomSpace* as, bool silent)
{
	DefaultJoinCallback djcb;
	return do_execute(as, &djcb, silent);
}

ValuePtr JoinLink::execute_cb(AtomSpace* as, JoinCallback* jcb)
{
	return do_execute(as, jcb, false);
}

DEFINE_LINK_FACTORY(JoinLink, JOIN_LINK)

/* ===================== END OF FILE ===================== */
