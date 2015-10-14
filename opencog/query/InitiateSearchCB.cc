/*
 * InitiateSearchCB.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  April 2015
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/AtomSpace.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>

#include "InitiateSearchCB.h"
#include "PatternMatchEngine.h"

using namespace opencog;

/* ======================================================== */

InitiateSearchCB::InitiateSearchCB(AtomSpace* as) :
	_classserver(classserver()),
	_variables(NULL),
	_pattern(NULL),
	_type_restrictions(NULL),
	_dynamic(NULL),
	_as(as)
{
}

void InitiateSearchCB::set_pattern(const Variables& vars,
                                   const Pattern& pat)
{
	_search_fail = false;

	_variables = &vars;
	_pattern = &pat;
	_type_restrictions = &vars.typemap;
	_dynamic = &pat.evaluatable_terms;
}


/* ======================================================== */

// Find a good place to start the search.
//
// The handle h points to a clause.  In principle, it is enough to
// simply find a constant in the clause, and just start there. In
// practice, this can be an awful way to do things. So, for example,
// most "typical" clauses will be of the form
//
//    EvaluationLink
//        PredicateNode "blah"
//        ListLink
//            VariableNode $var
//            ConceptNode  "item"
//
// Typically, the incoming set for "blah" will be huge, so starting the
// search there would be a poor choice. Typically, the incoming set to
// "item" will be much smaller, and so makes a better choice.  The code
// below tries to pass over "blah" and pick "item" instead.  It does so
// by comparing the size of the incoming sets of the two constants, and
// picking the one with the smaller ("thinner") incoming set. Note that
// this is a form of "greedy" search.
//
// Atoms that are inside of dynamically-evaluatable terms are not
// considered. That's because groundings for such terms might not exist
// in the atomspace, so a search that starts there is doomed to fail.
//
// Note that the algo explores the clause to its greatest depth. That's
// OK, because typical clauses are never very deep.
//
// A variant of this algo could incorporate the Attentional focus
// into the "thinnest" calculation, so that only high-AF atoms are
// considered.
//
// Note that the size of the incoming set really is a better measure,
// and not the depth.  So, for example, if "item" has a huge incoming
// set, but "blah" does not, then "blah" is a much better place to
// start.
//
// size_t& depth will be set to the depth of the thinnest constant found.
// Handle& start will be set to the link containing that constant.
// size_t& width will be set to the incoming-set size of the thinnest
//               constant found.
// The returned value will be the constant at which to start the search.
// If no constant is found, then the returned value is the undefnied
// handle.
//

Handle
InitiateSearchCB::find_starter(const Handle& h, size_t& depth,
                                     Handle& startrm, size_t& width)
{
	// If its a node, then we are done.
	Type t = h->getType();
	if (_classserver.isNode(t))
	{
		if (VARIABLE_NODE != t and GLOB_NODE != t)
		{
			width = h->getIncomingSetSize();
			startrm = h; // XXX wtf ???
			return h;
		}
		return Handle::UNDEFINED;
	}

	// If its a link, then find recursively
	return find_starter_recursive(h, depth, startrm, width);
}

Handle
InitiateSearchCB::find_starter_recursive(const Handle& h, size_t& depth,
                                         Handle& startrm, size_t& width)
{
	// If its a node, then we are done. Don't modify either depth or
	// start.
	Type t = h->getType();
	if (_classserver.isNode(t))
	{
		if (VARIABLE_NODE != t and GLOB_NODE != t)
		{
			width = h->getIncomingSetSize();
			return h;
		}
		return Handle::UNDEFINED;
	}

	// Ignore all dynamically-evaluatable links up front.
	if (_dynamic->find(h) != _dynamic->end())
		return Handle::UNDEFINED;

	// Iterate over all the handles in the outgoing set.
	// Find the deepest one that contains a constant, and start
	// the search there.  If there are two at the same depth,
	// then start with the skinnier one.
	size_t deepest = depth;
	startrm = Handle::UNDEFINED;
	Handle hdeepest(Handle::UNDEFINED);
	size_t thinnest = SIZE_MAX;

	LinkPtr ll(LinkCast(h));
	for (Handle hunt : ll->getOutgoingSet())
	{
		size_t brdepth = depth + 1;
		size_t brwid = SIZE_MAX;
		Handle sbr(h);

		// Blow past the QuoteLinks, since they just screw up the search start.
		if (QUOTE_LINK == hunt->getType())
			hunt = LinkCast(hunt)->getOutgoingAtom(0);

		Handle s(find_starter_recursive(hunt, brdepth, sbr, brwid));

		if (s)
		{
			// Each ChoiceLink is potentially disconnected from the rest
			// of the graph. Assume the worst case, explore them all.
			if (CHOICE_LINK == t)
			{
				Choice ch;
				ch.clause = _curr_clause;
				ch.best_start = s;
				ch.start_term = sbr;
				_choices.push_back(ch);
			}
			else
			if (brwid < thinnest
			    or (brwid == thinnest and deepest < brdepth))
			{
				deepest = brdepth;
				hdeepest = s;
				startrm = sbr;
				thinnest = brwid;
			}
		}
	}
	depth = deepest;
	width = thinnest;
	return hdeepest;
}

/* ======================================================== */
/**
 * Iterate over all the clauses, to find the "thinnest" one.
 * Skip any/all evaluatable clauses, as these typically do not
 * exist in the atomspace, anyway.
 */
Handle InitiateSearchCB::find_thinnest(const HandleSeq& clauses,
                                       const std::set<Handle>& evl,
                                       Handle& starter_term,
                                       size_t& bestclause)
{
	size_t thinnest = SIZE_MAX;
	size_t deepest = 0;
	bestclause = 0;
	Handle best_start(Handle::UNDEFINED);
	starter_term = Handle::UNDEFINED;
	_choices.clear();

	size_t nc = clauses.size();
	for (size_t i=0; i < nc; i++)
	{
		// Cannot start with an evaluatable clause!
		if (0 < evl.count(clauses[i])) continue;

		_curr_clause = i;
		Handle h(clauses[i]);
		size_t depth = 0;
		size_t width = SIZE_MAX;
		Handle term(Handle::UNDEFINED);
		Handle start(find_starter(h, depth, term, width));
		if (start != nullptr
		    and (width < thinnest
		         or (width == thinnest and depth > deepest)))
		{
			thinnest = width;
			deepest = depth;
			bestclause = i;
			best_start = start;
			starter_term = term;
		}
	}

	return best_start;
}

/* ======================================================== */
/**
 * Given a set of clauses, find a neighborhood to search, and perform
 * the search. A `neighborhood` is defined as all of the atoms that
 * can be reached from a given (non-variable) atom, by following either
 * it's incoming or its outgoing set.
 *
 * A neighborhood search is guaranteed to find all possible groundings
 * for the set of clauses. The reason for this is that, given a
 * non-variable atom in the pattern, any possible grounding of that
 * pattern must contain that atom, out of necessity. Thus, any possible
 * grounding must be contained in that neighborhood.  It is sufficient
 * to walk that graph until a suitable grounding is encountered.
 *
 * The return value is true if a grounding was found, else it returns
 * false. That is, this return value works just like all the other
 * satisfiability callbacks.  The flag '_search_fail' is set to true
 * if the search was not performed, due to a failure to find a sutiable
 * starting point.
 */
bool InitiateSearchCB::neighbor_search(PatternMatchEngine *pme)
{
	// Sometimes, the number of mandatory clauses can be zero...
	// or they might all be evaluatable.  In this case, its OK to
	// start searching with an optional clause. But if there ARE
	// mandatories, we must NOT start serch on an optional, since,
	// after all, it might be absent!
	bool try_all = true;
	for (const Handle& m : _pattern->mandatory)
	{
		if (0 == _pattern->evaluatable_holders.count(m))
		{
			try_all = false;
			break;
		}
	}

	const HandleSeq& clauses =
		try_all ?  _pattern->cnf_clauses :  _pattern->mandatory;

	// In principle, we could start our search at some node, any node,
	// that is not a variable. In practice, the search begins by
	// iterating over the incoming set of the node, and so, if it is
	// large, a huge amount of effort might be wasted exploring
	// dead-ends.  Thus, it pays off to start the search on the
	// node with the smallest ("narrowest" or "thinnest") incoming set
	// possible.  Thus, we look at all the clauses, to find the
	// "thinnest" one.
	//
	// Note also: the user is allowed to specify patterns that have
	// no constants in them at all.  In this case, the search is
	// performed by looping over all links of the given types.
	size_t bestclause;
	Handle best_start = find_thinnest(clauses, _pattern->evaluatable_holders,
	                                  _starter_term, bestclause);

	// Cannot find a starting point! This can happen if:
	// 1) all of the clauses contain nothing but variables,
	// 2) all of the clauses are evaluatable(!),
	// Somewhat unusual, but it can happen.  For this, we need
	// some other, alternative search strategy.
	if (nullptr == best_start and 0 == _choices.size())
	{
		_search_fail = true;
		return false;
	}

	// If only a single choice, fake it for the loop below.
	if (0 == _choices.size())
	{
		Choice ch;
		ch.clause = bestclause;
		ch.best_start = best_start;
		ch.start_term = _starter_term;
		_choices.push_back(ch);
	}
	else
	{
		// TODO -- weed out duplicates!
	}

	for (const Choice& ch : _choices)
	{
		bestclause = ch.clause;
		best_start = ch.best_start;
		_starter_term = ch.start_term;

		_root = clauses[bestclause];
		LAZY_LOG_FINE << "Search start node: " << best_start->toShortString();
		LAZY_LOG_FINE << "Start term is: "
		              << (_starter_term == nullptr ?
		                  "UNDEFINED" : _starter_term->toShortString());
		LAZY_LOG_FINE << "Root clause is: " <<  _root->toShortString();

		// This should be calling the over-loaded virtual method
		// get_incoming_set(), so that, e.g. it gets sorted by attentional
		// focus in the AttentionalFocusCB class...
		IncomingSet iset = get_incoming_set(best_start);
		size_t sz = iset.size();
		for (size_t i = 0; i < sz; i++)
		{
			Handle h(iset[i]);
			LAZY_LOG_FINE << "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n"
			              << "Loop candidate (" << i+1 << "/" << sz << "):\n"
			              << h->toShortString();
			bool found = pme->explore_neighborhood(_root, _starter_term, h);

			// Terminate search if satisfied.
			if (found) return true;
		}
	}

	// If we are here, we have searched the entire neighborhood, and
	// no satisfiable groundings were found.
	return false;
}

/* ======================================================== */
/**
 * Search for solutions/groundings over all of the AtomSpace, using
 * the standard, canonical assumptions about the structure of the search
 * pattern.  Here, the "standard, canonical" assumptions are that the
 * pattern consists of clauses that contain VariableNodes in them, with
 * the VariableNodes interpreted in the "standard, canonical" way:
 * namely, that these are the atoms that are to be grounded, as normally
 * described elsewhere in the documentation.  In such a case, a full and
 * complete search for any/all possible groundings is performed; if
 * there are groundings, they are guaranteed to be found; if there are
 * none, then it is guaranteed that this will also be correctly
 * reported. For certain, highly unusual (but still canonical) search
 * patterns, the same grounding may be reported more than once; grep for
 * notes pertaining to the ChoiceLink, and the ArcanaUTest for details.
 * Otherwise, all possible groundings are guaranteed to be returned
 * exactly once.
 *
 * We emphasize "standard, canonical" here, for a reason: the pattern
 * engine is capable of doing many strange, weird things, depending on
 * how the callbacks are designed to work.  For those other
 * applications, it is possible or likely that this method will fail to
 * traverse the "interesting" parts of the atomspace: non-standard
 * callbacks may also need a non-standard search strategy.
 *
 * Now, some notes on the strategy employed here, and how non-canonical
 * callbacks might affect it:
 *
 * 1) Search will begin at the first non-variable node in the "thinnest"
 *    clause.  The thinnest clause is chosen, so as to improve performance;
 *    but this has no effect on the thoroughness of the search.  The search
 *    will proceed by exploring the entire incoming-set for this node.
 *
 *    This is ideal, when the node_match() callback accepts a match only
 *    when the pattern and suggested nodes are identical (i.e. are
 *    exactly the same atom).  If the node_match() callback is willing to
 *    accept a broader range of node matches, then other possible
 *    solutions might be missed. Just how to fix this depends sharpely
 *    on what node_match() is willing to accept as a match.
 *
 *    Anyway, this seems like a very reasonable limitation: if you
 *    really want a lenient node_match(), then use variables instead.
 *    Don't overload node-match with something weird, and you should be
 *    OK.  Otherwise, you'll have to implement your own initiate_search()
 *    callback.
 *
 * 2) If the clauses consist entirely of variables, i.e. if there is not
 *    even one single non-variable node in the pattern, then a search is
 *    driven by looking for all links that are of the same type as one
 *    of the links in one of the clauses.
 *
 *    If the link_match() callback is willing to accept a broader range
 *    of types, then this search method may fail to find some possible
 *    patterns.
 *
 *    Lets start by noting that this situation is very rare: most
 *    patterns will not consist entirely if Links and VariableNodes.
 *    Almost surely, most reasonable people will have at least one
 *    non-variable node in the pattern. So the disucssion below almost
 *    surely does not apply.
 *
 *    But if yhou really want this, there are several possible remedies.
 *    One is to modify the link_type_search() callback to try each
 *    possible link type that is considered bo be equivalent by
 *    link_match(). Another alternative is to just leave the
 *    link_match() callback alone, and use variables for links, instead.
 *    This is probably the best strategy, because then the fairly
 *    standard reasoning can be used when thinking about the problem.
 *    Of course, you can always write your own initiate_search() callback.
 *
 * If the constraint 1) can be met, (which is always the case for
 * "standard, canonical" searches, then the pattern match should be
 * quite rapid.  Incoming sets tend to be small; in addition, the
 * implemnentation here picks the smallest, "tinnest" incoming set to
 * explore.
 *
 * The default implementation of node_match() and link_match() in this
 * class does satisfy both 1) and 2), so this algo will work correctly,
 * if these two methods are not overloaded with more callbacks that are
 * lenient about matching.
 *
 * If you overload node_match(), and do so in a way that breaks
 * assumption 1), then you will scratch your head, thinking
 * "why did my search fail to find this obvious solution?" The answer
 * will be for you to create a new search algo, in a new class, that
 * overloads this one, and does what you want it to.  This class should
 * probably *not* be modified, since it is quite efficient for the
 * "standard, canonical" case.
 */
bool InitiateSearchCB::initiate_search(PatternMatchEngine *pme)
{
	jit_analyze(pme);

	logger().fine("Attempt to use node-neighbor search");
	_search_fail = false;
	bool found = neighbor_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	// If we are here, then we could not find a clause at which to
	// start, which can happen if the clauses hold no variables, and
	// they are all evaluatable. This can happen for sequence links;
	// we want to quickly rule out this case before moving to more
	// complex searches, below.
	logger().fine("Cannot use node-neighbor search, use no-var search");
	_search_fail = false;
	found = no_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	// If we are here, then we could not find a clause at which to
	// start, which can happen if the clauses consist entirely of
	// variables! Which can happen (there is a unit test for this,
	// the LoopUTest), and so instead, we search based on the link
	// types that occur in the atomspace.
	logger().fine("Cannot use no-var search, use link-type search");
	_search_fail = false;
	found = link_type_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	// The URE Reasoning case: if we found nothing, then there are no
	// links!  Ergo, every clause must be a lone variable, all by
	// itself. This is how some URE rules may start: the specify a single
	// variable, all by itself, and set some type restrictions on it,
	// and that's all. We deal with this in the variable_search()
	// method.
	logger().fine("Cannot use link-type search, use variable-type search");
	_search_fail = false;
	found = variable_search(pme);
	return found;
}

/* ======================================================== */
/**
 * Find the rarest link type contained in the clause, or one
 * of its subclauses. Of course, QuoteLinks, and anything under
 * a Quotelink, must be ignored.
 */
void InitiateSearchCB::find_rarest(const Handle& clause,
                                   Handle& rarest,
                                   size_t& count)
{
	Type t = clause->getType();
	if (QUOTE_LINK == t) return;
	if (CHOICE_LINK == t) return;

	LinkPtr lll(LinkCast(clause));
	if (NULL == lll) return;

	size_t num = (size_t) _as->get_num_atoms_of_type(t);
	if (num < count)
	{
		count = num;
		rarest = clause;
	}

	const HandleSeq& oset = lll->getOutgoingSet();
	for (const Handle& h : oset)
		find_rarest(h, rarest, count);
}

/* ======================================================== */
/**
 * Initiate a search by looping over all Links of the same type as one
 * of the links in the set of clauses.  This attempts to pick the link
 * type which has the smallest number of atoms of that type in the
 * AtomSpace.
 */
bool InitiateSearchCB::link_type_search(PatternMatchEngine *pme)
{
	const HandleSeq& clauses = _pattern->mandatory;

	_search_fail = false;
	_root = Handle::UNDEFINED;
	_starter_term = Handle::UNDEFINED;
	size_t count = SIZE_MAX;

	for (const Handle& cl: clauses)
	{
		// Evaluatables don't exist in the atomspace, in general.
		// Cannot start a search with them.
		if (0 < _pattern->evaluatable_holders.count(cl)) continue;
		size_t prev = count;
		find_rarest(cl, _starter_term, count);
		if (count < prev)
		{
			prev = count;
			_root = cl;
		}
	}

	// The URE Reasoning case: if we found nothing, then there are no
	// links!  Ergo, every clause must be a lone variable, all by
	// itself. This is how some URE rules may start: the specify a single
	// variable, all by itself, and set some type restrictions on it,
	// and that's all. We deal with this in the variable_search()
	// method.
	if (nullptr == _root)
	{
		_search_fail = true;
		return false;
	}

	LAZY_LOG_FINE << "Start clause is: " << std::endl
	              << _root->toShortString();
	LAZY_LOG_FINE << "Start term is: " << std::endl
	              << _starter_term->toShortString();

	// Get type of the rarest link
	Type ptype = _starter_term->getType();

	HandleSeq handle_set;
	_as->get_handles_by_type(handle_set, ptype);

	size_t i = 0, hsz = handle_set.size();
	for (const Handle& h : handle_set)
	{
		LAZY_LOG_FINE << "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy\n"
		              << "Loop candidate (" << ++i << "/" << hsz << "):\n"
		              << h->toShortString();
		bool found = pme->explore_neighborhood(_root, _starter_term, h);
		if (found) return true;
	}
	return false;
}

/* ======================================================== */
/**
 * Initiate a search by looping over all atoms of the allowed
 * variable types (as set with the set_type_testrictions() method).
 * This assumes that the varset contains the variables to be searched
 * over, and that the type restrictins are set up approrpriately.
 *
 * If the varset is empty, or if there are no variables, then the
 * entire atomspace will be searched.  Depending on the pattern,
 * many, many duplicates might be reported. If you are not using
 * variables, then you probably don't want to use this method, either;
 * you should create somethnig more clever.
 */
bool InitiateSearchCB::variable_search(PatternMatchEngine *pme)
{
	const HandleSeq& clauses = _pattern->mandatory;

	// Find the rarest variable type;
	size_t count = SIZE_MAX;
	Type ptype = ATOM;

	LAZY_LOG_FINE << "varset size = " <<  _variables->varset.size();
	_root = Handle::UNDEFINED;
	_starter_term = Handle::UNDEFINED;
	for (const Handle& var: _variables->varset)
	{
		LAZY_LOG_FINE << "Examine variable " << var->toShortString();
		auto tit = _type_restrictions->find(var);
		if (_type_restrictions->end() == tit) continue;
		const std::set<Type>& typeset = tit->second;
		LAZY_LOG_FINE << "Type-restictions set size = "
		              << typeset.size();
		for (Type t : typeset)
		{
			size_t num = (size_t) _as->get_num_atoms_of_type(t);
			LAZY_LOG_FINE << "Type = "
			              << _classserver.getTypeName(t) << " has "
			              << num << " atoms in the atomspace";
			if (0 < num and num < count)
			{
				for (const Handle& cl : clauses)
				{
					// Evaluatables dont' exist in the atomspace, in general.
					// Cannot start a search with them.
					if (0 < _pattern->evaluatable_holders.count(cl)) continue;
					FindAtoms fa(var);
					fa.search_set(cl);
					if (cl == var)
					{
						_root = cl;
						_starter_term = cl;
						count = num;
						ptype = t;
						LAZY_LOG_FINE << "New minimum count of " << count;
						break;
					}
					if (0 < fa.least_holders.size())
					{
						_root = cl;
						_starter_term = *fa.least_holders.begin();
						count = num;
						ptype = t;
						LAZY_LOG_FINE << "New minimum count of "
						              << count << "(nonroot)";
						break;
					}
				}
			}
		}
	}

	// There were no type restrictions!
	if (nullptr == _root)
	{
		logger().fine("There were no type restrictions! That must be wrong!");
		if (0 == clauses.size())
		{
			// This is kind-of weird, it can happen if all clauses
			// are optional.
			_search_fail = true;
			return false;
		}
		_root = _starter_term = clauses[0];
	}

	HandleSeq handle_set;
	if (ptype == ATOM)
		_as->get_handles_by_type(handle_set, ptype, true);
	else
		_as->get_handles_by_type(handle_set, ptype);

	LAZY_LOG_FINE << "Atomspace reported " << handle_set.size() << " atoms";

	size_t i = 0, hsz = handle_set.size();
	for (const Handle& h : handle_set)
	{
		LAZY_LOG_FINE << "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\n"
		              << "Loop candidate (" << ++i << "/" << hsz << "):\n"
		              << h->toShortString();
		bool found = pme->explore_neighborhood(_root, _starter_term, h);
		if (found) return true;
	}

	return false;
}

/* ======================================================== */
/**
 * No search -- no variables, one evaluatable clause.
 * The stop-go sequence demo falls in this category: no actual
 * matching needs to be done; merely, the sequence needs to be
 * evaluated.  Arguably, it is a user error to use the pattern
 * matcher for this, as there is nothing that needs to be matched.
 * But, for just right now, we gloss over this, and allow it, because
 * it is "closely related" to sequences with variables. It is a
 * bit inefficient to use the pattern matcher for this, so if you
 * want it to run fast, re-work the below to not use the PME.
 */
bool InitiateSearchCB::no_search(PatternMatchEngine *pme)
{
	if (0 < _variables->varset.size() or
	    1 != _pattern->mandatory.size())
	{
		_search_fail = true;
		return false;
	}

	// The one-and-only clause must be evaluatable!
	const HandleSeq& clauses = _pattern->mandatory;
	const std::set<Handle>& evl = _pattern->evaluatable_holders;
	if (0 == evl.count(clauses[0]))
	{
		_search_fail = true;
		return false;
	}

	LAZY_LOG_FINE << "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww\n"
	              << "Non-search: no variables, no non-evaluatable clauses";
	_root = _starter_term = clauses[0];
	bool found = pme->explore_neighborhood(_root, _starter_term, _root);
	return found;
}

/* ======================================================== */
/**
 * Just-In-Time analysis of patterns. Patterns we could not unpack
 * earlier, because the definitions for them might not have been
 * present, or may have changed since the pattern was initially created.
 */
void InitiateSearchCB::jit_analyze(PatternMatchEngine* pme)
{
	// If there are no definitions, there is nothing to do.
	if (0 == _pattern->defined_terms.size())
		return;

	// Now is the time to look up the defintions!
	Variables vset;
	std::map<Handle, Handle> defnmap;
	for (const Handle& name : _pattern->defined_terms)
	{
		Handle defn = DefineLink::get_definition(name);
		if (not defn) continue;

		// Extract the variables in the definition.
		// Either they are given in a LambdaLink, or, if absent,
		// we just hunt down and bind all of them.
		if (_classserver.isA(LAMBDA_LINK, defn->getType()))
		{
			LambdaLinkPtr lam = LambdaLinkCast(defn);
			vset.extend(lam->get_variables());
			defn = lam->get_body();
		}
		else
		{
			FreeLink fl(defn);
			VariableList vl(fl.get_vars());
			vset.extend(vl.get_variables());
		}

		defnmap.insert({name, defn});
	}

	// Rebuild the pattern, expanding all DefinedPredicateNodes to one level.
	// Note that newbody is not being place in any atomspace; but I think
	// that is OK...
	Handle newbody = Substitutor::substitute(_pattern->body, defnmap);

	// We need to let both the PME know about the new clauses
	// and variables, and also let master callback class know,
	// too, since we are just one mixin in the callback class;
	// the other mixins need to be updated as well.
	vset.extend(*_variables);

	_pl = createPatternLink(vset, newbody);
	_variables = &_pl->get_variables();
	_pattern = &_pl->get_pattern();

	_type_restrictions = &_variables->typemap;
	_dynamic = &_pattern->evaluatable_terms;

	pme->set_pattern(*_variables, *_pattern);
	set_pattern(*_variables, *_pattern);
	logger().fine("JIT expanded!");
	_pl->debug_log();
}

/* ===================== END OF FILE ===================== */
