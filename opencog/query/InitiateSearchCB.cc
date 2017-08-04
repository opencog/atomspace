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

#define DEBUG 1
#ifdef DEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif

/* ======================================================== */

InitiateSearchCB::InitiateSearchCB(AtomSpace* as) :
	_classserver(classserver())
{
#ifdef CACHED_IMPLICATOR
	InitiateSearchCB::clear();
	InitiateSearchCB::ready(as);
#else
	_variables = NULL;
	_pattern = NULL;
	_dynamic = NULL;
	_pl = NULL;

	_root = Handle::UNDEFINED;
	_starter_term = Handle::UNDEFINED;

	_curr_clause = 0;
	_choices.clear();
 	_search_fail = false;
	_as = as;
#endif
}

#ifdef CACHED_IMPLICATOR
void InitiateSearchCB::ready(AtomSpace* as)
{
	_as = as;
}

void InitiateSearchCB::clear()
{
	_variables = NULL;
	_pattern = NULL;
	_dynamic = NULL;
	_pl = NULL;

	_root = Handle::UNDEFINED;
	_starter_term = Handle::UNDEFINED;

	_curr_clause = 0;
	_choices.clear();
 	_search_fail = false;
	_as = NULL;
}
#endif

void InitiateSearchCB::set_pattern(const Variables& vars,
                                   const Pattern& pat)
{
	_search_fail = false;

	_variables = &vars;
	_pattern = &pat;
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

	for (Handle hunt : h->getOutgoingSet())
	{
		size_t brdepth = depth + 1;
		size_t brwid = SIZE_MAX;
		Handle sbr(h);

		// Blow past the QuoteLinks, since they just screw up the search start.
		if (Quotation::is_quotation_type(hunt->getType()))
			hunt = hunt->getOutgoingAtom(0);

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
                                       const HandleSet& evl,
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
		if (start
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
	// If there are no non constant clauses, abort, will use
	// no_search instead.
	if (_pattern->clauses.empty()) {
		_search_fail = true;
		return false;
	}

	// Sometimes, the number of mandatory clauses can be zero...
	// or they might all be evaluatable.  In this case, its OK to
	// start searching with an optional clause. But if there ARE
	// mandatories, we must NOT start search on an optional, since,
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
		DO_LOG({LAZY_LOG_FINE << "Search start node: " << best_start->toShortString();})
		DO_LOG({LAZY_LOG_FINE << "Start term is: "
		              << (_starter_term == (Atom*) nullptr ?
		                  "UNDEFINED" : _starter_term->toShortString());})
		DO_LOG({LAZY_LOG_FINE << "Root clause is: " <<  _root->toShortString();})

		// This should be calling the over-loaded virtual method
		// get_incoming_set(), so that, e.g. it gets sorted by attentional
		// focus in the AttentionalFocusCB class...
		IncomingSet iset = get_incoming_set(best_start);
		size_t sz = iset.size();
		for (size_t i = 0; i < sz; i++)
		{
			Handle h(iset[i]);
			DO_LOG({LAZY_LOG_FINE << "xxxxxxxxxx neighbor_search xxxxxxxxxx\n"
			              << "Loop candidate (" << i+1 << "/" << sz << "):\n"
			              << h->toShortString();})
			bool found = pme->explore_neighborhood(_root, _starter_term, h);

			// If the starter term (or any other term we've explored) has globs
			// in it and it's possible to ground it to the same candidate
			// differently, go for it, before moving on to the next candidates.
			if (not found and pme->has_more_to_explore())
				found = pme->explore_neighborhood(_root, _starter_term, h);

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

	DO_LOG({logger().fine("Attempt to use node-neighbor search");})
	_search_fail = false;
	bool found = neighbor_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	// If we are here, then we could not find a clause at which to
	// start, which can happen if the clauses hold no variables, and
	// they are all evaluatable. This can happen for sequence links;
	// we want to quickly rule out this case before moving to more
	// complex searches, below.
	DO_LOG({logger().fine("Cannot use node-neighbor search, use no-var search");})
	_search_fail = false;
	found = no_search(pme);
	if (found) return true;
	if (not _search_fail) return false;

	// If we are here, then we could not find a clause at which to
	// start, which can happen if the clauses consist entirely of
	// variables! Which can happen (there is a unit test for this,
	// the LoopUTest), and so instead, we search based on the link
	// types that occur in the atomspace.
	DO_LOG({logger().fine("Cannot use no-var search, use link-type search");})
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
	DO_LOG({logger().fine("Cannot use link-type search, use variable-type search");})
	_search_fail = false;
	found = variable_search(pme);
	return found;
}

/* ======================================================== */
/**
 * Find the rarest link type contained in the clause, or one
 * of its subclauses.
 */
void InitiateSearchCB::find_rarest(const Handle& clause,
                                   Handle& rarest,
                                   size_t& count,
                                   Quotation quotation)
{
	Type t = clause->getType();

	// Base case
	if (quotation.is_unquoted() and (CHOICE_LINK == t)) return;

	if (not clause->isLink()) return;

	if (not quotation.consumable(t))
	{
		size_t num = (size_t) _as->get_num_atoms_of_type(t);
		if (num < count)
		{
			count = num;
			rarest = clause;
		}
	}

	// Recursive case
	quotation.update(t);

	const HandleSeq& oset = clause->getOutgoingSet();
	for (const Handle& h : oset)
		find_rarest(h, rarest, count, quotation);
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

	DO_LOG({LAZY_LOG_FINE << "Start clause is: " << std::endl
	              << _root->toShortString();})
	DO_LOG({LAZY_LOG_FINE << "Start term is: " << std::endl
	              << _starter_term->toShortString();})

	// Get type of the rarest link
	Type ptype = _starter_term->getType();

	HandleSeq handle_set;
	_as->get_handles_by_type(handle_set, ptype);

#ifdef DEBUG
	size_t i = 0, hsz = handle_set.size();
#endif
	for (const Handle& h : handle_set)
	{
		DO_LOG({LAZY_LOG_FINE << "yyyyyyyyyy link_type_search yyyyyyyyyy\n"
		              << "Loop candidate (" << ++i << "/" << hsz << "):\n"
		              << h->toShortString();})
		bool found = pme->explore_neighborhood(_root, _starter_term, h);

		// If the starter term (or any other term we've explored) has globs
		// in it and it's possible to ground it to the same candidate
		// differently, go for it, before moving on to the next candidates.
		if (not found and pme->has_more_to_explore())
			found = pme->explore_neighborhood(_root, _starter_term, h);

		if (found) return true;
	}
	return false;
}

/* ======================================================== */
/**
 * Initiate a search by looping over all atoms of the allowed
 * variable types (as set with the set_type_testrictions() method).
 * This assumes that the varset contains the variables to be searched
 * over, and that the type restrictions are set up appropriately.
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

	// Some search patterns simply do not have any groundable
	// clauses in them. This is one common reason why a variable-
	// based search is being performed.
	bool all_clauses_are_evaluatable = true;
	for (const Handle& cl : clauses)
	{
		if (0 < _pattern->evaluatable_holders.count(cl)) continue;
		all_clauses_are_evaluatable = false;
		break;
	}

	// Find the rarest variable type;
	size_t count = SIZE_MAX;
	std::set<Type> ptypes;

	DO_LOG({LAZY_LOG_FINE << "_variables = " <<  _variables->to_string();})
	_root = Handle::UNDEFINED;
	_starter_term = Handle::UNDEFINED;
	for (const Handle& var: _variables->varset)
	{
		DO_LOG({LAZY_LOG_FINE << "Examine variable " << var->toShortString();})

#ifdef _IMPLEMENT_ME_LATER
		// XXX TODO FIXME --- if there is a deep type in the mix, that
		// will offer a far-superior place to start the search.
		// Unfortunately, implementing this will require a bit more work,
		// so we punt for now, as there are no users ....
		auto dit = _variables->_deep_typemap.find(var);
		if (_variables->_deep_typemap.end() != dit)
			throw RuntimeException(TRACE_INFO, "Not implemented!");
#endif

		auto tit = _variables->_simple_typemap.find(var);
		if (_variables->_simple_typemap.end() == tit) continue;
		const std::set<Type>& typeset = tit->second;
		DO_LOG({LAZY_LOG_FINE << "Type-restriction set size = "
		              << typeset.size();})

		// Calculate the total number of atoms of typeset
		size_t num = 0;
		for (Type t : typeset)
			num += (size_t) _as->get_num_atoms_of_type(t);

		DO_LOG({LAZY_LOG_FINE << var->toString() << "has "
		              << num << " atoms in the atomspace";})

		if (0 < num and num < count)
		{
			for (const Handle& cl : clauses)
			{
				// Evaluatables don't exist in the atomspace, in general.
				// Therefore, we cannot start a search with them. Unless
				// they are all evaluatable, in which case we pick a clause
				// that has a variable with the narrowest type-membership.
				if (not all_clauses_are_evaluatable and
				    0 < _pattern->evaluatable_holders.count(cl)) continue;

				if (cl == var)
				{
					_root = cl;
					_starter_term = cl;
					count = num;
					ptypes = typeset;
					DO_LOG({LAZY_LOG_FINE << "New minimum count of " << count;})
					break;
				}

				FindAtoms fa(var);
				fa.search_set(cl);
				if (0 < fa.least_holders.size())
				{
					_root = cl;
					_starter_term = *fa.least_holders.begin();
					if (all_clauses_are_evaluatable)
						_starter_term = var;
					count = num;
					ptypes = typeset;
					DO_LOG({LAZY_LOG_FINE << "New minimum count of "
					              << count << " (nonroot)";})
					break;
				}
			}
		}
	}

	// There were no type restrictions!
	if (nullptr == _root)
	{

		if (not _variables->_deep_typemap.empty())
		{
			logger().warn("Warning: Full deep-type support not implemented!");
		}
		else
		{
// #define THROW_HARD_ERROR 1
#ifdef THROW_HARD_ERROR
			throw SyntaxException(TRACE_INFO,
				"Error: There were no type restrictions! That's infinite-recursive!");
#else
			logger().warn("Warning: No type restrictions! Your code has a bug in it!");
			for (const Handle& var: _variables->varset)
				logger().warn("Offending variable=%s\n", var->toString().c_str());
			for (const Handle& cl : clauses)
				logger().warn("Offending clauses=%s\n", cl->toString().c_str());

			// Terrible, terrible hack for detecting infinite loops.
			// When the world is ready for us, we should instead just
			// throw the hard error, as ifdef'ed above.
			static const Pattern* prev = nullptr;
			static unsigned int count = 0;
			if (prev != _pattern) { prev = _pattern; count = 0; }
			else {
				count++;
				if (1000 < count)
					throw RuntimeException(TRACE_INFO,
						"Infinite Loop detected! Recursed %u times!", count);
			}
#endif
		}

		if (0 == clauses.size())
		{
			// This is kind-of weird, but it can happen if all clauses
			// are optional.
			_search_fail = true;
			return false;
		}
		_root = _starter_term = clauses[0];
	}

	HandleSeq handle_set;
	if (ptypes.empty())
		_as->get_handles_by_type(handle_set, ATOM, true);
	else
		for (Type ptype : ptypes)
			_as->get_handles_by_type(handle_set, ptype);

	DO_LOG({LAZY_LOG_FINE << "Atomspace reported " << handle_set.size() << " atoms";})

#ifdef DEBUG
	size_t i = 0, hsz = handle_set.size();
#endif
	for (const Handle& h : handle_set)
	{
		DO_LOG({LAZY_LOG_FINE << "zzzzzzzzzzz variable_search zzzzzzzzzzz\n"
		              << "Loop candidate (" << ++i << "/" << hsz << "):\n"
		              << h->toShortString();})
		bool found = pme->explore_neighborhood(_root, _starter_term, h);

		// If the starter term (or any other term we've explored) has globs
		// in it and it's possible to ground it to the same candidate
		// differently, go for it, before moving on to the next candidates.
		if (not found and pme->has_more_to_explore())
			found = pme->explore_neighborhood(_root, _starter_term, h);

		if (found) return true;
	}

	return false;
}

/* ======================================================== */
/**
 * No search -- no variables, only constant, possibly evaluatable
 * clauses.  The stop-go sequence demo falls in this category: no
 * actual matching needs to be done; merely, the sequence needs to be
 * evaluated.  Arguably, it is a user error to use the pattern matcher
 * for this, as there is nothing that needs to be matched.  But, for
 * just right now, we gloss over this, and allow it, because it is
 * "closely related" to sequences with variables. It is a bit
 * inefficient to use the pattern matcher for this, so if you want it
 * to run fast, re-work the below to not use the PME.
 */
bool InitiateSearchCB::no_search(PatternMatchEngine *pme)
{
	if (0 < _variables->varset.size())
	{
		_search_fail = true;
		return false;
	}

	// Evaluate all evaluatable clauses
	return pme->explore_constant_evaluatables(_pattern->mandatory);
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

	// Now is the time to look up the definitions!
	// We loop here, so that all recursive definitions are expanded
	// as well.  XXX Except that this is wrong, if any of the
	// definitions are actually recursive. That is, this will be
	// an infinite loop if a defintion is self-referencing; so
	// really we need to expand, one level at a time, during
	// evaluation, and only expand if really, really needed. (Which
	// then brings up ideas like tail recursion, etc.)  Anyway, most
	// of this code should probably be moved to PatterLink::jit_expand()
	while (0 < _pattern->defined_terms.size())
	{
		Variables vset;
		HandleMap defnmap;
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
				Variables freevars;
				freevars.find_variables(defn);
				vset.extend(freevars);
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
	}

	_dynamic = &_pattern->evaluatable_terms;

	pme->set_pattern(*_variables, *_pattern);
	set_pattern(*_variables, *_pattern);
	DO_LOG({logger().fine("JIT expanded!");
	_pl->debug_log();})
}

/* ===================== END OF FILE ===================== */
