/*
 * PatternMatchCallback.h
 *
 * Author: Linas Vepstas February 2008
 *
 * Copyright (C) 2008,2009,2014 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PATTERN_MATCH_CALLBACK_H
#define _OPENCOG_PATTERN_MATCH_CALLBACK_H

#include <map>
#include <set>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/pattern/PatternTerm.h> // for pattern context

namespace opencog {

/**
 * Callback interface, used to implement specifics of hypergraph
 * matching, and also, to report solutions when found.
 */
class PatternMatchCallback
{
	public:
		virtual ~PatternMatchCallback() {};

		/**
		 * Called when a node in the template pattern
		 * needs to be compared to a possibly matching
		 * node in the atomspace. The first argument
		 * is a node from the pattern, and the second
		 * is a possible solution node from the atomspace.
		 * Return true if the nodes match, else return
		 * false. (i.e. return false if mis-match).
		 */
		virtual bool node_match(const Handle& patt_node,
		                        const Handle& grnd_node) = 0;

		/**
		 * Called when a variable in the template pattern
		 * needs to be compared to a possible grounding
		 * node in the atomspace. The first argument
		 * is a variable from the pattern, and the second
		 * is a possible solution node from the atomspace.
		 * Return true if the grouding is acceptable, else
		 * return false. (i.e. return false if mis-match).
		 */
		virtual bool variable_match(const Handle& patt_node,
		                            const Handle& grnd_node) = 0;

		/**
		 * Called when there is a variable in the template
		 * pattern, but it is not bound into the template
		 * itself: its just some other variable, not eligable
		 * for handling by variable_match() above. This variable
		 * is possibly free in the template, and it is possibly
		 * a bound variable that the matcher has stumbled across.
		 * If its a free variable, this callback can do as it
		 * wishes, but if it is a bound variable, this callback
		 * should probably respect that, and allow a match if and
		 * only if it is alpha-convertible to the proposed grounding.
		 */
		virtual bool scope_match(const Handle& patt_node,
		                         const Handle& grnd_node) = 0;

		/**
		 * Called right before link in the template pattern
		 * is to be compared to a possibly matching link in
		 * the atomspace. The first argument is a link from
		 * the pattern, and the second is a possible
		 * grounding link from the atomspace. Return true
		 * if the link contents should be compared, else
		 * return false. (i.e. return false if mis-match).
		 *
		 * If true is returned, then the pattern matcher
		 * will proceed, and will compare the outgoing sets
		 * of the two links.  Thus, this callback should not
		 * bother with looking at the outgoing sets.  Indeed,
		 * it is very possible that the outgoing sets will
		 * fail to match; but this is not yet known, at the
		 * time that this callback is made.  By contrast,
		 * the post_link_match() callback will be called
		 * after a full grounding has been established;
		 * that is, after the outgoing sets have been compared.
		 *
		 * This callback offers a good time to check the
		 * truth value, the attention value, and the link
		 * type, and to proceed with the search, or cut it
		 * off, based on these values.
		 */
		virtual bool link_match(const PatternTermPtr& patt_link,
		                        const Handle& grnd_link) = 0;

		/**
		 * Called after a candidate grounding has been found
		 * for a link.  This callback offers a final chance
		 * to reject the link match based on the actual
		 * grounding, or to perform post-match processing.
		 * Return true to reject the match.
		 *
		 * That is, this callback is called after the two
		 * links have been fully compared, and have been
		 * found to match.  It offers a chance to record
		 * the candidate grounding, or to reject it for some
		 * reason.
		 *
		 * The first link is from the pattern, the second is
		 * from the proposed grounding.
		 */
		virtual bool post_link_match(const Handle& patt_link,
		                             const Handle& grnd_link)
		{
			return true; // Accept the match, by default.
		}

		/**
		 * Called after a failed attempt to ground a link.
		 * After every call to link_match(), above, there will
		 * be a call either to post_link_match() above, or to
		 * this method. Thus, the total number of calls to these
		 * methods will always be balanced. This allows the
		 * callback system to maintain a per-link stack, where a
		 * stack push is performed by link_match(), and a stack
		 * pop is done either in post_link_match() or here.
		 */
		virtual void post_link_mismatch(const Handle& patt_link,
		                                const Handle& grnd_link)
		{}

		/**
		 * Called when the template pattern and the candidate grounding
		 * do not have the same type, or if one of them is undefined.
		 * By default, this is a mismatch, but, if overloaded, it can be
		 * used to declare approximate matches.
		 */
		virtual bool fuzzy_match(const Handle& ph, const Handle& gh)
		{
			return false;
		}

		/**
		 * Invoked to confirm or deny a candidate grounding for term that
		 * consistes entirely of connectives and evaluatable terms.
		 *
		 * An 'evaluatable term' is any pattern term (term occuring in the
		 * search pattern, either a node or link) that was  previously
		 * declared as 'evaluatable'.  The canonical, default convention
		 * is that evaluatable terms are any terms that contain a GPN --
		 * a 'GroundedPredicateNode', or are assorted relations, such
		 * as EqualLink, GreaterThanLink, etc.  Evaluatables may also be
		 * "connectives"; in the default canonical interpretation, these
		 * are AndLink, OrLink, NotLink, SequentialAnd and SequentialOr.
		 *
		 * The canonical interpretation is compiled in PatternLink. That
		 * is, PatternLink performs a static analysis of the pattern, and
		 * marks each term in the pattern as being evaluatable, or not.
		 * Other pattern compilers are free to choose other conventions;
		 * the pattern engine does not care.
		 *
		 * Unlike the other callbacks, this takes arguments in slightly
		 * different form.  Here, 'eval' is the evaluatable term, and
		 * 'gnds' contains the currently-proposed grounding for any
		 * variables occuring within that term. It is a map: the 'key' is
		 * either a variable, or another term whose grounding is being
		 * currently considered, and the 'value' is the proposed
		 * grounding.
		 *
		 * In ordinary logic, a well-formed formula with no free variables
		 * is called a "sentence". Thus, in the sense of logic, the 'eval'
		 * argument, together with 'gnds' combine to bind values to all
		 * the variables in 'term', thus leaving a sentence with no free
		 * variables. In logic, sentences are always true or false, ergo
		 * the the return value.
		 *
		 * The return value follows the same convention as all the other
		 * callbacks: 'true' accepts the grounding, and the search for the
		 * rest of the pattern continues, while 'false' rejects the
		 * grounding, and forces a backtrack.
		 */
		virtual bool evaluate_sentence(const Handle& eval,
		                               const GroundingMap& gnds) = 0;

		/**
		 * Called when a top-level clause has been fully grounded.
		 * This is meant to be used for evaluating the truth value
		 * of the clause, as an intermediate stage for evaluating
		 * the overall truth value of a solution (grounding).
		 *
		 * A clause match has occured if all calls to node_match(),
		 * variable_match(), link_match() and post_link_match() in
		 * that clause have returned true.
		 *
		 * The term_gnds map shows which terms in any ChoiceLinks
		 * were actually grounded.
		 *
		 * Return false to reject this clause as a valid grounding,
		 * return true to accept this grounding.
		 */
		virtual bool clause_match(const Handle& pattrn_link_h,
		                          const Handle& grnd_link_h,
		                          const GroundingMap& term_gnds)
		{
			// Reject templates grounded by themselves.
			if (pattrn_link_h == grnd_link_h) return false;
			return true;
		}

		/**
		 * Called when the search for a top-level optional clause
		 * has been completed. The clause may or may not have been
		 * grounded as a result of the search. If it has been grounded,
		 * then grnd will be non-null.
		 *
		 * Return false to terminate further searches from this point
		 * on; the result of termination will be backtracking to search
		 * for other possible groundings of the required clauses.
		 * Return true to examine the next optional clause (if any).
		 *
		 * Note that all required clauses will have been grounded before
		 * any optional clauses are examined.
		 */
		virtual bool optional_clause_match(const Handle& pattrn,
		                                   const Handle& grnd,
		                                   const GroundingMap& term_gnds) = 0;

		/**
		 * Called when the search for a top-level for-all clause
		 * has been completed. The clause may or may not have been
		 * grounded as a result of the search. If it has been grounded,
		 * then grnd will be non-null.
		 *
		 * Return false to terminate further searches from this point
		 * on; the result of termination will be backtracking to search
		 * for other possible groundings of the required clauses.
		 * Return true to examine the next for-all clause (if any).
		 *
		 * Note that all required clauses will have been grounded,
		 * and all optional clauses will have been examined before
		 * the for-all clauses are tested. This guarantees that the
		 * groundings for all variables are "final", and this is a
		 * last-chance test.
		 */
		virtual bool always_clause_match(const Handle& pattrn,
		                                 const Handle& grnd,
		                                 const GroundingMap& term_gnds) = 0;

		/**
		 * Called when a complete grounding for all clauses is found.
		 * Should return false to search for more solutions; or return
		 * true to terminate search.  (Just as in all the other callbacks,
		 * a return value of `true` means that the proposed grounding is
		 * acceptable. The engine is designed to halt once an acceptable
		 * solution has been found; thus, in order to force it to search
		 * for more, a return value of false is needed.)
		 *
		 * Note that this callback may be called multiple times, to report
		 * the same result.  This can happen, for example, if there are
		 * mutiple ways for the pattern to match up to the result.
		 */
		virtual bool grounding(const GroundingMap &var_soln,
		                       const GroundingMap &term_soln) = 0;

		/**
		 * Called whenever the incoming set of an atom is to be explored.
		 * This callback allows the search space to be prioritized, by
		 * returning (all or some of) the incoming set in some sorted
		 * order: the first in the list will be searched first.
		 * The search space can also be limited, by returning a set that
		 * is smaller than the full incoming set (for example, by
		 * returning only those atoms with a high av-sti).
		 */
		virtual IncomingSet get_incoming_set(const Handle& h, Type t)
		{
			return h->getIncomingSetByType(t);
		}

		virtual const TypeSet& get_connectives(void)
		{ static const TypeSet _empty; return _empty; }

		/**
		 * Called before when the search is started. This gives the system
		 * a chance to perform neeeded intializations before the actual
		 * search is started.  In principle, this callback is not really
		 * needed, since the `perform_search()` callback "knows" that the
		 * search is starting when it is called.  In practice,  the
		 * implementation is much simpler if there is a distinct callback
		 * to handle this situation.  Return `true` to abort the search
		 * before it is even started, else return `false`.
		 */
		virtual bool start_search(void) { return false; }

		/**
		 * Called to perform the search. This callback is responsible
		 * for performing the top-most, outer loop of the search. That is,
		 * it gets to pick the starting points for the search, thereby
		 * possibly limiting the breadth of the search.  It may also cull
		 * the variables, clauses, or negated clauses to remove those that
		 * will not alter the final semantics of the search.
		 *
		 * The return value is used to indicate if the search pattern was
		 * satisfied (grounded) or not.  This is just like the return
		 * values on all the other callbacks; it summarizes (passes
		 * through) the return values of all the others.
		 */
		virtual bool perform_search(PatternMatchCallback&) = 0;

		/**
		 * Called when the search has completed. In principle, this
		 * callback is not really needed, since the `perform_search()`
		 * callback "knows" when the search is completed: its completed
		 * when it returns. In practice, the implementation is much
		 * simpler if there is a distinct callback to announce completion.
		 * The argument is the return value from `perform_search()`.
		 */
		virtual bool search_finished(bool done) { return done; }

		/**
		 * A pair of functions that are called to obtain the set of
		 * clauses to explore next. These are clauses that contain
		 * ungrounded variables, for which a grounding must be found.
		 * The set is a disjunctive set (a choice set) -- it is enough
		 * to ground any one of out of the set.
		 *
		 * The first function, `next_connections()` is called with the
		 * current groundings at this time. Immediately afterwards,
		 * `get_next_clause()` is called to obtain one of the choices.
		 * It is then called repeatedly to get the next choice. It should
		 * return false if there are no more; else return true.
		 */
		virtual void next_connections(const GroundingMap& var_grounding) = 0;
		virtual bool get_next_clause(PatternTermPtr& clause,
		                             Handle& joint) = 0;

		/**
		 * Called after a top-level clause (tree) has been fully
		 * grounded. This gives the callee the opportunity to save
		 * state onto a stack, if needed.
		 */
		virtual void push(void) {}

		/**
		 * Called prior to starting a back-track, retreating from the
		 * most recently grounded top-level clause (tree). This
		 * gives the callee the opportunity to maintain state with a
		 * stack, if needed.
		 */
		virtual void pop(void) {}

		/**
		 * Called before search initiation, to indicate the pattern
		 * that will be searched for, and the variables to be grounded
		 * during the search.
		 */
		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat) = 0;

		/**
		 * You get to call this, to perform the actual earch.
		 */
		virtual bool satisfy(const PatternLinkPtr&) = 0;
};

// See notes in `InitiateSearchMixin.cc` for an explanation of the
// threading code and its status.
// #define USE_THREADED_PATTERN_ENGINE
#ifdef USE_THREADED_PATTERN_ENGINE
	#define DECLARE_PE_MUTEX std::mutex _mtx;
	#define LOCK_PE_MUTEX std::lock_guard<std::mutex> lck(_mtx);
#else
	#define DECLARE_PE_MUTEX
	#define LOCK_PE_MUTEX
#endif // USE_THREADED_PATTERN_ENGINE

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_CALLBACK_H
