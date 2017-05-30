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
#include <opencog/atoms/core/VariableList.h> // for VariableTypeMap
#include <opencog/atoms/pattern/Pattern.h> // for VariableTypeMap
#include <opencog/atoms/pattern/PatternTerm.h> // for pattern context

namespace opencog {
class PatternMatchEngine;

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
		 * declared as 'evaluatable', by the 'set_evaluatable_terms()'
		 * callback below.  Thus, for example, the canonical, default
		 * convention is that evaluatable terms are any terms that
		 * contain a GPN -- a 'GroundedPredicateNode', and therefore
		 * requires running code that lies outside of the pattern matcher
		 * (typically scheme or python code).  In general, though,
		 * evaluatable terms don't have to be of this sort -- they can
		 * be any part of the search pattern that was previously
		 * specified by the  'set_evaluatable_terms()' callback.
		 *
		 * Connectives are any link types that were declared by the
		 * 'get_connectives()' callback, below. Connectives are those
		 * link types that are are able to combine evaluatable terms.
		 * For example, the canonical, default connectives are logic
		 * connectives: AndLink, OrLink, NotLink. They don't have to
		 * be these, but, that is what they would be if emulating a
		 * logic that uses the classical boolean logic values of
		 * true/false.
		 *
		 * For another example, they might be SetUnion, SetIntersectionLink,
		 * and SetComplimentLink if emulating a logic that uses probability
		 * values for truth values (so, e.g. SetUnionLink would connect
		 * the probability of A or B, i.e. it would compute P(A or B)
		 * SetIntersectionLink would compute P(A and B), and so on.
		 *
		 * Unlike the other callbacks, this takes arguments in s slightly
		 * different form.  Here, 'eval' is the evalutatable term, and
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
		                               const HandleMap& gnds) = 0;

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
		                          const HandleMap& term_gnds)
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
		                                   const HandleMap& term_gnds) = 0;

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
		virtual bool grounding(const HandleMap &var_soln,
		                       const HandleMap &term_soln) = 0;

		/**
		 * Called whenever the incoming set of an atom is to be explored.
		 * This callback allows the search space to be prioritized, by
		 * returning (all or some of) the incoming set in some sorted
		 * order: the first in the list will be searched first.
		 * The search space can also be limited, by returning a set that
		 * is smaller than the full incoming set (for example, by
		 * returning only those atoms with a high av-sti).
		 */
		virtual IncomingSet get_incoming_set(const Handle& h)
		{
			return h->getIncomingSet();
		}

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

		virtual const std::set<Type>& get_connectives(void)
		{ static const std::set<Type> _empty; return _empty; }

		/**
		 * Called to initiate the search. This callback is responsible
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
		virtual bool initiate_search(PatternMatchEngine *) = 0;

		/**
		 * Called when the search has completed. In principle, this is not
		 * really needed, since the above callback "knows" when the search
		 * is completed: its completed when the above returns. In practice, 
		 * the implementation is much simpler if we have a distinct
		 * callback to handle this situation.  The argument, is the return
		 * value from initiaitate_search().
		 */
		virtual bool search_finished(bool done) { return done; }

		/**
		 * Called before search initiation, to indicate the pattern
		 * that will be searched for, and the variables to be grounded
		 * during the search.
		 */
		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat) = 0;
};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_CALLBACK_H
