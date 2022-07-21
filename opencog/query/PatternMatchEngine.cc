/*
 * PatternMatchEngine.cc
 *
 * Copyright (C) 2008,2009,2011,2014,2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  February 2008
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

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atomspace/AtomSpace.h>

#include "PatternMatchEngine.h"

using namespace opencog;

/* ======================================================== */
/**
 * Pattern Match Engine Overview
 * -----------------------------
 * The explanation of how this code *actually* works has been
 * moved to README-Algorithm. That README provides the "big-picture"
 * explanation of the rather complex interwoven code below. Read it
 * first, and refer to it when examining the main methods below.
 */
/* ======================================================== */

// The macro POPSTK has an OC_ASSERT when QDEBUG is defined, so we keep
// that #define around, although it's not clear why that OC_ASSERT
// wouldn't be kept no matter what (it's not like it's gonna take up a
// lot of resources).

// #define QDEBUG 1
#ifdef QDEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif


#ifdef QDEBUG
static inline void logmsg(const char * msg, const Handle& h)
{
	LAZY_LOG_FINE << msg << std::endl
	              << (h == (Atom*) nullptr ?
	                  std::string("       (invalid handle)") :
	                  h->to_short_string("       "));
}

static inline void logmsg(const char * msg, const HandleSeq& hs)
{
	LAZY_LOG_FINE << msg << std::endl
	              << oc_to_string(hs, "       ");
}

static inline void logmsg(const char * msg, const PatternTermPtr& ptm)
{
	LAZY_LOG_FINE << msg << ptm->to_string("       ")
	              << ptm->getQuote()->to_short_string("       ");
}

static inline void logmsg(const char * msg, const PatternTermPtr& ptm,
                          bool rslt)
{
	LAZY_LOG_FINE << msg << ptm->to_string("       ")
	              << ptm->getQuote()->to_short_string("       ")
	              << "\n       Found: " << rslt;
}

static inline void logmsg(const char * msg, int a, int b)
{
	logger().fine(msg, a, b);
}

static inline void logmsg(const char * msg, size_t n)
{
	LAZY_LOG_FINE << msg << " " << n;
}

static inline void logmsg(const char * msg)
{
	LAZY_LOG_FINE << msg;
}
#else
static inline void logmsg(const char*, const PatternTermPtr&, bool) {}
static inline void logmsg(const char*, const PatternTermPtr&) {}
static inline void logmsg(const char*, const Handle&) {}
static inline void logmsg(const char*, const HandleSeq&) {}
static inline void logmsg(const char*, int, int) {}
static inline void logmsg(const char*, size_t) {}
static inline void logmsg(const char*) {}
#endif


/* ======================================================== */
/* Reset the current variable grounding to the last grounding pushed
 * onto the stack. */
#ifdef QDEBUG
   #define POPSTK(stack,soln) {         \
      OC_ASSERT(not stack.empty(),      \
           "Unbalanced stack " #stack); \
      soln = stack.top();               \
      stack.pop();                      \
   }
#else
   #define POPSTK(stack,soln) {         \
      soln = stack.top();               \
      stack.pop();                      \
   }
#endif

/* ======================================================== */

/// Compare a VariableNode in the pattern to the proposed grounding.
///
/// Handle hp is from the pattern clause.  By default, the code here
/// allows a variable to be grounded by itself -- this avoids a lot
/// of second-guessing involving alpha-converted variable names,
/// which get handled at a higher layer, which has access to the
/// entire clause. (The clause_match() callback, to be specific).
///
bool PatternMatchEngine::variable_compare(const Handle& hp,
                                          const Handle& hg)
{
	// If we already have a grounding for this variable, the new
	// proposed grounding must match the existing one. Such multiple
	// groundings can occur when traversing graphs with loops in them.
	auto gnd = var_grounding.find(hp);
	if (var_grounding.end() != gnd)
		return (gnd->second == hg);

	// VariableNode had better be an actual node!
	// If it's not then we are very very confused ...
	OC_ASSERT (hp->is_node(),
	           "Expected variable to be a node, got this: %s\n",
	           hp->to_short_string().c_str());

	// Else, we have a candidate grounding for this variable.
	// The variable_match() callback may implement some tighter
	// variable check, e.g. to make sure that the grounding is
	// of some certain type.
	if (not _pmc.variable_match(hp, hg)) return false;

	// Make a record of it. Cannot record GlobNodes here; they're
	// variadic.
	if (hp->get_type() != GLOB_NODE)
	{
		logmsg("Found grounding of variable:");
		logmsg("$$ variable:", hp);
		logmsg("$$ ground term:", hg);
		var_grounding[hp] = hg;
	}
	return true;
}

/* ======================================================== */

/// Compare an atom to itself.
///
bool PatternMatchEngine::self_compare(const PatternTermPtr& ptm)
{
	const Handle& hp = ptm->getHandle();
	if (not ptm->isQuoted()) var_grounding[hp] = hp;

	logmsg("Compare atom to itself:", ptm->getQuote());
	return true;
}

/* ======================================================== */

/// Compare two nodes, one in the pattern, one proposed grounding.
/// Return true if they match, else return false.
bool PatternMatchEngine::node_compare(const Handle& hp,
                                      const Handle& hg)
{
	// Call the callback to make the final determination.
	bool match = _pmc.node_match(hp, hg);
	if (match)
	{
		logmsg("Found matching nodes");
		logmsg("# pattern:", hp);
		logmsg("# match:", hg);
		if (hp != hg) var_grounding[hp] = hg;
	}
	return match;
}

/* ======================================================== */

/// If the two links are both ordered, its enough to compare them
/// "side-by-side". Return true if they match, else return false.
/// See `tree_compare` for a general explanation.
bool PatternMatchEngine::ordered_compare(const PatternTermPtr& ptm,
                                         const Handle& hg)
{
	const PatternTermSeq& osp = ptm->getOutgoingSet();
	const HandleSeq& osg = hg->getOutgoingSet();

	// The recursion step: traverse down the tree.
	depth ++;

	// If the pattern contains no globs, then the pattern and ground
	// must match exactly, with atoms in the outgoing sets pairing up.
	// If the pattern has globs, then more complex matching is needed;
	// a glob can match one or more atoms in a row. If there are no
	// globs, and the arity is mis-matched, then perform fuzzy matching.

	bool match = true;
	const Handle &hp = ptm->getHandle();
	if (ptm->hasGlobbyVar())
	{
		match = glob_compare(osp, osg);
	}
	else
	{
		size_t osg_size = osg.size();
		size_t osp_size = osp.size();

		// If the arities are mis-matched, do a fuzzy compare instead.
		if (osp_size != osg_size)
		{
			match = _pmc.fuzzy_match(ptm->getHandle(), hg);
		}
		else
		{
			// Side-by-side recursive compare.
			for (size_t i=0; i<osp_size; i++)
			{
				if (not tree_compare(osp[i], osg[i], CALL_ORDER))
				{
					match = false;
					break;
				}
			}
		}
	}

	depth --;
	logmsg("ordered_compare match?:", match);

	if (not match)
	{
		_pmc.post_link_mismatch(hp, hg);
		return false;
	}

	// If we've found a grounding, lets see if the
	// post-match callback likes this grounding.
	match = _pmc.post_link_match(hp, hg);
	if (not match) return false;

	// If we've found a grounding, record it.
	record_grounding(ptm, hg);

	return true;
}

/* ======================================================== */

/// Compare the contents of a Present term in the pattern to the
/// proposed grounding. The term `ptm` points at the Present term.
///
/// XXX FIXME: this is currently a weak stop-gap measure to handle
/// the special case of Present terms embedded in Choice terms.
/// Present terms that are NOT in a Choice are handled by the
/// do_next_clause() system, which assumes that Present terms happen
/// only as top-level clauses. Someday, someone should merge these
/// two mechanisms, so that this guy gets the sophistication of
/// the get_next_clause() mechanism.
bool PatternMatchEngine::present_compare(const PatternTermPtr& ptm,
                                         const Handle& hg)
{
	const PatternTermSeq& osp = ptm->getOutgoingSet();
	logmsg("present_compare");

	throw RuntimeException(TRACE_INFO,
		"Unexpecteed call to unimplemented function!!");

	return tree_compare(osp[0], hg, CALL_PRESENT);

/*
	for (const PatternTermPtr& pto: osp)
	{
		bool match = tree_compare(pto, hg, CALL_PRESENT);
		if (not match) return false;
	}
*/
	return true;
}

/* ======================================================== */

/// Compare a Choice term in the pattern to the proposed grounding.
/// The term `ptm` points at the Choice term.
///
/// Choice terms are multiple-choice links. As long as we can
/// can match one of the sub-expressions of the Choice term, then
/// the Choice term as a whole can be considered to be grounded.
///
bool PatternMatchEngine::choice_compare(const PatternTermPtr& ptm,
                                        const Handle& hg)
{
	const Handle& hp = ptm->getHandle();
	PatternTermSeq osp = ptm->getOutgoingSet();

	// _choice_state lets use resume where we last left off.
	size_t iend = osp.size();
	size_t icurr = curr_choice(ptm, hg);

	DO_LOG({LAZY_LOG_FINE << "tree_comp resume choice search at " << icurr
	              << " of " << iend << " of term=" << ptm->to_string()
	              << ", choose_next=" << _choose_next;})

	// XXX This is almost surely wrong... if there are two
	// nested choice links, then this will hog the steps,
	// and the deeper choice will fail.
	if (_choose_next)
	{
		icurr++;
		_choose_next = false; // we are taking a step, so clear the flag.
	}

	while (icurr<iend)
	{
		solution_push();
		const PatternTermPtr& hop = osp[icurr];

		DO_LOG({LAZY_LOG_FINE << "tree_comp choice " << icurr
		              << " of " << iend;})

		bool match;
		if (hop->isPresent())
			match = present_compare(hop, hg);
		else
			match = tree_compare(hop, hg, CALL_CHOICE);

		if (match)
		{
			// If we've found a grounding, lets see if the
			// post-match callback likes this grounding.
			match = _pmc.post_link_match(hp, hg);
			if (match)
			{
				// Even the stack, *without* erasing the discovered grounding.
				solution_drop();

				// If the grounding is accepted, record it.
				record_grounding(ptm, hg);

				_choice_state[ptm] = icurr;
				return true;
			}
		}
		else
		{
			_pmc.post_link_mismatch(hp, hg);
		}
		solution_pop();
		_choose_next = false; // we are taking a step, so clear the flag.
		icurr++;
	}

	// If we are here, we've explored all the possibilities already
	_choice_state.erase(ptm);
	return false;
}

/// Return the current choice state for the given pattern & ground
/// combination.
size_t PatternMatchEngine::curr_choice(const PatternTermPtr& ptm,
                                       const Handle& hg)
{
	auto cs = _choice_state.find(ptm);
	if (_choice_state.end() == cs)
	{
		_choose_next = false;
		return 0;
	}
	return cs->second;
}

bool PatternMatchEngine::have_choice(const PatternTermPtr& ptm,
                                     const Handle& hg)
{
	return 0 < _choice_state.count(ptm);
}

/* ======================================================== */
static int facto (int n) { return (n==1)? 1 : n * facto(n-1); };

/// Unordered link comparison
///
/// Compare two unordered links, side by side. In some ways, this is
/// similar to the ordered link compare: for a given, fixed permutation
/// of the unordered link, the compare is side by side.  However, if
/// that compare fails, the next permutation must then be tried, until
/// a match is found or all permutations are exhausted.  But there's a
/// problem: if there are multiple, nested unordered links, or if they
/// are peers (siblings) in the tree, then if one takes a step, the
/// other must not. Coordinating this is difficult, and requires a long
/// explanation. So here goes:
///
/*****************************************************************

How do unordered links work?
----------------------------
This is complicated, so we write it out.  When ascending from below (i.e.
from do_term_up()), unordered links may be found in two different
places: The parent term may be unordered, or the parent link may hold
another link (a sibling to us) that is unordered. Traversal needs to
handle both cases.  Thus, the upwards-movement methods (do_term_up(),
explore_up_branches(), etc.) are incapable of discovering unordered links,
as they cannot "see" the siblings.  Siblings can only be found during
tree-compare, moving downwards.  Thus, tree_compare must do a lot of
heavy lifting.

When comparing trees downwards, we have two situations we may be in:
  1) we may be asked to compare things exactly like last time,
     (take_step == false)
  2) we may be asked to try out the next permutation.
     (take_step == true)
Our behavior is thus controlled by the `take_step` flag.
We need to report back two bits of state: whether or not we found a match,
and whether or not there are more permutation possibilities to explore
(the `have_more` boolean flag).

The topmost routine to call tree_compare must *always* set _have_more=F
and _take_step=T before calling tree_compare.  This will cause
tree_compare to advance to the next matching permutation, or to run until
all permutations are exhausted, and no match was found.

Odometers
---------
Stepping through all possible permutations is "easy" for just one
unordered link. If there are more than one, then several difficulties
arise.  These are, roughly:

A) Two (or more) unordered links that are siblings to one another.
   These can be visualized as being in left-right order; they are
   strung together to form an "odometer". The odometer must spin as all
   odometers do: all possible permutations of the right-most link must
   be explored, before even one step of the left link can be taken.
   Then, once the left link takes a step, all possible permutations
   of the right-most link must be tried, again.  This is solved by
   having a `perm_to_step` pointer: it indicates whose turn it is to
   take one step.

B) Two (or more) unordered links are in a parent-chiled relationship.
   This is similar to the odometer situation, in that all permutations
   of the child must be explored before the parent can take a step.
   It could be treated much as the above, with the understanding that
   intervening ordered links mean that state needs to be held on stack.

C) Nested odometers: that is, combinations of case A) and case B).
   This is where most of the complexity comes in: we have to track
   both parent-child relationships, as well as sibling relationships.
   Part of the problem is that we don't known, a priori, whether or
   not there is more than on unordered link, or not, until we hit it.
   If there is more than one, its not immediately obvious if it is a
   sibling or a descendant. (Perhaps some of this complexity could
   be avoided during pattern compilation, i.e. when PatternLink gets
   created: we could analyze the structure up front, and then make
   use of it during traversal. This is not done right now (2019).)

There's a final bit of complexity: we might be hitting unordered links
for the first time, as we traverse up from below (e.g. by tracing from
a joining variable in a different clause) or we might be traversing
from above. The order in which steps are taken depend on this traversal
stack: thus, its alwways the case that permutations of links higher in
the stack are explored first, before those lower in the stack.


******************************************************************/

bool PatternMatchEngine::unorder_compare(const PatternTermPtr& ptm,
                                         const Handle& hg)
{
	const Handle& hp = ptm->getHandle();
	const HandleSeq& osg = hg->getOutgoingSet();
	const PatternTermSeq& osp = ptm->getOutgoingSet();
	size_t arity = osp.size();

	// Globs not allowed in this function.
	OC_ASSERT(not ptm->hasAnyGlobbyVar());

	// They've got to be the same size, at the least!
	// unless there are globs in the pattern
	if (osg.size() != arity)
		return _pmc.fuzzy_match(hp, hg);

	// Either we're going to take a step; or we aren't.
	// If we're not taking a step, then there are unexplored
	// permutations.
	OC_ASSERT (not (_perm_take_step and _perm_have_more),
	           "Impossible situation! BUG!");

	// Place the parent unordered link ("podo") where the child
	// unordered link can find it. Save the old parent on stack.
	PermOdo save_podo = _perm_podo;
	_perm_podo = _perm_odo;

	// _perm_state lets use resume where we last left off.
	Permutation mutation = curr_perm(ptm);

	// Likewise, pick up the odometer state where we last left off.
	if (_perm_odo_state.find(ptm) != _perm_odo_state.end())
		_perm_odo = _perm_odo_state.find(ptm)->second;
	else
		_perm_odo.clear();

	// If we are here, we've got possibilities to explore.
#ifdef QDEBUG
	int num_perms = 0;
	if (logger().is_fine_enabled())
	{
		num_perms = facto(mutation.size());
		logger().fine("tree_comp RESUME unordered search at %d of %d of term=%s "
		              "take_step=%d have_more=%d step_this=%d\n",
		              _perm_count[ptm] + 1, num_perms,
		              ptm->to_string().c_str(), _perm_take_step,
		              _perm_have_more, ptm == _perm_to_step);
	}
#endif
	do
	{
		bool match = true;
		solution_push();

		// If we've been told to take a step, then take it now.
		if (_perm_take_step and ptm == _perm_to_step)
			goto take_next_step;

		// If we are not taking a step, then see if we've got a tree
		// match. This is more or less just like the ordered-link
		// comparison: pair up the outgoing sets.
		DO_LOG({LAZY_LOG_FINE << "tree_comp explore unordered perm "
		              << _perm_count[ptm] +1 << " of " << num_perms
		              << " of term=" << ptm->to_string();})

		for (size_t i=0; i<arity; i++)
		{
			if (not tree_compare(mutation[i], osg[i], CALL_UNORDER))
			{
				match = false;
				break;
			}
		}

		// These flags might have been (mis-)set by the callees...
		// which would be very confusing.
		OC_ASSERT(not (_perm_take_step and _perm_have_more),
		          "This shouldn't happen. Impossible situation! BUG!");

		// We are not the ones who are taking the step. Just report
		// the tree-comparison results, as found.
		if (_perm_take_step and ptm != _perm_to_step)
		{
			DO_LOG({LAZY_LOG_FINE << "DO NOT step; stepper="
			        << _perm_to_step->to_string() << " so just repeat "
			        << _perm_count[ptm] + 1
			        << " of " << num_perms
			        << " for term=" << ptm->to_string();})
			// Balance the push above.
			solution_drop();
			_perm_odo = _perm_podo;
			_perm_podo = save_podo;
			return match;
		}

		if (match)
		{
			// If we've found a grounding, lets see if the
			// post-match callback likes this grounding.
			match = _pmc.post_link_match(hp, hg);
			if (match)
			{
				// Even the stack, *without* erasing the discovered grounding.
				solution_drop();

				// If the grounding is accepted, record it.
				record_grounding(ptm, hg);

				// Handle case 5&7 of description above.
				DO_LOG({LAZY_LOG_FINE << "Good permutation "
				              << _perm_count[ptm] + 1
				              << " of " << num_perms
				              << " for term=" << ptm->to_string();})
				_perm_state[ptm] = mutation;
				_perm_have_more = true;
				_perm_go_around = false;
				_perm_odo_state[ptm] = _perm_odo;
				_perm_odo = _perm_podo;
				_perm_podo = save_podo;
				return true;
			}
		}
		else
		{
			_pmc.post_link_mismatch(hp, hg);
		}

		// Odometer code. If there are multiple unordered links,
		// then we might be the one that went all the way around
		// first, and we exhausted all permutations. But then
		// someone else took a step, so we have to go around back
		// to the beginning, and try all over again.
		if (_perm_go_around)
		{
			// So first, take a look at *all* of the "wheels" in the
			// odometer. If some of them have not yet gone all the
			// way around, then there's more work to do.
			bool not_done = false;
			for (const auto& odo: _perm_odo)
			{
				if (odo.first == ptm) continue;
				DO_LOG({LAZY_LOG_FINE << "Maybe go PERM odo "
				                      << odo.first->to_string()
				                      << " done=" << odo.second;})
				if (not odo.second) { not_done = true; break; }
			}

			if (not_done)
			{
				// There are more unexplored permutations...
				DO_LOG({LAZY_LOG_FINE << "GO around " << ptm->to_string();})
				_perm_go_around = false;
				_perm_have_more = true;
				_perm_state[ptm] = mutation;
				solution_pop();
				_perm_odo_state[ptm] = _perm_odo;
				_perm_odo = _perm_podo;
				_perm_podo = save_podo;
				return false;
			}
		}

		// If we are here, then there was no match. Just print some
		// debug info, and ... take a step.
		DO_LOG({LAZY_LOG_FINE << "Bad permutation "
		              << _perm_count[ptm] + 1
		              << " of " << num_perms
		              << " for term=" << ptm->to_string();})

take_next_step:
		_perm_take_step = false; // we are taking the step, so clear the flag.
		_perm_have_more = false; // start with a clean slate...
		solution_pop();

		// Clear out the permutation state of any unordered links
		// that lie below us.  When we revisit these, we will want to
		// start with a clean slate, every time.
		for (auto it =  _perm_state.begin(); it != _perm_state.end(); )
		{
			if (it->first->isDescendant(ptm))
			{
				_perm_count.erase(it->first);
				it = _perm_state.erase(it);
			}
			else
				it ++;
		}

		// Clear the odometer that we maintain that records the state
		// of the unordered links *below* us.
		_perm_odo.clear();
		_perm_odo_state[ptm] = _perm_odo;

#if ODO_CLEANUP_NOT_NEEDED
		// Like the above, cleanup the odometer state of any unordered
		// links that lie below us.
		for (auto it =  _perm_odo_state.begin(); it != _perm_odo_state.end(); )
		{
			if (it->first->isDescendant(ptm))
				it = _perm_odo_state.erase(it);
			else
				it ++;
		}
#endif
#ifdef QDEBUG
		if (logger().is_fine_enabled())
			_perm_count[ptm] ++;
#endif
	} while (std::next_permutation(mutation.begin(), mutation.end(),
	         std::less<PatternTermPtr>()));

	// If we are here, we've explored all the possibilities already
	DO_LOG({LAZY_LOG_FINE << "Exhausted all permutations of term="
	             << ptm->to_string();})
	_perm_state.erase(ptm);
	_perm_count.erase(ptm);
	_perm_have_more = false;
	_perm_to_step = nullptr;

	if (0 < _perm_step_saver.size())
	{
		POPSTK(_perm_step_saver, _perm_to_step);
		_perm_have_more = true;
		_perm_go_around = true;
	}

	// Since we're done, let any unordered links above us know
	// that we've wrapped around. And then reset them back.
	_perm_podo[ptm] = true;
	_perm_odo = _perm_podo;
	_perm_podo = save_podo;

	return false;
}

/// Return the saved unordered-link permutation for this
/// particular point in the tree comparison (i.e. for the
/// particular unordered link hp in the pattern.)
PatternMatchEngine::Permutation
PatternMatchEngine::curr_perm(const PatternTermPtr& ptm)
{
	auto ps = _perm_state.find(ptm);
	if (_perm_state.end() != ps)
		return ps->second;

	// Do not have a currently-running permuation. Create a new one.
	DO_LOG({LAZY_LOG_FINE << "tree_comp FRESH START unordered term="
	                      << ptm->to_string();})
	Permutation perm = ptm->getOutgoingSet();
	// Sort into explicit std::less<PatternTermPtr>() order, as
	// otherwise std::next_permutation() will miss some perms.
	sort(perm.begin(), perm.end(), std::less<PatternTermPtr>());
	_perm_take_step = false;

	// This will become the permutation to step, next time we
	// have to step. Meanwhile, save to old stepper, so that
	// we can resume it when all perms of this one are exhausted.
	if (nullptr != _perm_to_step)
		_perm_step_saver.push(_perm_to_step);
	_perm_to_step = ptm;

	// If there are unordered links above us, we need to tell them
	// about our existence, and let them know that we haven't been
	// explored yet. We tell them by placing ourselves into their
	// odometer.
	if (_perm_podo.find(ptm) == _perm_podo.end())
		_perm_podo[ptm] = false;

	return perm;
}

/// Return true if there are more permutations to explore.
/// Else return false.
bool PatternMatchEngine::have_perm(const PatternTermPtr& ptm)
{
	if (_perm_state.end() == _perm_state.find(ptm))
		return false;
	return true;
}

void PatternMatchEngine::perm_push(void)
{
	_perm_stack.push(_perm_state);
#ifdef QDEBUG
	if (logger().is_fine_enabled())
		_perm_count_stack.push(_perm_count);
#endif

	_perm_stepper_stack.push(_perm_to_step);
	_perm_take_stack.push(_perm_take_step);
	_perm_more_stack.push(_perm_have_more);
	_perm_breakout_stack.push(_perm_breakout);

	_perm_odo_stack.push(_perm_odo_state);
}

void PatternMatchEngine::perm_pop(void)
{
	POPSTK(_perm_stack, _perm_state);
#ifdef QDEBUG
	if (logger().is_fine_enabled())
		POPSTK(_perm_count_stack, _perm_count);
#endif

	POPSTK(_perm_stepper_stack, _perm_to_step)
	POPSTK(_perm_take_stack, _perm_take_step);
	POPSTK(_perm_more_stack, _perm_have_more);
	POPSTK(_perm_breakout_stack, _perm_breakout);

	// XXX should we be clearing ... or popping this flag?
	_perm_go_around = false;

	POPSTK(_perm_odo_stack, _perm_odo_state);
}

/* ======================================================== */

/// Compare the outgoing sets of two trees side-by-side, where
/// the pattern contains at least one GlobNode.
bool PatternMatchEngine::glob_compare(const PatternTermSeq& osp,
                                      const HandleSeq& osg)
{
	bool match = true;
	size_t osp_size = osp.size();
	size_t osg_size = osg.size();

	size_t ip = 0;
	size_t jg = 0;

	GlobGrd glob_grd;
	GlobPosStack glob_pos_stack;

	// Common things that need to be done when backtracking.
	bool backtracking = false;
	bool cannot_backtrack_anymore = false;
	auto backtrack = [&](bool is_glob)
	{
		backtracking = true;

		// If we are looking at a glob right now and fail
		// to ground it, pop the stack, go back to the
		// previous one and try again.
		if (is_glob)
		{
			// Erase the grounding record of the glob before
			// popping it out from the stack.
			glob_grd.erase(glob_pos_stack.top().first);

			glob_pos_stack.pop();
			_glob_state[osp] = {glob_grd, glob_pos_stack};
		}

		// See where the previous glob is and try again
		// from there.
		if (0 == glob_pos_stack.size())
			cannot_backtrack_anymore = true;
		else
		{
			ip = glob_pos_stack.top().second.first;
			jg = glob_pos_stack.top().second.second;

			solution_pop();
		}
	};

	// Common things that need to be done when a match
	// is found for a glob.
	auto record_match = [&](const PatternTermPtr& glob,
	                        const HandleSeq& glob_seq)
	{
		solution_push();

		glob_grd[glob] = glob_seq.size();
		_glob_state[osp] = {glob_grd, glob_pos_stack};

		Handle glp(createLink(std::move(glob_seq), LIST_LINK));
		var_grounding[glob->getHandle()] = glp;

		logmsg("Found grounding of glob:");
		logmsg("$$ glob:", glob->getQuote());
		logmsg("$$ ground term:", glp);
	};

	// Common things needed to be done when it's not a match.
	auto mismatch = [&]()
	{
		match = false;
		_glob_state.erase(osp);
	};

	// Resume the matching from a previous state.
	// i.e. we had successfully grounded osp to osg, but it
	// turns out the groundings do not satisfy some other terms
	// in the same pattern, so we try again and see if the globs
	// in osp can be grounded differently.
	auto r = _glob_state.find(osp);
	if (r != _glob_state.end())
	{
		backtracking = true;

		solution_pop();
		glob_grd = r->second.first;
		glob_pos_stack = r->second.second;
		ip = glob_pos_stack.top().second.first;
		jg = glob_pos_stack.top().second.second;
	}

	while (ip<osp_size)
	{
		// Reject if no more backtracking is possible.
		if (cannot_backtrack_anymore)
		{
			mismatch();
			break;
		}

		if (osp[ip]->isGlobbyVar())
		{
			HandleSeq glob_seq;
			const PatternTermPtr& glob(osp[ip]);
			const Handle& ohp(glob->getHandle());

			// A glob may appear more than once in the pattern,
			// so check if that's the case. If we have already
			// grounded it previously, make sure the grounding
			// here is consistent with the earlier grounding.
			auto vg = var_grounding.find(ohp);
			if (not backtracking and vg != var_grounding.end())
			{
				bool no_match = false;

				// The grounding of a glob is wrapped in a ListLink,
				// so compare the outgoing set of it.
				for (const Handle& h : vg->second->getOutgoingSet())
				{
					if (jg >= osg_size or h != osg[jg])
					{
						no_match = true;
						break;
					}
					jg++;
				}

				// Backtrack if the previous grounding does not fit here.
				if (no_match) backtrack(false);
				// Otherwise, it's a match, move on.
				else ip++;

				continue;
			}

			// No need to push to stack if we are backtracking.
			if (backtracking)
			{
				// Reset the flag, so that the next glob will be
				// pushed to the stack.
				backtracking = false;
			}
			else
			{
				// XXX why are we not doing any checks to see if the
				// grounding meets the variable constraints?
				glob_pos_stack.push({glob, {ip, jg}});
				_glob_state[osp] = {glob_grd, glob_pos_stack};
			}

			// First of all, see if we have seen this glob in
			// previous iterations.  Huh ??? Why???
			size_t last_grd = SIZE_MAX;
			auto gi = glob_grd.find(glob);
			if (gi != glob_grd.end())
			{
				last_grd = gi->second;
			}

			// If the lower bound of the interval is zero, the glob
			// can be grounded to nothing.
			if (_variables->is_lower_bound(ohp, 0))
			{
				// Try again, find another glob that can be grounded
				// in a different way. (we are probably resuming the
				// search from a previous state)
				if (0 == last_grd)
				{
					backtrack(true);
					continue;
				}

				// On the other hand, if we failed to ground this glob
				// in the previous iteration, just let it ground to
				// nothing (as long as it is not the last one in osp),
				// and we are done with it.
				if (1 == last_grd and ip+1 < osp_size)
				{
					record_match(glob, glob_seq);
					ip++;
					continue;
				}

				// If we have already gone through all the atoms of
				// the candidate at this point, we are done.
				if (jg >= osg_size)
				{
					record_match(glob, glob_seq);
					ip++;
					continue;
				}

				// Just in case, if the upper bound is zero...
				// XXX Huh ???
				if (not _variables->is_upper_bound(ohp, 1))
				{
					record_match(glob, glob_seq);
					ip++;
					continue;
				}
			}

			// If we are here, that means we have to ground the glob to
			// at least one atom.

			// Try again if we have already gone through everything in osg.
			if (jg >= osg_size)
			{
				backtrack(true);
				continue;
			}

			// Try to match as many atoms as possible.
			// Iterate from the maximum allowed number of match to the minimum.
			// Till valid match is found.
			const GlobInterval& interval = _variables->get_interval(ohp);
			for (auto i = std::min({interval.second, osg_size - jg, last_grd - 1});
			     i >= interval.first; i--)
			{
				HandleSeq osg_seq = HandleSeq(osg.begin() + jg,
				                              osg.begin() + i + jg);
				Handle wr_h = createLink(osg_seq, LIST_LINK);

				auto tc = tree_compare(glob, wr_h, CALL_GLOB);
				if (tc)
				{
					glob_seq.insert(glob_seq.end(),
					                osg_seq.begin(),
					                osg_seq.end());
					jg += i - 1;
					break;
				}
			}

			if (0 == glob_seq.size())
			{
				backtrack(true);
				continue;
			}

			// Try again if there is no more osp to be explored but
			// we haven't finished osg yet.
			if (ip+1 == osp_size and jg+1 < osg_size)
			{
				backtrack(true);
				continue;
			}

			// If we are here, we've got a match; record the glob.
			record_match(glob, glob_seq);

			// Try to match another one.
			ip++; jg++;
		}
		else
		{
			// If we are here, we are not comparing to a glob.

			// Try again if we have already gone through all the
			// atoms in osg.
			if (jg >= osg_size)
			{
				backtrack(false);
				continue;
			}

			// Try again if we reached the end of osp, but there
			// are two or more atoms to be matched in osg (because
			// maybe we can match one atom with the final atom of the
			// pattern, but we certainly cannot match two or more.)
			if (ip+1 == osp_size and jg+1 < osg_size)
			{
				backtrack(false);
				continue;
			}

			// Try again if this pair is not a match.
			if (not tree_compare(osp[ip], osg[jg], CALL_ORDER))
			{
				backtrack(false);
				continue;
			}

			ip++; jg++;
		}
	}

	return match;
}

/* ======================================================== */
/// "Sparse" matching. This handles the situation of an unordered link
/// that contains a (single) glob node. It's sparse because the
/// unordered pattern only specifies a handful of terms to be matched;
/// everything unmatched at the end gets dumped into the glob. The
/// code handles only one glob, because two or more "don't make sense":
/// there's just a pile of left-overs; partitioning them ito two or more
/// sets is a distinct operation. So we don't do that.
///
/// The prototypical use for this search is chemistry, where one is
/// searching for a small functional group inside a molecule, and don't
/// care about the rest, the moiety (which can be very large; thus
/// "sparse".)
//
// XXX The current implementation is a brute-force search, and is highly
// inefficient for truly sparse searches. A (vastly) superior search
// woudld be to obtain the connected components in the search set, and
// try to ground those.  This avoids the pointless odometer spinning.
//
// There's also a bug/limitation: this code does not support nested
// sparse searches.  It should not be hard to implement: just copy the
// general algo used in unordered link permutations, which does handle
// nesting correctly. But right now, no users envisioned, so... not
// supported.

#ifdef QDEBUG
static void prt_sparse_odo(const std::vector<int>& sel,
                           int szg, const char *msg)
{
	std::string buf = msg;
	for (size_t i=0; i<sel.size(); i++)
	{
		buf += " ";
		buf += std::to_string(sel[i]);
	}
	buf += " of " + std::to_string(szg);
	logger().fine("%s", buf.c_str());
}
#endif // QDEBUG


/// Return true if there are more rotor postions to explore.
/// Else return false.
bool PatternMatchEngine::have_more_rotors(const PatternTermPtr& ptm)
{
	if (_sparse_glob.end() == _sparse_glob.find(ptm))
		return false;
	return true;
}

/// Initialize the sparse-state odometer for the first time.
bool PatternMatchEngine::setup_rotors(const PatternTermPtr& ptm,
                                      const Handle& hg)
{
	auto ss = _sparse_glob.find(ptm);
	if (_sparse_glob.end() != ss)
		return true;

	// If there's no glob, then we are starting from scratch.
	// Set things up.
	DO_LOG({LAZY_LOG_FINE << "tree_comp NEW SETUP sparse term="
	                      << ptm->to_string();})

	// XXX TODO The logic here should be updated to resemble that
	// in curr_perm(), which deals correctly with nested permutations
	// of unordered patterns. For just right now, we are not
	// implementing nested sparse links, mostly because I'm too lazy
	// to write the unit tests. Sorry!  The fix is easy, though: do
	// what `curr_perm()` does.

	const PatternTermSeq& osp = ptm->getOutgoingSet();
	const HandleSeq& osg = hg->getOutgoingSet();

	// Fuzzy compares are not supported. hg MUST have an arity at
	// least as big as the pattern, minus one, because glob might
	// be empty. Exact glob size bound checked shortly below.
	size_t osg_size = osg.size();
	size_t osp_size = osp.size();
	if (osg_size < osp_size-1) return false;

	// Find the glob in the pattern, and everything else.
	PatternTermPtr glob;
	PatternTermSeq pats;
	for (size_t i = 0; i< osp_size; i++)
	{
		Type pty = osp[i]->getHandle()->get_type();
		if (GLOB_NODE == pty)
		{
			glob = osp[i];
			continue;
		}
		pats.push_back(osp[i]);
	}

	// Verify that the glob size bounds are correct.
	size_t glsz = osg_size - osp_size + 1;
	const Handle& gloh(glob->getHandle());
	if (not _variables->is_lower_bound(gloh, glsz))
		return false;
	if (not _variables->is_upper_bound(gloh, glsz))
		return false;

	int szg = (int) osg_size;
	int szp = (int) pats.size();

#ifdef QDEBUG
	for (int it=0; it < szp; it++)
	{
		const PatternTermPtr& pto = pats[it];
		logger().fine("Terms for sparse %d pto=%s\n", it,
		               pto->getHandle()->to_short_string().c_str());
	}
	logger().fine("");
	for (int ig=0; ig < szg; ig++)
	{
		const Handle& hog = osg[ig];
		logger().fine("Gnds for sparse are %d gnd=%s\n", ig,
		               hog->to_short_string().c_str());
	}
#endif

	_sparse_take_step = false;

	// Set up the sparse odometer for the first time.
	Rotors rotors(szp, -1);
	for (int it=0; it<szp; it++)
	{
		logmsg("Setting up sparse rotor %d  (%d)\n", it, -1);

		const PatternTermPtr& pto = pats[it];

		int ig;
		for (ig = 0; ig < szg; ig++)
		{
			solution_push();  // Each tree_compare writes into soln set.
			logmsg("Test sparse rotor %d with proposed ground %d", it, ig);
			const Handle& hog = osg[ig];
			bool match = tree_compare(pto, hog, CALL_SPARSE);
			if (match)
			{
				solution_drop();
				rotors[it] = ig;
				logmsg("Sparse rotor setup %d grounded at %d", it, ig);
				break;
			}
			solution_pop();
		}

		// Staight out of the chute, we got nothing!
		if (ig >= szg) return false;
	}

	// Initialize rotor state.
	_sparse_glob.emplace(ptm, glob->getHandle());
	_sparse_term.emplace(ptm, pats);
	_sparse_state.emplace(ptm, rotors);

	DO_LOG(prt_sparse_odo(rotors, szg, "Initialized sparse odo:");)

	return true;
}

/// Compare the outgoing sets of two trees side-by-side, where
/// the pattern is unordered and contains at least one GlobNode.
/// See `explore_sparse_branches()` for more info.
/// Conceptually similar to `unordered_compare()` or `glob_compare()`
/// in that we have a particular compare state that should be used
/// when the comparison is performed.
bool PatternMatchEngine::sparse_compare(const PatternTermPtr& ptm,
                                        const Handle& hg)
{
	bool match = setup_rotors(ptm, hg);
	if (not match) return false;

	logmsg("Enter sparse_compare");

	// _sparse_state lets use resume where we last left off.
	Rotors rotors = _sparse_state[ptm];
	const PatternTermSeq& pats = _sparse_term[ptm];

	const HandleSeq& osg = hg->getOutgoingSet();
	int szg = (int) osg.size();
	int szp = (int) pats.size();

	// If not taking a step, set up the grounding as we last knew it.
	if (not _sparse_take_step)
	{
		for (int it=0; it < szp; it++)
		{
			const PatternTermPtr& pto = pats[it];
			const Handle& hog = osg[rotors[it]];
			bool match = tree_compare(pto, hog, CALL_SPARSE);
			OC_ASSERT(match, "Internal Error: unable to restore sparse state!");
		}
		return record_sparse(ptm, hg);
	}

	// Set up the grounding as we last knew it; all but the last one.
	int it;
	for (it=0; it < szp-1; it++)
	{
		const PatternTermPtr& pto = pats[it];
		const Handle& hog = osg[rotors[it]];
		solution_push();
		tree_compare(pto, hog, CALL_SPARSE);
	}
	solution_push(); // isolate the last rotor.

	it = szp - 1;
	DO_LOG(prt_sparse_odo(rotors, szg, "Pre-stepper sparse odo:");)
	while (0 <= it)
	{
		const PatternTermPtr& pto = pats[it];
		int ig = rotors[it];

		logmsg("Stepping sparse rotor %d (was %d)\n", it, ig);

		// As we revisit each sparse rotor, we want any unordered
		// links that lie underneath to take a step.  So that the
		// other permutations get tried, before we move on the next
		// rotor. (Here, `rotors[it]` is the rotor for `it`.)
		if (pto->hasUnorderedLink() and _perm_have_more)
		{
			_perm_have_more = false;
			_perm_take_step = true;
		}
		else if (pto->hasChoice())
		{
			// I think this is correct. But it's untested! XXX verify!
			_choose_next = true;
		}
		else  // this is an ordered link. Just plain-old step.
			ig++;

		for (; ig < szg; ig++)
		{
			solution_push();
			logmsg("Test sparse rotor=%d w/ gnd by=%d", it, ig);
			const Handle& hog = osg[ig];
			bool match = tree_compare(pto, hog, CALL_SPARSE);
			if (match)
			{
				solution_drop();
				rotors[it] = ig;
				logmsg("Spun sparse rotor %d to new ground %d", it, ig);

				if (szp - 1 == it)
				{
					DO_LOG(prt_sparse_odo(rotors, szg, "Post-step sparse odo:");)
					for (int j=0; j<szp; j++) solution_drop();

					// Save the new state.
					_sparse_state.insert_or_assign(ptm, rotors);
					return record_sparse(ptm, hg);
				}
				it ++;
				rotors[it] = 0;
				solution_push();
				break;
			}
			solution_pop();
		}

		// If the above wrapped, back up and try again.
		if (ig >= szg)
		{
			it --;
			solution_pop();  // discard old current rotor
			solution_pop();  // discard new current rotor
			solution_push(); // restart with a clean slate.
		}
	}

	logmsg("Odo for sparse is exhausted");
	_sparse_take_step = false;
	_sparse_glob.clear();
	_sparse_state.clear();
	_sparse_term.clear();
	return false;
}

/// Record a successful sparse match. After matching the required terms
/// in the pattern, everything else left over is pushed into the glob.
/// Then the term as a whole, and the grounding, are recorded.
bool PatternMatchEngine::record_sparse(const PatternTermPtr& ptm,
                                       const Handle& hg)
{
	// If we've found a grounding, let's see if the
	// post-match callback likes this grounding.
	const Handle& hp = ptm->getHandle();
	bool match = _pmc.post_link_match(hp, hg);
	if (not match) return false;

	Handle glob = _sparse_glob[ptm];

	// Everything in osg that did NOT show up in the pattern
	// must necessarily be a part of the glob.
	HandleSet gnds;
	for (const PatternTermPtr& otp: ptm->getOutgoingSet())
	{
		const Handle& gnd = var_grounding[otp->getHandle()];
		if (gnd)
			gnds.insert(gnd);
	}

	HandleSeq rest;
	for (const Handle& otg: hg->getOutgoingSet())
	{
		if (gnds.end() != gnds.find(otg)) continue;
		rest.push_back(otg);
	}

	Handle glp(createLink(std::move(rest), UNORDERED_LINK));
	var_grounding[glob] = glp;

	// If we've found a grounding, record it.
	record_grounding(ptm, hg);
	return true;
}

/* ======================================================== */
/**
 * tree_compare compares two trees, side-by-side.
 *
 * Compare two incidence trees, side-by-side.  The incidence tree is
 * given by following the "outgoing set" of the links appearing in the
 * tree.  The incidence tree is the so-called "Levi graph" of the
 * hypergraph.  The first arg should be a handle to a term in the
 * pattern, while the second arg is a handle to a candidate grounding.
 * The pattern (template) clause is compared to the candidate grounding,
 * returning true if there is a match, else return false.
 *
 * The comparison is recursive, so this method calls itself on each
 * subtree (term) of the template term, performing comparisons until a
 * match is found (or not found).
 *
 * The pattern clause may contain quotes (QuoteLinks), which signify
 * that what follows must be treated as a literal (constant), rather
 * than being interpreted.  Thus, quotes can be used to search for
 * expressions containing variables (since a quoted variable is no
 * longer a variable, but a constant).  Quotes can also be used to
 * search for GroundedPredicateNodes (since a quoted GPN will be
 * treated as a constant, and not as a function).  Quotes can be nested,
 * only the first quote is used to escape into the literal context,
 * and so quotes can be used to search for expressions containing
 * quotes.  It is assumed that the QuoteLink has an arity of one, as
 * its quite unclear what an arity of more than one could ever mean.
 *
 * This method has side effects. The main one is to insert variable
 * groundings and term groundings into `var_grounding` when grounded
 * variables and grounded terms are discovered in the pattern. (A term
 * is gounded when all variables in it are grounded). This is done
 * progressively, so that earlier groundings will be recorded even if
 * later ones fail. Thus, in order to use this method safely, the caller
 * must make a temp copy of `var_grounding`, and restore the temp if
 * there is no match.
 */
bool PatternMatchEngine::tree_compare(const PatternTermPtr& ptm,
                                      const Handle& hg,
                                      Caller caller)
{
	const Handle& hp = ptm->getHandle();

	// Do we already have a grounding for this? If we do, and the
	// proposed grounding is the same as before, then there is
	// nothing more to do.
	auto gnd = var_grounding.find(hp);
	if (gnd != var_grounding.end()) return (gnd->second == hg);

	Type tp = hp->get_type();

	// If the pattern is a DefinedSchemaNode, we need to substitute
	// its definition. XXX TODO. Hmm. Should we do this at runtime,
	// i.e. here, or at static-analysis time, when creating the PatternLink?
	if (DEFINED_SCHEMA_NODE == tp and not ptm->isQuoted())
		throw RuntimeException(TRACE_INFO, "Not implemented!!");

	if (ptm->isBoundVariable())
		return variable_compare(hp, hg);

	// If they're the same atom, then clearly they match....
	// if it doesn't contain variables, and if it isn't evaluatable.
	// -- If it contains variables, then these need to be grounded.
	//    This check will be applied later, at the clause level.
	// -- If it contains black-box evaluatables e.g. GPN's, then the
	//    evaluation may have side-effects (e.g. send a message) or
	//    it might depend on external state (e.g. SequenceUTest).
	if ((hp == hg) and not ptm->hasAnyEvaluatable())
		return self_compare(ptm);

	// If this is some other rando variable that is not part of
	// search pattern, i.e. if is is a scoped variable, then
	// accept a match to any other alpha-equivalent variable.
	// XXX FIXME - this is not very elegant. We should probably
	// have a distinct `scoped_link_compare()` function to handle
	// this. Right now, the scope_match() callback uses a rather
	// screwy and indirect trick to check alpha conversion.
	if (VARIABLE_NODE == tp and not ptm->isQuoted())
		return _pmc.scope_match(hp, hg);

	// If both are nodes, compare them as such.
	if (hp->is_node() and hg->is_node())
		return node_compare(hp, hg);

	// Choice terms are multiple-choice links. As long as we can
	// can match one of the sub-expressions of the Choice, then
	// the Choice term as a whole can be considered to be grounded.
	// Note, we must do this before the fuzzy_match below, because
	// hg might be a node (i.e. we compare a choice of nodes to one
	// node).
	if (ptm->isChoice())
		return choice_compare(ptm, hg);

	// If they're not both links, then it is clearly a mismatch.
	if (not (hp->is_link() and hg->is_link())) return _pmc.fuzzy_match(hp, hg);

	// Let the callback perform basic checking.
	bool match = _pmc.link_match(ptm, hg);
	if (not match) return false;

	logmsg("tree depth:", depth);
	logmsg("tree_compare:", hp);
	logmsg("to:", hg);

	// If the two links are both ordered, its enough to compare
	// them "side-by-side".
	if (2 > ptm->getArity() or not ptm->isUnorderedLink())
		return ordered_compare(ptm, hg);

	// If we are here, we are dealing with an unordered link.
	// If ptm is unordered and has a glob in it, then a sparse compare.
	if (ptm->hasGlobbyVar())
		return sparse_compare(ptm, hg);

	// No glob vars in ptm. There might still be some deeper in.
	return unorder_compare(ptm, hg);
}

/* ======================================================== */

/// explore_up_branches -- look for groundings for the given term.
///
/// The argument passed to this function is a clause that needs to be
/// grounded. One of this clause's subterms has already been grounded:
/// the subterm is in `ptm`, and the corresponding grounding is
/// in `hg`.  Thus, if the clause is going to be grounded, it will
/// be grounded by some atom in the incoming set of `hg`. Viz, we are
/// walking upwards in these trees, in lockstep.
///
/// This method wraps the major branch-point of the entire pattern
/// matching process. Each element of the incoming set is the start of
/// a different possible branch to be explored; each one might yield
/// a grounding. Thus, when backtracking, after a failed grounding in
/// one branch, we backtrack to here, and try another branch. When
/// backtracking, all state must be popped and pushed again, to enter
/// the new branch. We don't pushd & pop here, we push-n-pop in the
/// explore_unordered_branches() method.
///
/// This method is part of a recursive chain that only terminates
/// when a grounding for *the entire pattern* was found (and the
/// grounding was accepted) or if all possibilities were exhaustively
/// explored.  Thus, this returns true only if entire pattern was
/// grounded.
///
bool PatternMatchEngine::explore_up_branches(const PatternTermPtr& ptm,
                                             const Handle& hg,
                                             const PatternTermPtr& clause)
{
	const PatternTermPtr& parent(ptm->getParent());

	if (clause == parent and clause->hasEvaluatable())
		OC_ASSERT(false, "Error: Unexpected situation!\n");

	// We may have walked up to the top of an evaluatable term.
	// At this time, we only handle IdenticalLinks.
	if (parent->isIdentical())
		return explore_clause_identical(ptm, hg, clause);

	// Check if its molecular chemistry.
	// This avoids the accidental call to upglob below.
	// This could be optimized to call upord if everything
	// else is ordered ... XXX FIXME ...
	if (parent->isUnorderedLink() and parent->hasGlobbyVar())
		return explore_upund_branches(ptm, hg, clause);

	// Check if the pattern has globs in it.
	// XXX I'm not convinced this is right, if there are mixtures
	// of unordered and globby links in different places...
	if (parent->hasAnyGlobbyVar())
		return explore_upglob_branches(ptm, hg, clause);

	if (ptm->hasUnorderedLink())
		return explore_upund_branches(ptm, hg, clause);

	return explore_upord_branches(ptm, hg, clause);
}

/// Same as explore_up_branches(), handles the case where `ptm` is an
/// ordered node with no GlobNodes in it. This is a straightforward
/// loop over the incoming set, and nothing more. Well, something more;
/// it includes an optimization for a fast path up the search chain.
bool PatternMatchEngine::explore_upord_branches(const PatternTermPtr& ptm,
                                                const Handle& hg,
                                                const PatternTermPtr& clause)
{
	// Move up the solution graph, looking for a match.
	const PatternTermPtr& parent(ptm->getParent());
	Type t = parent->getHandle()->get_type();

	// Optimization. Short-circuit the search.
	// If the parent pattern term doesn't have any other bound
	// variables, aside from `ptm` which is grounded by `hg`,
	// then we can immediately and directly move upwards. Do this
	// by assembling the single (unique) upward term, and then
	// seeing if it's acceptable to the gauntlet of callbacks.
	// If it is, we are done.
	//
	// The prototypical search being handled here is that of
	//
	//     EvaluationLink
	//         PredicateNode "some const"
	//         ListLink
	//             VariableNode "$x"
	//             ConceptNode "foo"
	//
	// If we arrive here, with `ptm` being the ListLink, and the `hg`
	// being the grounding of the ListLink, then we should be able to
	// immediately jump to the EvaluationLink, without any further ado.
	// Specifically, there is no need to search the incoming set of `hg`;
	// just build up the EvaluationLink and offer it as the ground.

	bool need_search = false;
	HandleSeq oset;
	oset.reserve(parent->getArity());
	for (const PatternTermPtr& pp: parent->getOutgoingSet())
	{
		if (pp == ptm)
			oset.push_back(hg);
		else if (pp->hasAnyBoundVariable())
		{
			// If we are here, then `parent` has another
			// variable, besides `ptm`. In some (rare?)
			// cases, we might already know how it's grounded.
			// Is it a waste of CPU time to check? I dunno.
			auto gnd(var_grounding.find(pp->getHandle()));
			if (gnd == var_grounding.end())
			{
				// Oh no! Abandon ship!
				need_search = true;
				break;
			}
			oset.push_back(gnd->second);
		}
		else
			oset.push_back(pp->getHandle());
	}
	if (not need_search)
	{
		// Yuck. What we really want to do here is to find out
		// if `Link(t, oset)` is in the incoming set of `hg`. But
		// there isn't any direct way of doing this (at this time).
		Handle hup(_pmc.get_link(hg, t, std::move(oset)));
		if (nullptr == hup) return false;
		return explore_type_branches(parent, hup, clause);
	}

	// If we are here, then somehow the upward-term is not unique, and
	// we have to explore the incoming set of the ground to see which
	// (if any) of the incoming set satsisfies the parent term.

	IncomingSet iset = _pmc.get_incoming_set(hg, t);
	size_t sz = iset.size();
	DO_LOG({LAZY_LOG_FINE << "Looking upward at ordered term = "
	                      << parent->getQuote()->to_string() << std::endl
	                      << "The grounded pivot point " << hg->to_string()
	                      << " has " << sz << " branches";})

	// Just explore directly upwards.
	bool found = false;
	for (size_t i = 0; i < sz; i++)
	{
		DO_LOG({LAZY_LOG_FINE << "Try upward branch " << i+1 << " of " << sz
		                      << " at ordered term=" << parent->to_string()
		                      << " propose=" << iset[i]->to_string();})

		found = explore_type_branches(parent, iset[i], clause);
		if (found) break;
	}

	logmsg("Found upward soln =", found);
	return found;
}

/// Same as explore_up_branches(), handles the case where `ptm` has
/// has unordered links without any GlobNodes in them.
/// Handles the case where there's at least one (and maybe more)
/// unordered links, somewhere at this level, or next to us or to
/// the side and below us. Explore all of the different possible
/// permutations.
bool PatternMatchEngine::explore_upund_branches(const PatternTermPtr& ptm,
                                                const Handle& hg,
                                                const PatternTermPtr& clause)
{
	// Move up the solution graph, looking for a match.
	const PatternTermPtr& parent(ptm->getParent());
	Type t = parent->getHandle()->get_type();

	IncomingSet iset = _pmc.get_incoming_set(hg, t);
	size_t sz = iset.size();
	DO_LOG({LAZY_LOG_FINE << "Looking upward at maybe disordered term = "
	                      << parent->getQuote()->to_string() << std::endl
	                      << "The grounded pivot point " << hg->to_string()
	                      << " has " << sz << " branches";})

	_perm_breakout = _perm_to_step;
	bool found = false;
	for (size_t i = 0; i < sz; i++)
	{
		DO_LOG({LAZY_LOG_FINE << "Try upward permutable branch "
		                      << i+1 << " of " << sz
		                      << " at unordered term=" << parent->to_string()
		                      << " propose=" << iset[i]->to_string();})

		_perm_odo.clear();
		perm_push();
		_perm_go_around = false;
		found = explore_odometer(parent, iset[i], clause);
		perm_pop();

		if (found) break;
	}
	_perm_breakout = nullptr;

	logmsg("Found upward soln =", found);
	return found;
}

/// Same as explore_up_branches(), handles the case where `ptm`
/// is an ordered link with a GlobNode in it. In this case, we need to
/// loop over the incoming, just as above, and also loop over different
/// glob grounding possibilities.
bool PatternMatchEngine::explore_upglob_branches(const PatternTermPtr& ptm,
                                                 const Handle& hg,
                                                 const PatternTermPtr& clause)
{
	const PatternTermPtr& parent(ptm->getParent());
	Type t = parent->getHandle()->get_type();
	IncomingSet iset;
	if (nullptr == hg->getAtomSpace())
		iset = _pmc.get_incoming_set(hg->getOutgoingAtom(0), t);
	else
		iset = _pmc.get_incoming_set(hg, t);

	size_t sz = iset.size();
	DO_LOG({LAZY_LOG_FINE << "Looking globby upward for term = "
	                      << parent->getQuote()->to_string() << std::endl
	                      << "It's grounding " << hg->to_short_string()
	                      << " has " << sz << " branches";})

	// Move up the solution graph, looking for a match.
	bool found = false;
	for (size_t i = 0; i < sz; i++)
	{
		DO_LOG({LAZY_LOG_FINE << "Try upward branch " << i+1 << " of " << sz
		                      << " for glob term=" << parent->to_string()
		                      << " propose=" << iset[i]->id_to_string();})

		// Before exploring the link branches, record the current
		// _glob_state size.  The idea is, if the parent & iset[i] is a match,
		// their state will be recorded in _glob_state, so that one can,
		// if needed, resume and try to ground those globs again in a
		// different way (e.g. backtracking from another branchpoint).
		auto saved_glob_state = _glob_state;

		found = explore_glob_branches(parent, iset[i], clause);

		// Restore the saved state, for the next go-around.
		_glob_state = saved_glob_state;

		if (found) break;
	}
	logmsg("Found upward soln =", found);
	return found;
}

/// explore_glob_branches -- explore glob grounding alternatives.
///
/// Given a pattern containing one or more globs, this explores all
/// possible alternative groundings for those glob nodes.
///
/// This only works for the case where the pattern is an ordered link.
/// When the pattern is unordered, a completely different enumeration
/// strategy is required. See `explore_sparse_branches()` for that case.
///
bool PatternMatchEngine::explore_glob_branches(const PatternTermPtr& ptm,
                                               const Handle& hg,
                                               const PatternTermPtr& clause)
{
	// Check to make sure the pattern actually has globs in it!
	OC_ASSERT(ptm->hasAnyGlobbyVar(),
	          "Glob exploration went horribly wrong!");

	// Record the glob_state *before* starting exploration.
	size_t gstate_size = _glob_state.size();

	// If no solution is found, and there are globs, then there may
	// be other ways to ground the glob differently.  So keep trying,
	// until all possibilities are exhausted.
	//
	// Once there are no more possible ways to ground globby terms,
	// they are removed from glob_state. So simply by comparing the
	// _glob_state size before and after seems to be an OK way to
	// quickly check if we can move on to the next one or not.
	do
	{
		if (explore_type_branches(ptm, hg, clause))
			return true;
		logmsg("Globby clause not grounded; try again");
	}
	while (_glob_state.size() > gstate_size);

	return false;
}

// explore_odometer - explore multiple unordered links at once.
//
// The core issue addressed here is that there may be lots of
// UnorderedLinks below us, and we have to explore all of them.
// So this tries to advance all of them.
bool PatternMatchEngine::explore_odometer(const PatternTermPtr& ptm,
                                          const Handle& hg,
                                          const PatternTermPtr& clause)
{
	bool found = explore_type_branches(ptm, hg, clause);
	if (found)
		return true;

	while (_perm_have_more and _perm_to_step != _perm_breakout)
	{
		_perm_have_more = false;
		_perm_take_step = true;
		_perm_go_around = false;

		logmsg("ODO STEP unordered beneath term:", ptm);
		if (explore_type_branches(ptm, hg, clause))
			return true;
	}
	return false;
}

/// explore_unordered_branches -- explore UnorderedLink alternatives.
///
/// Every UnorderedLink of arity N presents N-factorial different
/// grounding possibilities, corresponding to different permutations
/// of the UnorderedLink.  Each permutation must be explored. Thus,
/// this can be thought of as a branching of exploration possibilities,
/// each branch corresponding to a different permutation.  (If you
/// know algebra, then think of the "free object" (e.g. the "free
/// group") where alternative branches are "free", unconstrained.)
///
/// For each possible branch, the current state is saved, the branch
/// is explored, then the state is popped. If the exploration yielded
/// nothing, then the next branch is explored, until exhaustion of the
/// possibilities.  Upon exhaustion, it returns to the caller.
///
/// This only applies when the UnorderedLink pattern has a fixed arity,
/// i.e. has no glob nodes in it to soak up any remaining unmatched
/// subterms. When there is a glob node, the search takes on a different
/// form; see `explore_sparse_branches()`.
///
bool PatternMatchEngine::explore_unordered_branches(const PatternTermPtr& ptm,
                                                    const Handle& hg,
                                                    const PatternTermPtr& clause)
{
	do
	{
		// If the pattern was satisfied, then we are done for good.
		if (explore_single_branch(ptm, hg, clause))
			return true;

		logmsg("Step to next permutation");

		// If we are here, there was no match.
		// On the next go-around, take a step.
		_perm_take_step = true;
		_perm_have_more = false;
	}
	while (have_perm(ptm));

	_perm_take_step = false;
	_perm_have_more = false;
	logmsg("No more unordered permutations");

	return false;
}

/// explore_sparse_branches -- explore "sparse" unordered links.
///
/// When an UnorderedLink has a glob in it, the manner of iterating over
/// all possbile alternative groundings is different. In this case, one
/// must try to ground each of the subterms of the term in every
/// possible way, given the contents of `hg`, and then take everything
/// left over in `hg` and jam  it into the glob. This differs from the
/// unordered/odometer search, which only works for fixed-arity
/// groundings, and also from the glob search, whoch works only for
/// ordered links.
///
/// Note: it does not make sense for an unordered pattern to have more
/// than one glob in it! The one glob soaks up all the unmatched
/// left-overs; having two only confuses the situation.
///
/// This is "sparse" in the sense that the glob might end up being huge,
/// containing everything left-over that wasn't in the pattern. For
/// the case of chemistry informatics, the pattern will specify the
/// functional group, and the glob will end up holding the moiety that
/// is not a part of the functional group.
///
/// XXX FIXME: Right now, this code handles graphs that have only one
/// single sparse search.   Nested sparse searches are not supported;
/// to implement those, its "easy": implement the same flow control as
/// the unordered_explore steppers. I'm lzay, today, so I am not doing
/// this just right now.
///
/// Also: right now, it uses a very inefficient search strategy,
/// enumerating over everything. See notes on match_sparse for a better
/// idea.
///
bool PatternMatchEngine::explore_sparse_branches(const PatternTermPtr& ptm,
                                                 const Handle& hg,
                                                 const PatternTermPtr& clause)
{
	// XXX TODO FIXME. The ptm needs to be decomposed into connected
	// components. Then only the connected components need to be walked
	// over.  That would be much more efficient.
	do
	{
		// If the pattern was satisfied, then we are done for good.
		if (explore_single_branch(ptm, hg, clause))
			return true;

		logmsg("Sparse explore: Step to next odo");

		// If we are here, there was no match.
		// On the next go-around, take a step.
		_sparse_take_step = true;
	}
	while (have_more_rotors(ptm));
	_sparse_take_step = false;

	return false;
}

/// explore_type_branches -- perform exploration of alternatives.
///
/// This dispatches exploration of different grounding alternatives to
/// one of the "specialist" functions that know how to ground specific
/// link types.
///
/// In the simplest case, there are no alternatives, and this just
/// dispatches to `explore_single_branch()` which is just a wrapper
/// around `tree_compare()`. This just returns true or false to indicate
/// if the suggested grounding `hg` actually is a match for the current
/// term being grounded. Before calling `tree_compare()`, the
/// `explore_single_branch()` method pushes all current state, and then
/// pops it upon return. In other words, this encapsulates a single
/// up-branch (incoming-set branch): grounding of that single branch
/// succeeds or fails. Failure backtracks to the caller of this method;
/// upon return, the current state has been restored; this routine
/// leaves the current state as it found it.
///
/// The `explore_single_branch()` method is part of a recursive chain
/// that only terminates  when a grounding for *the entire pattern* was
/// found (and the grounding was accepted) or if all possibilities were
/// exhaustivelyexplored.  Thus, this returns true only if entire
/// pattern was grounded.
bool PatternMatchEngine::explore_type_branches(const PatternTermPtr& ptm,
                                               const Handle& hg,
                                               const PatternTermPtr& clause)
{
	// Iterate over different possible choices. At this time, this will
	// never happen, because the selection of start-terms and joining
	// terms never selects a term that is embedded inside a Choice. That
	// is, we never start with a term underneath a Choice, move up,
	// discover that we are in a Choice, and then have to explore the
	// other choices. And that's OK, because this avoids complexity.
	if (ptm->isChoice())
		return explore_choice_branches(ptm, hg, clause);

	// Unordered links have permutations to explore.
	if (ptm->isUnorderedLink())
	{
		// If ptm is unordered and has a glob in it, then...
		if (ptm->hasGlobbyVar())
			return explore_sparse_branches(ptm, hg, clause);

		// No glob vars in ptm. There might still be some deeper in.
		return explore_unordered_branches(ptm, hg, clause);
	}

	return explore_single_branch(ptm, hg, clause);
}

/* ... just search ...  */
bool PatternMatchEngine::explore_term_branches(const PatternTermPtr& term,
                                               const Handle& hg,
                                               const PatternTermPtr& clause)
{
	logmsg("Begin exploring term:", term);
	bool found;
	if (term->hasAnyGlobbyVar())
		found = explore_glob_branches(term, hg, clause);

	else if (term->hasUnorderedLink())
	{
		// The asert case is handled immediately above, in explore_type_branches
		OC_ASSERT(not term->hasGlobbyVar(), "Buggy or not implemented!");
		found = explore_odometer(term, hg, clause);
	}
	else
		found = explore_type_branches(term, hg, clause);

	logmsg("Finished exploring term:", term, found);
	return found;
}

/// See explore_unordered_branches() for a general explanation.
/// This method handles the Choice term branch alternatives only.
/// This method is never called, currently; see notes above. It's
/// never called because we never get a start-term or a join-term
/// that lies inside of a Choice, and therefore, never have to
/// explore the alternatives.
bool PatternMatchEngine::explore_choice_branches(const PatternTermPtr& ptm,
                                                 const Handle& hg,
                                                 const PatternTermPtr& clause)
{
	throw RuntimeException(TRACE_INFO,
		"Maybe this works but its not tested!! Find out!");

	logmsg("Begin choice branchpoint iteration loop");
	do {
		// XXX This `need_choice_push` thing is probably wrong; it probably
		// should resemble the perm_push() used for unordered links.
		// However, currently, no test case trips this up. so .. OK.
		// Whatever. This still probably needs fixing.
		if (_need_choice_push) choice_stack.push(_choice_state);
		bool match = explore_single_branch(ptm, hg, clause);
		if (_need_choice_push) POPSTK(choice_stack, _choice_state);
		_need_choice_push = false;

		// If the pattern was satisfied, then we are done for good.
		if (match)
			return true;

		logmsg("Step to next choice");
		// If we are here, there was no match.
		// On the next go-around, take a step.
		_choose_next = true;
	} while (have_choice(ptm, hg));

	logmsg("Exhausted all choice possibilities"
	       "\n----------------------------------");
	return false;
}

/// Quick hack to find a variable in a pattern term. Needed immediately
/// below. Recursively drill down till we find the desired variable.
static PatternTermPtr find_variable_term(const PatternTermPtr& term,
                                         const Handle& var)
{
	if (term->getHandle() == var) return term;
	for (const PatternTermPtr& subterm: term->getOutgoingSet())
	{
		const PatternTermPtr& vterm = find_variable_term(subterm, var);
		if (PatternTerm::UNDEFINED != vterm) return vterm;
	}
	return PatternTerm::UNDEFINED;
}

/// Find the next untried connectable term in the `parent` Present term.
/// If there is no such term, returns false; otherwise, returns true,
/// and the next term to explore is returned in `untried_term` and the
/// joining variable in `joint`.  This does a brute-force search. This
/// should be good enough for almost all use-cases I can imagine. The
/// only way to get fancier would be to build some structures during
/// static analysis:
/// -- build a connectivity map, just like the one for clauses
/// -- build a clause_variables struct, but just for this term
/// -- search for the thinnest joint, just like `get_next_clause`
/// XXX FIXME -- do the above.
///
bool PatternMatchEngine::next_untried_present(const PatternTermPtr& parent,
                                              const PatternTermPtr& clause,
                                              PatternTermPtr& untried_term,
                                              PatternTermPtr& joint,
                                              Handle& jgnd)
{
	// So, first, get a common shared variable. Assume that clause
	// will point at the right thing.
	const HandleSeq& varseq = _pat->clause_variables.at(clause);
	if (0 == varseq.size())
		throw RuntimeException(TRACE_INFO, "Expecting a variable!");

	// Loop over all the terms in the Present term
	for (const PatternTermPtr& pterm: parent->getOutgoingSet())
	{
		if (issued_present.end() != issued_present.find(pterm))
			continue;

		// Now look for a grounded variable
		for (const Handle& jvar : varseq)
		{
			const auto& pr = var_grounding.find(jvar);
			if (var_grounding.end() == pr) continue;

			// Get the joining variable.
			joint = find_variable_term(pterm, jvar);
			if (PatternTerm::UNDEFINED == joint) continue;

			jgnd = pr->second;
			untried_term = pterm;
			issued_present.insert(pterm);
			return true;
		}
	}

	// We expect all of the terms to have been joined together by now.
	if (parent->getArity() != issued_present.size())
		throw RuntimeException(TRACE_INFO, "Unable to join all terms!");

	return false;
}

/// This attempts to obtain a grounding for an embedded Present terms.
/// That is, for a Present term that is not at the top-most level.
/// At this time, we expect to encounter these only inside of Choice
/// terms and inside of evaluatable terms.
/// This tries to verify that each of the terms under the Present term
/// can be grounded. The branch exploration consists of examining
/// each different way in which this can be accomplished.
bool PatternMatchEngine::explore_present_branches(const PatternTermPtr& ptm,
                                                  const Handle& hg,
                                                  const PatternTermPtr& clause)
{
	const Handle& hp = ptm->getHandle();

	// Reject self-grounds.
	if (hp == hg) return false;

	logmsg("!! explore_present:", hp);
	logmsg("!! present gnd:", hg);

	bool joins = tree_compare(ptm, hg, CALL_PRESENT);
	logmsg("!! explore_present result=", joins);
	if (not joins) return false;

	const PatternTermPtr& parent = ptm->getParent();
	if (1 == parent->getArity())
		return do_term_up(parent, hg, clause);

	// Compare the other parts of the present link.
	// They all have to match.
	issued_present.insert(ptm);

	PatternTermPtr joint;
	PatternTermPtr next_term;
	Handle jgnd;
	bool have_more = next_untried_present(parent, clause,
	                                      next_term, joint, jgnd);
	if (have_more)
	{
		logmsg("!! maybe_present:", next_term->getQuote());

		// Explore from this joint.
		bool found = explore_term_branches(joint, jgnd, clause);

		logmsg("!! maybe_present result=",  found);
		if (not found) issued_present.clear();
		return found;
	}

	logmsg("!! explore_present success!");
	issued_present.clear();
	return do_term_up(parent, hg, clause);
}

/// Check the proposed grounding hg for pattern term hp.
///
/// As the name implies, this will explore only one single potential
/// (proposed) grounding for the current pattern term. This is meant
/// to be called after a viable branch has been identified for
/// exploration.
///
/// This is wrapper around tree compare; if tree_compare
/// returns false, then this returns immediately.
///
/// However, this method is part of the upwards-recursion chain,
/// so if tree_compare approves the proposed grounding, this will
/// recurse upwards, calling do_term_up to get the next pattern
/// term. Thus, this method will return true ONLY if ALL OF the terms
/// and clauses in the pattern are satisfiable (are accepted matches).
///
bool PatternMatchEngine::explore_single_branch(const PatternTermPtr& ptm,
                                               const Handle& hg,
                                               const PatternTermPtr& clause)
{
	solution_push();

	logmsg("ssss Checking term:", ptm);
	logmsg("ssss For soln by:", hg);

	bool match = tree_compare(ptm, hg, CALL_SOLN);

	if (not match)
	{
		logmsg("ssss NO solution for term:", ptm);
		logmsg("ssss NOT solved by:", hg);
		solution_pop();
		return false;
	}

	logmsg("ssss Solved term:", ptm);
	logmsg("ssss It is solved by:", hg);
	logmsg("ssss will move up.");

	// Continue onwards to the rest of the pattern.
	bool found = do_term_up(ptm, hg, clause);

	solution_pop();
	return found;
}

/// do_term_up() -- move upwards from the current term.
///
/// Given the current term, in `hp`, find its parent in the clause,
/// and then call explore_up_branches() to see if the term's parent
/// has corresponding match in the solution graph.
///
/// Note that, in the "normal" case, a given term has only one, unique
/// parent in the given root_clause, and so its easy to find; one just
/// looks at the path from the root clause down to the term, and the
/// parent is the link immediately above it.
///
/// There are five exceptions to this "unique parent" case:
///  * The term is already the root clause; it has no parent. In this
///    case, we send it off to the machinery that explores the next
///    clause.
///  * Exactly the same term may appear twice, 3 times, etc. in the
///    clause, all at different locations.  This is very rare, but
///    can happen. In essence, it has multiple parents; each needs
///    to be checked. We loop over these.
///  * The term is a part of a larger, evaluatable term. In this case,
///    we don't want to go to the immediate parent, we want to go to
///    the larger evaluatable term, and offer that up as the thing to
///    match (i.e. to evaluate, to invoke callbacks, etc.)
///  * The parent is a Choice term. In this case, the Choice term
///    itself cannot be directly matched, as is; only its children can
///    be. So in this case, we fetch the Choice terms's parent, instead.
///  * Some crazy combination of the above.
///
/// If it weren't for these complications, this method would be small
/// and simple: it would send the parent to explore_up_branches(), and
/// then explore_up_branches() would respond as to whether it is
/// satisfiable (solvable) or not.
///
/// Takes as an argument an atom `hp` in the pattern, and its matching
/// grounding `hg`.  Thus, hp's parent will need to be matched to hg's
/// parent.
///
/// Returns true if a grounding for the term's parent was found.
///
bool PatternMatchEngine::do_term_up(const PatternTermPtr& ptm,
                                    const Handle& hg,
                                    const PatternTermPtr& clause)
{
	depth = 1;

	// If we are here, then everything below us matches.  If we are
	// at the top of the clause, move on to the next clause. Else,
	// we are working on a term somewhere in the middle of a clause
	// and need to walk upwards.
	if (ptm == clause)
		return clause_accept(clause, hg);

	// Move upwards in the term, and hunt for a match, again.
	// There are two ways to move upwards: for a normal term, we just
	// find its parent in the clause. For an evaluatable term, we find
	// the parent evaluatable in the clause, which may be many steps
	// higher.
	logmsg("This term has ground, move upwards:", ptm);

	const PatternTermPtr& parent = ptm->getParent();
	OC_ASSERT(PatternTerm::UNDEFINED != parent, "Unknown term parent");

	// Most of the time, we expect to hit this.
	if (parent->isPresent() and not parent->isLiteral())
	{
		OC_ASSERT(parent != clause, "Not expecting a Present clause here!");
		return explore_present_branches(ptm, hg, clause);
	}

	// Handle identical terms before evaluatables.
	if (parent->isIdentical())
		return explore_clause_identical(ptm, hg, clause);

	if (parent->hasAnyEvaluatable())
	{
		OC_ASSERT(false, "Hit some dead code!");
		// XXX TODO make sure that all variables in the clause have
		// been grounded!  If they're not, something is badly wrong!
		logmsg("Term inside evaluatable, move up to it's top:",
			       clause->getQuote());
		bool found = _pmc.evaluate_sentence(clause->getHandle(), var_grounding);
		logmsg("After evaluating clause, found = ", found);
		if (found)
			return clause_accept(clause, hg);

		return false;
	}

	// Do the simple case first, Choice terms are harder.
	if (not parent->isChoice())
	{
		bool found = explore_up_branches(ptm, hg, clause);
		logmsg("After moving up the clause, found = ", found);
		return found;
	}

	// If we are here, then we have Choice term. (We checked above.)
	if (parent == clause)
	{
		logmsg("Exploring Choice term at root");
		return clause_accept(clause, hg);
	}

	// If we are here, then we have an embedded Choice term, e.g.
	// a ChoiceLink that is not at the clause root. It's contained
	// in some other link, and we have to get that link and
	// perform comparisons on it. i.e. we have to "hop over"
	// (hop up) past the Choice term, before resuming the search.
	// The easiest way to hop is to do it recursively... i.e.
	// call ourselves again.
	logmsg("Exploring Choice term below root");

	OC_ASSERT(not have_choice(parent, hg),
	          "Something is wrong with the Choice code");

	_need_choice_push = true;
	return do_term_up(parent, hg, clause);
}

/// This is called when we've navigated to the top of a clause,
/// and so it is fully grounded, and we're essentially done.
/// However, let the callbacks have the final say on whether to
/// proceed onwards, or to backtrack.
///
bool PatternMatchEngine::clause_accept(const PatternTermPtr& clause,
                                       const Handle& hg)
{
	// We have to unwrap one more level of quotation before we are done.
	const Handle& clause_root(clause->getQuote());

	// Is this clause a required clause? If so, then let the callback
	// make the final decision; if callback rejects, then it's the
	// same as a mismatch; try the next one.
	bool match;
	if (clause->isAbsent())
	{
		clause_accepted = true;
		match = _pmc.optional_clause_match(clause_root, hg, var_grounding);
		logmsg("Optional clause match callback match=", match);
	}
	else
	if (clause->isAlways())
	{
		_did_check_forall = true;
		if (hg == clause_root) return false;
		match = _pmc.always_clause_match(clause_root, hg, var_grounding);
		_forall_state = _forall_state and match;
		logmsg("For-all clause match callback match=", match);
	}
	else
	{
		match = _pmc.clause_match(clause_root, hg, var_grounding);
		logmsg("clause match callback match=", match);
	}
	if (not match) return false;

	if (not clause->hasAnyEvaluatable())
	{
		clause_grounding[clause_root] = hg;

		// Handle the highly unusual case of the top-most clause
		// being a GlobNode. We were unable to record this earlier,
		// in variable_compare(), so we do it here.
		if (clause_root->get_type() == GLOB_NODE)
			var_grounding[clause_root] = hg;

		logmsg("---------------------\nclause:", clause_root);
		logmsg("ground:", hg);

		// Cache the result, so that it can be reused.
		// See commentary on `explore_clause()` for more info.
		HandleSeq key;
		const HandleSeq& clvars(_pat->clause_variables.at(clause));
		size_t cvsz = clvars.size();

		// Clause contains just a single variable
		if (1 == cvsz and
			 _pat->cacheable_clauses.find(clause_root) !=
		    _pat->cacheable_clauses.end())
		{
			const Handle& jgnd(var_grounding.at(clvars[0]));
			key = HandleSeq({clause_root, jgnd});
		}

		// Clause contains two or more variables.
		if (1 < cvsz and
			 _pat->cacheable_multi.find(clause_root) !=
		    _pat->cacheable_multi.end())
		{
			key = clause_grounding_key(clause_root, clvars);
		}

		if (0 < key.size())
		{
#ifdef QDEBUG
			// The same clause can sometimes be grounded multiple
			// times, but if the caching is valid, then it should
			// always be grounded exactly the same way.
			const auto& prev = _gnd_cache.find(key);
			if (_gnd_cache.end() != prev)
				OC_ASSERT(prev->second == hg, "Internal Error");
#endif
			_gnd_cache.insert({key, hg});
		}
	}

	// Now go and do more clauses.
	return do_next_clause();
}

/// This is called when all previous clauses have been grounded; so
/// we search for the next one, and try to ground that.
bool PatternMatchEngine::do_next_clause(void)
{
	// Keep the next joint and the next clause on the C++ stack.
	PatternTermPtr do_clause;
	PatternTermPtr joiner;

	clause_stacks_push();
	_pmc.next_connections(var_grounding);
	bool have_more = _pmc.get_next_clause(do_clause, joiner);

	// If there are no further clauses to solve,
	// we are really done! Report the solution via callback.
	if (not have_more)
	{
		bool found = report_grounding(var_grounding, clause_grounding);
		logmsg("==================== FINITO! accepted=", found);
		DO_LOG(log_solution(var_grounding, clause_grounding);)
		clause_stacks_pop();
		return found;
	}

	bool found = false;
	while (true)
	{
		logmsg("Next clause is", do_clause->getQuote());
		DO_LOG({LAZY_LOG_FINE << "This clause is "
			              << (do_clause->isAbsent()? "absent" : "required");})
		DO_LOG({LAZY_LOG_FINE << "This clause is "
			              << (do_clause->hasAnyEvaluatable()?
			                  "dynamically evaluatable" : "non-dynamic");
		logmsg("Joining variable is", joiner->getQuote());
		logmsg("Joining grounding is", var_grounding[joiner->getQuote()]); })

		// Start solving the next unsolved clause. Note: this is a
		// recursive call, and not a loop. Recursion is halted when
		// the next unsolved clause has no grounding.
		//
		// We continue our search at the variable/glob that "joins"
		// (is shared in common) between the previous (solved) clause,
		// and this clause.

		clause_stacks_push();
		clause_accepted = false;
		Handle hgnd(var_grounding[joiner->getHandle()]);
		if (nullptr == hgnd)
		{
			// Hack for clauses with no variables...
			const Handle& j(joiner->getHandle());
			var_grounding[j] = j;
			hgnd = j;
		}
		found |= explore_clause(joiner, hgnd, do_clause);
		clause_stacks_pop();

		if (not _pmc.get_next_clause(do_clause, joiner)) break;
		logmsg("This was a multiple-choice clause; looping around.");
	}

	// If we are here, and found is false, then we've exhausted all
	// of the search possibilities for the current clause. If this
	// is an optional clause, and no solutions were reported for it,
	// then report the failure of finding a solution now. If this was
	// also the final optional clause, then in fact, we've got a
	// grounding for the whole thing ... report that!
	//
	// Note that lack of a match halts recursion; thus, we can't
	// depend on recursion to find additional unmatched optional
	// clauses; thus we have to explicitly loop over all optional
	// clauses that don't have matches.
	while ((false == found) and
	       (false == clause_accepted) and
	       (do_clause->isAbsent()))
	{
		const Handle& curr_root(do_clause->getHandle());
		static Handle undef(Handle::UNDEFINED);
		bool match = _pmc.optional_clause_match(curr_root, undef, var_grounding);
		logmsg("Exhausted search for optional clause, cb=", match);
		if (not match) {
			clause_stacks_pop();
			return false;
		}

		clause_grounding[curr_root] = Handle::UNDEFINED;
		_pmc.next_connections(var_grounding);
		have_more = _pmc.get_next_clause(do_clause, joiner);
		if (not have_more)
		{
			logmsg("==================== FINITO BANDITO!");
			DO_LOG({log_solution(var_grounding, clause_grounding);})
			found = report_grounding(var_grounding, clause_grounding);
			clause_stacks_pop();
			return found;
		}

		logmsg("Next optional clause is", do_clause->getQuote());

		// Now see if this optional clause has any solutions,
		// or not. If it does, we'll recurse. If it does not,
		// we'll loop around back to here again.
		clause_accepted = false;
		const Handle& hgnd(var_grounding[joiner->getHandle()]);

		found = explore_term_branches(joiner, hgnd, do_clause);
	}

	// If we failed to find anything at this level, we need to
	// backtrack, i.e. pop the stack, and begin a search for
	// other possible matches and groundings.
	clause_stacks_pop();

	return found;
}

/* ======================================================== */
/**
 * Push all stacks related to the grounding of a clause. This push is
 * meant to be done only when a grounding for a clause has been found,
 * and the next clause is about the be attempted. It saves all of the
 * traversal data associated with the current clause, so that, later
 * on, traversal can be resumed where it was left off.
 *
 * This does NOT push any of the redex stacks because (with the current
 * redex design), all redex substitutions should have terminatated by
 * now, and returned to the main clause. i.e. the redex stack is assumed
 * to be empty, at this point.  (Its possible this design may change in
 * in the future if multi-clause redexes are allowed, whatever the heck
 * that may be!?)
 */
void PatternMatchEngine::clause_stacks_push(void)
{
	_clause_stack_depth++;
	logmsg("--- CLAUSE stack push to depth=", _clause_stack_depth);

	var_solutn_stack.push(var_grounding);
	_clause_solutn_stack.push(clause_grounding);

	choice_stack.push(_choice_state);

	perm_push();

	// These should already be null/false; if not there's slop
	// in the code. Clear, just to be sure.
	_perm_to_step = nullptr;
	_perm_take_step = false;
	_perm_have_more = false;
	_perm_breakout = nullptr;
	_perm_go_around = false;
	_perm_odo.clear();
	_perm_podo.clear();
	// _perm_odo_state.clear();

	_pmc.push();
}

/**
 * Pop all clause-traversal-related stacks. This restores state
 * so that the traversal of a single clause can resume where it left
 * off. These do NOT affect any of the redex stacks (which are assumed
 * to be empty at this point.)
 */
void PatternMatchEngine::clause_stacks_pop(void)
{
	_pmc.pop();

	// The grounding stacks are handled differently.
	POPSTK(_clause_solutn_stack, clause_grounding);
	POPSTK(var_solutn_stack, var_grounding);

	POPSTK(choice_stack, _choice_state);

	perm_pop();

	_clause_stack_depth --;
	logmsg("CLAUSE stack pop to depth", _clause_stack_depth);
}

/**
 * Unconditionally clear all graph traversal stacks
 * XXX TODO -- if the algo is working correctly, then all
 * of these should already be empty, when this method is
 * called. So really, we should check the stack size, and
 * assert if it is not zero ...
 */
void PatternMatchEngine::clause_stacks_clear(void)
{
	_clause_stack_depth = 0;
#if 0
	// Currently, only GlobUTest fails when this is uncommented.
	OC_ASSERT(0 == _clause_solutn_stack.size());
	OC_ASSERT(0 == var_solutn_stack.size());
	OC_ASSERT(0 == choice_stack.size());
	OC_ASSERT(0 == _perm_stack.size());
	OC_ASSERT(0 == _perm_stepper_stack.size());
#else
	while (!_clause_solutn_stack.empty()) _clause_solutn_stack.pop();
	while (!var_solutn_stack.empty()) var_solutn_stack.pop();
	while (!choice_stack.empty()) choice_stack.pop();
	while (!_perm_stack.empty()) _perm_stack.pop();
	while (!_perm_stepper_stack.empty()) _perm_stepper_stack.pop();
	while (!_perm_step_saver.empty()) _perm_step_saver.pop();
#endif
}

void PatternMatchEngine::solution_push(void)
{
	var_solutn_stack.push(var_grounding);
	_clause_solutn_stack.push(clause_grounding);
}

void PatternMatchEngine::solution_pop(void)
{
	POPSTK(var_solutn_stack, var_grounding);
	POPSTK(_clause_solutn_stack, clause_grounding);
}

void PatternMatchEngine::solution_drop(void)
{
	var_solutn_stack.pop();
	_clause_solutn_stack.pop();
}

/* ======================================================== */

/// Pass the grounding that was found out to the callback.
/// ... unless there is an Always clasue, in which case we
/// save them up until we've looked at all of them.
bool PatternMatchEngine::report_grounding(const GroundingMap &var_soln,
                                          const GroundingMap &term_soln)
{
	// If there is no for-all clause (no AlwaysLink clause)
	// then report groundings as they are found.
	if (_pat->always.size() == 0)
		return _pmc.grounding(var_soln, term_soln);

	// Don't even bother caching, if we know we are losing.
	if (not _forall_state) return false;

	// If we are here, we need to record groundings, until later,
	// when we find out if the for-all clauses were satsified.
	_var_ground_cache.push_back(var_soln);
	_term_ground_cache.push_back(term_soln);

	// Keep going.
	return false;
}

bool PatternMatchEngine::report_forall(void)
{
	// Nothing to do.
	if (_pat->always.size() == 0) return false;

	// If its OK to report, then report them now.
	bool halt = false;
	if (_forall_state)
	{
		size_t nitems = _var_ground_cache.size();
		OC_ASSERT(_term_ground_cache.size() == nitems);
		for (size_t i=0; i<nitems; i++)
		{
			halt = _pmc.grounding(_var_ground_cache[i],
			                      _term_ground_cache[i]);
			if (halt) break;
		}
	}
	_forall_state = true;
	_var_ground_cache.clear();
	_term_ground_cache.clear();
	return halt;
}

/* ======================================================== */

/**
 * explore_neighborhood - explore the local (connected) neighborhood
 * of the starter clause, looking for a match.  The idea here is that
 * it is much easier to traverse a connected graph looking for the
 * appropriate subgraph (pattern) than it is to try to explore the
 * whole atomspace, at random.  The user callback `perform_search()`
 * should call this method, suggesting a clause to start with, and
 * where in the clause the search should begin.
 *
 * Inputs:
 * do_clause: Must be one of the clauses previously specified in the
 *            clause list of the match() method.
 * term:      Must be a sub-term of do_clause; that is, must be a link
 *            that appears in do_clause. (It may be the clause itself,
 *            which happens when the clause is shallow.)
 * grnd:      Must be one of the outgoing atoms of `term` that is
 *            constant, i.e. is not a variable, and does not contain
 *            any variables.
 *
 * Returns true if one (or more) matches are found
 *
 * This routine is meant to be invoked on every candidate atom taken
 * from the atom space. That atom is assumed to anchor some part of
 * a graph that hopefully will match the pattern.
 */
bool PatternMatchEngine::explore_neighborhood(const PatternTermPtr& term,
                                              const Handle& grnd,
                                              const PatternTermPtr& clause)
{
	clause_stacks_clear();
	clear_current_state();
	_nack_cache.clear();

	bool halt = explore_clause(term, grnd, clause);
	bool stop = report_forall();
	return halt or stop;
}

/// Has every variable in the clause been fully grounded already?
/// If so, then we can deal with this clause directly, without
/// having to perform any further searches.
bool PatternMatchEngine::is_clause_grounded(const PatternTermPtr& clause) const
{
	const auto& it = _pat->clause_variables.find(clause);
	OC_ASSERT(it != _pat->clause_variables.end(), "Internal Error");
	for (const Handle& hvar : it->second)
	{
		if (var_grounding.end() == var_grounding.find(hvar))
			return false;
	}
	return true;
}

/// Return a lookup key for this clause.
/// The `varseq` should be the variables in this clause
/// as recorded in `_pat->clause_variables.find(clause)`
HandleSeq PatternMatchEngine::clause_grounding_key(const Handle& clause,
                                                   const HandleSeq& varseq) const
{
	static HandleSeq empty;
	HandleSeq key({clause});
	for (const Handle& hvar : varseq)
	{
		const auto& gv = var_grounding.find(hvar);
		if (var_grounding.end() == gv)
			return empty;
		key.push_back(gv->second);
	}
	return key;
}

/**
 * Every clause in a pattern is one of two types:  it either
 * specifies a pattern to be matched, or it specifies an evaluatable
 * atom that must be evaluated to determine if it is to be accepted.
 * (In the default callback, evaluatable atoms are always crisp
 * boolean-logic formulas, although the infrastructure is designed
 * to handle other situations as well, e.g. Bayesian formulas, etc.)
 *
 * `explore_clause_direct()` handles pattern matching, while
 * `explore_clause_evaluatable()` handles the evaluatable ones.
 *
 * Returns true if there was a match, else returns false to reject it.
 */
bool PatternMatchEngine::explore_clause_direct(const PatternTermPtr& term,
                                               const Handle& grnd,
                                               const PatternTermPtr& clause)
{
	// If we are looking for a pattern to match, then ... look for it.
	logmsg("Clause is matchable; start matching it");

	_did_check_forall = false;
	bool found = explore_term_branches(term, grnd, clause);

	if (not _did_check_forall and clause->isAlways())
	{
		// We need to record failures for the AlwaysLink
		Handle empty;
		_forall_state = _forall_state and
			_pmc.always_clause_match(clause->getHandle(), empty, var_grounding);
	}

	return found;
}

bool PatternMatchEngine::explore_clause_evaluatable(const PatternTermPtr& term,
                                                    const Handle& grnd,
                                                    const PatternTermPtr& clause)
{
	logmsg("Clause is evaluatable; start evaluating it");

	// Some people like to have a clause that is just one big
	// giant variable, matching almost anything. Keep these folks
	// happy, and record the suggested grounding. There's nowhere
	// else to do this, so we do it here.
	if (term->isBoundVariable() or term->isGlobbyVar())
		var_grounding[term->getHandle()] = grnd;

	// All variables in the clause had better be grounded!
	OC_ASSERT(is_clause_grounded(clause), "Internal error!");

	bool found = _pmc.evaluate_sentence(clause->getHandle(), var_grounding);
	logmsg("Post evaluating clause, found = ", found);
	if (found)
	{
		return clause_accept(clause, grnd);
	}
	else if (clause->isAlways())
	{
		// We need to record failures for the AlwaysLink
		Handle empty;
		_forall_state = _forall_state and
			_pmc.always_clause_match(clause->getHandle(), empty, var_grounding);
	}

	return false;
}

/**
 * IdenticalLinks are a form of equality, stating that all of the
 * "sides" (all of the elements of the outgoing set; there may be more
 * than two) must be identical. During search, two cases arise:
 * either the sides are already grounded, or they are not. If they're
 * grounded already, then obviously the grounding is rejected if they
 * are not identical.  If one side is not grounded, but another side
 * is, then the IdenticalLink can be treated as an assignment from the
 * grounded side to the ungrounded side. This is what is implemented
 * here: both run-time checking and assignment.
 *
 * Note that some assignments can be done statically, at pattern compile
 * time, but not all cases can be handled at compile time.  An example
 * of an assignment that cannot be done at compile time: `x = (a or b)`
 * where `(a or b)` is a ChoiceLink. The choice is obviously not known
 * until pattern run-time.  Thus, run-time assignment is more general
 * than static analysis at pattern compile time.
 */
bool PatternMatchEngine::explore_clause_identical(const PatternTermPtr& term,
                                                  const Handle& grnd,
                                                  const PatternTermPtr& clause)
{
	// We have not yet reached the IdenticalLink. So keep going.
	if (not term->getParent()->isIdentical())
		return explore_term_branches(term, grnd, clause);

	logmsg("Clause is identity:", clause);

	if (not tree_compare(term, grnd, CALL_SOLN))
	{
		logmsg("iiii NO solution for term:", term);
		logmsg("iiii NOT solved by:", grnd);
		return false;
	}

	const auto& it = _pat->clause_variables.find(clause);
	OC_ASSERT(it != _pat->clause_variables.end(), "Internal Error");
	const HandleSeq& clausevars = it->second;

	// Search for a term in the link that is grounded.
	// Proposed groundings may have ungrounded variables in them.
	// Reject these. Typically, these are self-groundings.
	Handle vterm;
	Handle gterm;
	const HandleSeq& ioset = clause->getHandle()->getOutgoingSet();
	for (const Handle& side : ioset)
	{
		auto gnd = var_grounding.find(side);
		if ((var_grounding.end() != gnd) and
		    not any_free_in_tree(gnd->second, clausevars))
		{
			vterm = side;
			gterm = gnd->second;
			break;
		}
	}
	if (nullptr == gterm) return false;

	logmsg("Identity grounding to validate:", gterm);
	for (const PatternTermPtr& side : clause->getOutgoingSet())
	{
		if (side->getHandle() == vterm) continue;
		if (not tree_compare(side, gterm, CALL_SOLN)) return false;
	}

	// IdenticalLinks are necessarily evaluatable, and thus are
	// necessarily embedded in an evaluatable clause, which we can
	// evaluate from the top. So pass this on for completion.
	return clause_accept(clause, grnd);
}

/**
 * Same as explore_clause_direct, but looks at the cache of pre-grounded
 * clauses, first. This saves some CPU time for certain patterns that
 * have repeated re-explorations of clauses. An example pattern is given
 * below. It's an example of some of the cpu-intensive searches in the
 * genome/proteome/reactome annotation code. Its looking for pathways
 * from start to endpoint. Because multiple paths exist, the endpoint
 * gets searched repeatedly. The caching avoids the repeated search.
 *
 * To make this discussion less abstract and more real, here's an
 * example, from the gene annotation code:

(Evaluation (Predicate "foo") (List (Concept "a") (Concept "start-point")))
(Evaluation (Predicate "foo") (List (Concept "b") (Concept "start-point")))
(Evaluation (Predicate "foo") (List (Concept "c") (Concept "start-point")))

(Evaluation (Predicate "bar") (List (Concept "w") (Concept "end-point")))
(Evaluation (Predicate "bar") (List (Concept "x") (Concept "end-point")))
(Evaluation (Predicate "bar") (List (Concept "y") (Concept "end-point")))
(Evaluation (Predicate "bar") (List (Concept "z") (Concept "end-point")))

; Multiple pathways connecting start and end-points.
(Inheritance (Concept "a") (Concept "z"))
(Inheritance (Concept "b") (Concept "z"))
(Inheritance (Concept "b") (Concept "y"))
(Inheritance (Concept "b") (Concept "x"))
(Inheritance (Concept "c") (Concept "x"))
(Inheritance (Concept "c") (Concept "w"))

(define find-pathways (Get (And
   (Evaluation (Predicate "foo") (List (Variable "$a") (Concept "start-point")))
   (Evaluation (Predicate "bar") (List (Variable "$z") (Concept "end-point")))
   (Inheritance (Variable "$a") (Variable "$z")))))

; (cog-execute! find-pathways)

 * In the above example, the start/end-points would be genes, and the
 * paths would be reactome grid paths. The above pattern has two cache
 * hits, one on "x" and one on the "z" end-point.
 *
 * There's no unit test for this. Caching does hurt some workloads by
 * as much as 5%. It helps the genome-annotation code by 25%.
 *
 * TODO: The implementation here is minimal - very simple, very basic.
 * One could get much fancier. For example, the cache could be held in
 * the atomspace (thus making it effective across multiple searches,
 * e.g. with different start points but with the same end-points.)
 * In this case, maybe the cache should be held in a special Value
 * attached to a clause.  Size and bloat issues occur with atomspace
 * caching; these would need to be managed.
 *
 * A different fancy approach would be to spot more complex recurring
 * sub-patterns, and cache those. This adds considerable complexity,
 * and would become effective only for large, complex searches, of
 * which I haven't seen any good examples of, yet.
 */
bool PatternMatchEngine::explore_clause(const PatternTermPtr& term,
                                        const Handle& grnd,
                                        const PatternTermPtr& pclause)
{
	// The two sides of an identity can be equated directly.
	if (pclause->isIdentical())
		return explore_clause_identical(term, grnd, pclause);

	// Evaluatable clauses are not cacheable.
	if (pclause->hasAnyEvaluatable())
		return explore_clause_evaluatable(term, grnd, pclause);

	// Multiple-choice clauses are not cacheable in the current design.
	// This is because the cache is at the wrong level; the choices are
	// explored in a loop above the cache, instead of below it.
	if (pclause->isChoice())
		return explore_clause_direct(term, grnd, pclause);

	// Build the cache lookup key
	HandleSeq key;

	// Single-variable cache. Due to the way we are called, `term`
	// is a subterm of the clause that contains a variable, and
	// `grnd` is the grounding of that variable.
	const Handle& clause = pclause->getHandle();
	if (_pat->cacheable_clauses.find(clause) != _pat->cacheable_clauses.end())
		key = HandleSeq({clause, grnd});

	// Multi-variable cache.
	if (_pat->cacheable_multi.find(clause) != _pat->cacheable_multi.end())
	{
		const HandleSeq& varseq = _pat->clause_variables.at(pclause);
		key = clause_grounding_key(clause, varseq);
	}

	// No key, nothing to look up.
	if (0 == key.size())
		return explore_clause_direct(term, grnd, pclause);

	const auto& cac = _gnd_cache.find(key);
	if (cac != _gnd_cache.end())
	{
		logmsg("Cache hit!");

		// Record the clause grounding.
		var_grounding[clause] = cac->second;

		// Copy variable groundings, which were stored in the key.
		// Usually, this is not needed; however, if the variable
		// is in the outgoing set of the clause, then the grounding
		// won't have been recorded yet, and so we have to do it here.
		// Tested by `CacheHitUTest`.
		const HandleSeq& clvars(_pat->clause_variables.at(pclause));
		size_t cvsz = clvars.size();
		for (size_t iv=0; iv<cvsz; iv++)
			var_grounding[clvars[iv]] = key[iv+1];

		return do_next_clause();
	}

	// Do we have a negative cache? If so, it will always fail.
	const auto& nac = _nack_cache.find(key);
	if (nac != _nack_cache.end())
	{
		logmsg("NAC Cache hit!", key);
		return false;
	}

	bool okay = explore_clause_direct(term, grnd, pclause);
	if (not okay)
		_nack_cache.insert(key);
	return okay;
}

void PatternMatchEngine::record_grounding(const PatternTermPtr& ptm,
                                          const Handle& hg)
{
	const Handle& hp = ptm->getHandle();

	// If this is a closed pattern, not containing any variables,
	// then there is no need to save it.
	if (hp == hg)
		return;

	// Quoted patterns are tricky. If a pattern is quoted, AND the
	// quote appears in the pattern term, then we record the quote.
	// Otherwise, just record the raw grounding.
	// Tested in UnorderedUTest::test_quote() and elsewhere.
	if (not ptm->isQuoted())
		var_grounding[hp] = hg;
	else if (const Handle& quote = ptm->getQuote())
		var_grounding[quote] = hg;
	else
		var_grounding[hp] = hg;
}

/**
 * Clear current traversal state. This gets us into a state where we
 * can start traversing a set of clauses.
 */
void PatternMatchEngine::clear_current_state(void)
{
	// Clear all state.
	var_grounding.clear();
	clause_grounding.clear();

	depth = 0;

	// Choice state
	_choice_state.clear();
	_need_choice_push = false;
	_choose_next = true;

	// UnorderedLink state
	_perm_have_more = false;
	_perm_take_step = false;
	_perm_go_around = false;
	_perm_to_step = nullptr;
	_perm_breakout = nullptr;
	_perm_state.clear();
	_perm_odo.clear();
	_perm_podo.clear();
	_perm_odo_state.clear();

	// GlobNode state
	_glob_state.clear();
}

bool PatternMatchEngine::explore_constant_evaluatables(const PatternTermSeq& clauses)
{
	bool found = true;
	for (const PatternTermPtr& clause : clauses)
	{
		if (clause->hasAnyEvaluatable())
		{
			found = _pmc.evaluate_sentence(clause->getHandle(), GroundingMap());
			if (not found)
				break;
		}
	}
	if (found)
		report_grounding(GroundingMap(), GroundingMap());

	return found;
}

PatternMatchEngine::PatternMatchEngine(PatternMatchCallback& pmcb)
	: _pmc(pmcb),
	_nameserver(nameserver()),
	_variables(nullptr),
	_pat(nullptr),
	clause_accepted(false)
{
	// current state
	depth = 0;

	// graph state
	_clause_stack_depth = 0;

	// choice link state
	_need_choice_push = false;
	_choose_next = true;

	// unordered link state
	_perm_have_more = false;
	_perm_take_step = false;
	_perm_go_around = false;
	_perm_to_step = nullptr;
	_perm_breakout = nullptr;
	_perm_odo.clear();
	_perm_podo.clear();
	_perm_odo_state.clear();
}

void PatternMatchEngine::set_pattern(const Variables& v,
                                     const Pattern& p)
{
	_variables = &v;
	_pat = &p;
}

/* ======================================================== */

#ifdef QDEBUG
void PatternMatchEngine::print_solution(
	const GroundingMap &vars,
	const GroundingMap &clauses)
{
	Logger::Level save = logger().get_level();
	logger().set_level("fine");
	logger().set_timestamp_flag(false);
	logger().set_print_to_stdout_flag(true);
	logger().set_print_level_flag(false);
	log_solution(vars, clauses);

	logger().set_level(save);
}

void PatternMatchEngine::log_solution(
	const GroundingMap &vars,
	const GroundingMap &clauses)
{
	if (!logger().is_fine_enabled())
		return;

	logger().fine() << "There are groundings for " << vars.size() << " terms";
	int varcnt = 0;
	for (const auto& j: vars)
	{
		Type vtype = j.first->get_type();
		if (VARIABLE_NODE == vtype or GLOB_NODE == vtype) varcnt++;
	}
	logger().fine() << "Groundings for " << varcnt << " variables:";

	// Print out the bindings of solutions to variables.
	for (const auto& j: vars)
	{
		const Handle& var(j.first);
		const Handle& soln(j.second);

		// Only print grounding for variables.
		Type vtype = var->get_type();
		if (VARIABLE_NODE != vtype and GLOB_NODE != vtype) continue;

		if (not soln)
		{
			logger().fine("ERROR: ungrounded variable %s\n",
			              var->to_short_string().c_str());
			continue;
		}

		logger().fine("\t%s maps to %s\n",
		              var->to_short_string().c_str(),
		              soln->to_short_string().c_str());
	}

	// Print out the full binding to all of the clauses.
	logger().fine() << "Groundings for " << clauses.size() << " clauses:";
	GroundingMap::const_iterator m;
	int i = 0;
	for (m = clauses.begin(); m != clauses.end(); ++m, ++i)
	{
		// AbsentLink's won't be grounded...
		if (not m->second)
		{
			logmsg("Ungrounded (absent) clause", m->first);
			continue;
		}
		std::string str = m->second->to_short_string("       ");
		logger().fine("Clause %d:\n%s", i, str.c_str());
	}
}

/**
 * For debug logging only
 */
void PatternMatchEngine::log_term(const HandleSet &vars,
                                  const HandleSeq &clauses)
{
	logger().fine("Clauses:");
	for (Handle h : clauses)
		LAZY_LOG_FINE << h->to_short_string();

	logger().fine("Vars:");
	for (Handle h : vars)
		LAZY_LOG_FINE << h->to_short_string();
}
#else

void PatternMatchEngine::log_solution(const GroundingMap &vars,
                                      const GroundingMap &clauses) {}

void PatternMatchEngine::log_term(const HandleSet &vars,
                                  const HandleSeq &clauses) {}
#endif

/* ===================== END OF FILE ===================== */
