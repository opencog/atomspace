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

#include <opencog/util/algorithm.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/Logger.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atoms/pattern/PatternUtils.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

#include "PatternMatchEngine.h"


using namespace opencog;
/* ======================================================== */
/**
 * Pattern Match Engine Overview
 * -----------------------------
 * The explanation of how this code *actually* works has been
 * moved to README-Algorithm. That README provies the "big-picture"
 * explanation of the rather complex interwoven code below. Read it
 * first, and refert to it when examining the main methods below.
 */
/* ======================================================== */

// The macro POPSTK has an OC_ASSERT when DEBUG is defined, so we keep
// that #define around, although it's not clear why that OC_ASSERT
// wouldn't be kept no matter what (it's not like it's gonna take up a
// lot of resources).

#define DEBUG 1
#ifdef DEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif


#ifdef DEBUG
static inline void log(const Handle& h)
{
	LAZY_LOG_FINE << h->to_short_string();
}

static inline void logmsg(const char * msg, const Handle& h)
{
	LAZY_LOG_FINE << msg << std::endl
	              << (h == (Atom*) nullptr ?
	                  std::string("(invalid handle)") :
	                  h->to_short_string());
}
#else
static inline void logmsg(const char * msg, const Handle& h) {}
static inline void log(const Handle& h) {}
#endif


/* ======================================================== */
/* Reset the current variable grounding to the last grounding pushed
 * onto the stack. */
#ifdef DEBUG
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
		DO_LOG({LAZY_LOG_FINE << "Found grounding of variable:";})
		logmsg("$$ variable:", hp);
		logmsg("$$ ground term:", hg);
		var_grounding[hp] = hg;
	}
	return true;
}

/* ======================================================== */

/// Compare an atom to itself. Amazingly, this is more complicated
/// that what it might seem to be ...
///
/// If they're the same atom, then clearly they match.
/// ... but only if atom is a constant (i.e. contains no bound variables)
///
bool PatternMatchEngine::self_compare(const PatternTermPtr& ptm)
{
	const Handle& hp = ptm->getHandle();
	if (not ptm->isQuoted()) var_grounding[hp] = hp;

	logmsg("Compare atom to itself:", hp);
	return true;
}

/* ======================================================== */

/// Compare two nodes, one in the pattern, one proposed grounding.
bool PatternMatchEngine::node_compare(const Handle& hp,
                                      const Handle& hg)
{
	// Call the callback to make the final determination.
	bool match = _pmc.node_match(hp, hg);
	if (match)
	{
		DO_LOG({LAZY_LOG_FINE << "Found matching nodes";})
		logmsg("# pattern:", hp);
		logmsg("# match:", hg);
		if (hp != hg) var_grounding[hp] = hg;
	}
	return match;
}

/* ======================================================== */

/// If the two links are both ordered, its enough to compare
/// them "side-by-side".
bool PatternMatchEngine::ordered_compare(const PatternTermPtr& ptm,
                                         const Handle& hg)
{
	PatternTermSeq osp = ptm->getOutgoingSet();
	const HandleSeq &osg = hg->getOutgoingSet();

	size_t osg_size = osg.size();
	size_t osp_size = osp.size();

	// The recursion step: traverse down the tree.
	// In principle, we could/should push the current groundings
	// onto the stack before recursing, and then pop them off on
	// return.  Failure to do so could leave some bogus groundings,
	// sitting around, i.e. groundings that were found during
	// recursion but then discarded due to a later mis-match.
	//
	// In practice, I was unable to come up with any test case
	// where this mattered; any bogus groundings eventually get
	// replaced by valid ones.  Thus, we save some unknown amount
	// of cpu time by simply skipping the push & pop here.
	//
	depth ++;

	// If the pattern contains no globs, then the pattern and ground
	// must match exactly, term by term. If the pattern has globs,
	// perform glob-matching (which can be thought of as a well-defined
	// kind of fuzzy matching). If there are no globs, and the arity is
	// mis-matched, then perform fuzzy matching.
	bool match = true;
	if (0 == _pat->globby_terms.count(ptm->getHandle()))
	{
		// If the arities are mis-matched, do a fuzzy compare instead.
		if (osp_size != osg_size)
		{
			match = _pmc.fuzzy_match(ptm->getHandle(), hg);
		}
		else
		{
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
	else
	{
		// If we are here, then the pattern contains globs. A glob can
		// match one or more atoms in a row. Thus, we have a more
		// complicated search ...
		match = glob_compare(osp, osg);
	}

	depth --;
	DO_LOG({LAZY_LOG_FINE << "tree_comp down link match=" << match;})

	const Handle &hp = ptm->getHandle();
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
	if (hp != hg) var_grounding[hp] = hg;

	return true;
}

/* ======================================================== */

/// Compare a ChoiceLink in the pattern to the proposed grounding.
/// hp points at the ChoiceLink.
///
/// CHOICE_LINK's are multiple-choice links. As long as we can
/// can match one of the sub-expressions of the ChoiceLink, then
/// the ChoiceLink as a whole can be considered to be grounded.
///
bool PatternMatchEngine::choice_compare(const PatternTermPtr& ptm,
                                        const Handle& hg)
{
	const Handle& hp = ptm->getHandle();
	PatternTermSeq osp = ptm->getOutgoingSet();

	// _choice_state lets use resume where we last left off.
	size_t iend = osp.size();
	bool fresh = false;
	size_t icurr = curr_choice(ptm, hg, fresh);
	if (fresh) choose_next = false; // took a step, clear the flag

	DO_LOG({LAZY_LOG_FINE << "tree_comp resume choice search at " << icurr
	              << " of " << iend << " of term=" << ptm->to_string()
	              << ", choose_next=" << choose_next;})

	// XXX This is almost surely wrong... if there are two
	// nested choice links, then this will hog the steps,
	// and the deeper choice will fail.
	if (choose_next)
	{
		icurr++;
		choose_next = false; // we are taking a step, so clear the flag.
	}

	while (icurr<iend)
	{
		solution_push();
		const PatternTermPtr& hop = osp[icurr];

		DO_LOG({LAZY_LOG_FINE << "tree_comp or_link choice " << icurr
		              << " of " << iend;})

		bool match = tree_compare(hop, hg, CALL_CHOICE);
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
				if (hp != hg) var_grounding[hp] = hg;

				_choice_state[GndChoice(ptm, hg)] = icurr;
				return true;
			}
		}
		else
		{
			_pmc.post_link_mismatch(hp, hg);
		}
		solution_pop();
		choose_next = false; // we are taking a step, so clear the flag.
		icurr++;
	}

	// If we are here, we've explored all the possibilities already
	_choice_state.erase(GndChoice(ptm, hg));
	return false;
}

/// Return the current choice state for the given pattern & ground
/// combination.
size_t PatternMatchEngine::curr_choice(const PatternTermPtr& ptm,
                                       const Handle& hg,
                                       bool& fresh)
{
	auto cs = _choice_state.find(GndChoice(ptm, hg));
	if (_choice_state.end() == cs)
	{
		fresh = true;
		return 0;
	}
	return cs->second;
}

bool PatternMatchEngine::have_choice(const PatternTermPtr& ptm,
                                     const Handle& hg)
{
	return 0 < _choice_state.count(GndChoice(ptm, hg));
}

/* ======================================================== */
static int facto (int n) { return (n==1)? 1 : n * facto(n-1); };

/// Unordered link comparison
///
/// Compare two unordered links, side by side. In some ways, this is
/// similar to the ordered link compare: for a given, fixed permuation
/// of the unordered link, the compare is side by side.  However, if
/// that compare fails, the next permuation must then be tried, until
/// a match is found or all permuations are exhausted.  But there's a
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
we may be asked to compare things exactly like last time, or we may be
asked to try out the next permutation. We need to report back two bits
of state: whether or not we found a match, and whether or not we have
more possibilities to explore. Our behavior is thus controlled by this
third bit, as well as what happened below us.

The correct actions to take are best explored by truth tables: the
settings of the take_step and have_more flags on entry, and what to
do after running tree_compare downwards. These are handled by two
truth tables.

The topmost routine to call tree_compare must *always* set have_more=F
and take_step=T before calling tree_compare.  This will cause
tree_compare to advance to the next matching permuation, or to run until
all permuations are exhausted, and no match was found.


Flag settings upon entry
------------------------

     take  have
case step  more    Comments / Action to be Taken
---- ----  ----    -----------------------------
  A    T    T     Impossible, Who set have_more? Who set take_step?
  B    T    F     Normal entry: we are first unordered link to be hit
                  during downward travesal. Proceed using the truth
                  table below.
  C    F    T     We are not the first unorder. Someone ran before us.
                  If we are in the very first permuation, (i.e. we are
                    entering for the first time) we must call downwards.
                    If this returns a mismatch, we must step until we
                    find a match, or until we exhaust all permuatations.
                    See next table for what to return: we return cases
                    5, 7 or 8.
                  If we are not the first permutation, we could just
                    return T, because that is what we did last time.
                    i.e. we are told to not take a step, so we don't,
                    and we know a-priori last time were were here, we
                    must have returned a match.  Thus, we can return
                    case 5 below.  We cannot return case 7 because we
                    can't know what std::next_perm returns.
                    (See however, footnote below).
                  If we hold an evaluatable, we must call down.
  D    F    F     Perform same as C above.

Footnote: case C: Well, the reasoning there is almost right, but not
quite. If the unordered link contains a variable, and it is also not in
the direct line of exploration (i.e. its grounding is NOT recorded)
then its truthiness holds only for a grounding that no longer exists.
Thus, for case C, it is safer to always check.

However, by the above reasoning: if the grounding wasn't recorded
(because the link is not in the recursion path) then the permuation
should not be recorded either. It should start with a permutation from
nothing.  XXX FIXME ... except we have no test case that illustrates
this failure mode.  It would require a peer unordered link that takes
a different order when the parent changes. Perhaps unordered links
nested inside a ChoiceLink would trigger this?

If case B was encountered on entry, we call downwards ourselves, and
then report back, according to the truth table below.

     returned result flags
     take  have   got
case step  more  match    Comments / Action to tbe Taken
---- ----  ----  ----     ------------------------------
 1     T    T      T    Impossible, error: who set have_more w/o
                          taking step??
 2     T    T      F    ditto
 3     T    F      T    We have no unordered links below us; we are at
                        the bottom.  If there had been unordered links,
                        they would have cleared the take_step flag. The
                        match we detected is the same match the last
                        time we were here. So we take a step, call
                        down again, and keep stepping until there is a
                        match or until all permuations are exhausted.
                          If match, we return: take_step=F,
                            have_more = result of std::next_perm
                            (we return case 5 or 7)
                          If exhaust, we return take_step=F, have_more=F
                            (we return case 8)

 4     T    F      F    If we are not holding any evaluatable links,
                        then this is impossible, as last time we were
                        here, we returned T.  If we are holding
                        evaluatable links, then one of them changed its
                        mind. Oh well. Take a step, proceed as in case 3.

 5     F    T      T    Someone below us took a step. Do nothing.
                        Return case 5 flags.
 6     F    T      F    Impossible; link that took the step should have
                        stepped until match or exhastion.
 7     F    F      T    Unusual; its the last match for a lower unordered
                        link. We report the match. We do not take a
                        step; we do set the have_more flag to indicate
                        that we ourselves still have more.  i.e We return
                        case 5 flags.
 8     F    F      F    Typical; a lower unord link ran to exhaustion,
                        and got nada.  So *we* take a step, and call
                        downwards again. We keep  going till match or till
                        exhaustion. If there's a match, we expect to see
                        the case 5 flags, so we halt and return.  If we
                        exhaust, we return case 8.

The above assumes that the curr_perm() method always returns either
the current permuation, or it returns a fresh permuation. If it returned
a fresh permuation, this counts as "taking a step", so we need to know
this.

Notice that these rules never pushed of popped the have-more stack.
The have-more stack is only pushed/popped by other branchpoints, before
they call compare_tree.

******************************************************************/

bool PatternMatchEngine::unorder_compare(const PatternTermPtr& ptm,
                                         const Handle& hg)
{
	const Handle& hp = ptm->getHandle();
	const HandleSeq& osg = hg->getOutgoingSet();
	PatternTermSeq osp = ptm->getOutgoingSet();
	size_t arity = osp.size();
	bool has_glob = (0 < _pat->globby_holders.count(ptm->getHandle()));

	// They've got to be the same size, at the least!
	// unless there are globs in the pattern
	if (osg.size() != arity and not has_glob)
		return _pmc.fuzzy_match(ptm->getHandle(), hg);

	// Test for case A, described above.
	OC_ASSERT (not (take_step and have_more),
	           "Impossible situation! BUG!");

	// _perm_state lets use resume where we last left off.
	bool fresh = false;
	Permutation mutation = curr_perm(ptm, hg, fresh);
	if (fresh) take_step = false; // took a step, clear the flag.

	// Cases C and D fall through.
	// If we are here, we've got possibilities to explore.
#ifdef DEBUG
	int num_perms = 0;
	if (logger().is_fine_enabled())
	{
		num_perms = facto(mutation.size());
		logger().fine("tree_comp resume unordered search at %d of %d of term=%s "
		              "take_step=%d have_more=%d\n",
		              perm_count[Unorder(ptm, hg)], num_perms,
		              ptm->to_string().c_str(), take_step, have_more);
	}
#endif
	do
	{
		DO_LOG({LAZY_LOG_FINE << "tree_comp explore unordered perm "
		              << perm_count[Unorder(ptm, hg)] << " of " << num_perms
		              << " of term=" << ptm->to_string();})
		solution_push();
		bool match = true;

		if (has_glob)
		{
			// Each glob comparision steps the glob state forwards.
			// Each different permutation has to start with the
			// same glob state as before. So save and restore state.
			std::map<GlobPair, GlobState> saved_glob_state = _glob_state;
			match = glob_compare(mutation, osg);
			_glob_state = saved_glob_state;
		}
		else
		{
			for (size_t i=0; i<arity; i++)
			{
				if (not tree_compare(mutation[i], osg[i], CALL_UNORDER))
				{
					match = false;
					break;
				}
			}
		}

		// Check for cases 1&2 of description above.
		// The step-next may have been taken by someone else, in the
		// tree_compare immediate above.
		OC_ASSERT(not (take_step and have_more),
		          "This shouldn't happen. Impossible situation! BUG!");

		// Handle cases 3&4 of the description above. Seems wisest
		// to do this before post_link_match() !??
		if (take_step and not have_more)
		{
			OC_ASSERT(match or (0 < _pat->evaluatable_holders.count(hp)),
			          "Impossible: should have matched!");
			goto take_next_step;
		}

		// If we are here, then take_step is false, because
		// cases 1,2,3,4 already handled above.
		// Well, actually, this can happen, if we are not careful
		// to manage the have_more flag on a stack.
//		OC_ASSERT(match or not have_more or 1==num_perms,
//		          "Impossible: case 6 happened!");

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
				if (hp != hg) var_grounding[hp] = hg;

				// Handle case 5&7 of description above.
				have_more = true;
				DO_LOG({LAZY_LOG_FINE << "Good permutation "
				              << perm_count[Unorder(ptm, hg)]
				              << " for term=" << ptm->to_string()
				              << " have_more=" << have_more;})
				_perm_state[Unorder(ptm, hg)] = mutation;
				return true;
			}
		}
		else
		{
			_pmc.post_link_mismatch(hp, hg);
		}
		// If we are here, we are handling case 8.
		DO_LOG({LAZY_LOG_FINE << "Above permuation " << perm_count[Unorder(ptm, hg)]
		              << " failed term=" << ptm->to_string();})

take_next_step:
		take_step = false; // we are taking a step, so clear the flag.
		have_more = false; // start with a clean slate...
		solution_pop();
		if (logger().is_fine_enabled())
			perm_count[Unorder(ptm, hg)] ++;
	} while (std::next_permutation(mutation.begin(), mutation.end()));

	// If we are here, we've explored all the possibilities already
	DO_LOG({LAZY_LOG_FINE << "Exhausted all permuations of term=" << ptm->to_string();})
	_perm_state.erase(Unorder(ptm, hg));
	have_more = false;
	return false;
}


/// Compare a PatternTermPtr with a clause taking in consideration
/// quoting or unquoting operations.
bool PatternMatchEngine::clause_compare(const PatternTermPtr& ptm,
                                        const Handle& clause)
{
	return ptm->getHandle() == clause
		or (Quotation::is_quotation_type(clause->get_type())
		    and ptm->getHandle() == clause->getOutgoingAtom(0));
}

/// Return the saved unordered-link permutation for this
/// particular point in the tree comparison (i.e. for the
/// particular unordered link hp in the pattern.)
PatternMatchEngine::Permutation
PatternMatchEngine::curr_perm(const PatternTermPtr& ptm,
                              const Handle& hg,
                              bool& fresh)
{
	auto ps = _perm_state.find(Unorder(ptm, hg));
	if (_perm_state.end() == ps)
	{
		DO_LOG({LAZY_LOG_FINE << "tree_comp fresh start unordered link term="
		              << ptm->to_string();})
		Permutation perm = ptm->getOutgoingSet();
		sort(perm.begin(), perm.end());
		fresh = true;
		return perm;
	}
	return ps->second;
}

/// Return true if there are more permutations to explore.
/// Else return false.
bool PatternMatchEngine::have_perm(const PatternTermPtr& ptm,
                                   const Handle& hg)
{
	if (_perm_state.end() == _perm_state.find(Unorder(ptm, hg)))
		return false;
	return true;
}

void PatternMatchEngine::perm_push(void)
{
	perm_stack.push(_perm_state);
	if (logger().is_fine_enabled())
		perm_count_stack.push(perm_count);
}

void PatternMatchEngine::perm_pop(void)
{
	POPSTK(perm_stack, _perm_state);
	if (logger().is_fine_enabled())
		POPSTK(perm_count_stack, perm_count);
}

/* ======================================================== */

/// Compare the outgoing sets of two trees side-by-side, where
/// the pattern contains at least one GlobNode.
bool PatternMatchEngine::glob_compare(const PatternTermSeq& osp,
                                      const HandleSeq& osg)
{
	bool match = true;
	GlobPair gp = {osp, osg};
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
			_glob_state[gp] = {glob_grd, glob_pos_stack};
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
		_glob_state[gp] = {glob_grd, glob_pos_stack};

		Handle glp(createLink(glob_seq, LIST_LINK));
		var_grounding[glob->getHandle()] = glp;

		DO_LOG({LAZY_LOG_FINE << "Found grounding of glob:";})
		logmsg("$$ glob:", glob->getHandle());
		logmsg("$$ ground term:", glp);
	};

	// Common things needed to be done when it's not a match.
	auto mismatch = [&]()
	{
		match = false;
		_glob_state.erase(gp);
	};

	// Resume the matching from a previous state.
	// i.e. we had successfully grounded osp to osg, but it
	// turns out the groundings do not satisfy some other terms
	// in the same pattern, so we try again and see if the globs
	// in osp can be grounded differently.
	auto r = _glob_state.find(gp);
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

		const Handle& ohp(osp[ip]->getHandle());
		Type ptype = ohp->get_type();

		if (GLOB_NODE == ptype)
		{
			HandleSeq glob_seq;
			PatternTermPtr glob(osp[ip]);

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
				_glob_state[gp] = {glob_grd, glob_pos_stack};
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
			if (_varlist->is_lower_bound(ohp, 0))
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
				if (not _varlist->is_upper_bound(ohp, 1))
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
			bool tc;
			do
			{
				tc = tree_compare(glob, osg[jg], CALL_GLOB);
				if (tc)
				{
					// Can't match more than it did last time.
					if (glob_seq.size()+1 >= last_grd)
					{
						jg--;
						break;
					}

					// Can't exceed the upper bound.
					if (not _varlist->is_upper_bound(ohp, glob_seq.size()+1))
					{
						jg--;
						break;
					}

					glob_seq.push_back(osg[jg]);

					// See if we can match the next one.
					jg++;
				}
				// Can't match more, e.g. a type mis-match
				else jg--;
			} while (tc and jg<osg_size);

			// Try again if we can't ground the glob after all.
			if (0 == glob_seq.size())
			{
				backtrack(true);
				continue;
			}

			// Try again if we can't ground enough atoms to satisfy
			// the lower bound restriction.
			if (not _varlist->is_lower_bound(ohp, glob_seq.size()))
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
/**
 * tree_compare compares two trees, side-by-side.
 *
 * Compare two incidence trees, side-by-side.  The incidence tree is
 * given by following the "outgoing set" of the links appearing in the
 * tree.  The incidence tree is the so-called "Levi graph" of the
 * hypergraph.  The first arg should be a handle to a clause in the
 * pattern, while the second arg is a handle to a candidate grounding.
 * The pattern (template) clause is compared to the candidate grounding,
 * returning true if there is a mis-match.
 *
 * The comparison is recursive, so this method calls itself on each
 * subtree (term) of the template clause, performing comparisons until a
 * match is found (or not found).
 *
 * Return false if there's a mis-match. The goal here is to walk over
 * the entire tree, without mismatches.  Since a return value of false
 * stops the iteration, false is used to signal a mismatch.
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
 * That method have side effects. The main one is to insert variable
 * groundings (and in fact sub-clauses grounding as well) in
 * var_grounding when encountering variables (and sub-clauses) in the
 * pattern.
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

	// If the pattern link is executable, then we should execute, and
	// use the result of that execution. (This isn't implemented yet,
	// because all variables in an executable link need to be grounded,
	// before the execution can occur... thus, this needs to be handled
	// a lot like a VirtualLink.)
	if (is_executable(hp))
		throw RuntimeException(TRACE_INFO, "Not implemented!!");

	Type tp = hp->get_type();

	// If the pattern is a DefinedSchemaNode, we need to substitute
	// its definition. XXX TODO.
	if (DEFINED_SCHEMA_NODE == tp)
		throw RuntimeException(TRACE_INFO, "Not implemented!!");

	// Handle hp is from the pattern clause, and it might be one
	// of the bound variables. If so, then declare a match.
	if (not ptm->isQuoted())
	{
		if (_varlist->varset.end() != _varlist->varset.find(hp))
			return variable_compare(hp, hg);

		// Report other variables that might be found.
		if (VARIABLE_NODE == tp)
			return _pmc.scope_match(hp, hg);
	}

	// If they're the same atom, then clearly they match.
	// ... but only if hp is a constant i.e. contains no bound variables)
	//
	// If the pattern contains atoms that are evaluatable i.e. GPN's
	// then we must fall through, and let the tree comp mechanism
	// find and evaluate them. That's for two reasons: (1) because
	// evaluation may have side-effects (e.g. send a message) and
	// (2) evaluation may depend on external state. These are
	// typically used to implement behavior trees, e.g SequenceUTest
	if ((hp == hg) and not is_evaluatable(hp))
		return self_compare(ptm);

	// If both are nodes, compare them as such.
	if (hp->is_node() and hg->is_node())
		return node_compare(hp, hg);

	// CHOICE_LINK's are multiple-choice links. As long as we can
	// can match one of the sub-expressions of the ChoiceLink, then
	// the ChoiceLink as a whole can be considered to be grounded.
	// Note, we must do this before the fuzzy_match below, because
	// hg might be a node (i.e. we compare a choice of nodes to one
	// node).
	if (CHOICE_LINK == tp)
		return choice_compare(ptm, hg);

	// If they're not both links, then it is clearly a mismatch.
	if (not (hp->is_link() and hg->is_link())) return _pmc.fuzzy_match(hp, hg);

	// Let the callback perform basic checking.
	bool match = _pmc.link_match(ptm, hg);
	if (not match) return false;

	DO_LOG({LAZY_LOG_FINE << "depth=" << depth;})
	logmsg("tree_compare:", hp);
	logmsg("to:", hg);

	// If the two links are both ordered, its enough to compare
	// them "side-by-side".
	if (2 > hp->get_arity() or _nameserver.isA(tp, ORDERED_LINK))
		return ordered_compare(ptm, hg);

	// If we are here, we are dealing with an unordered link.
	return unorder_compare(ptm, hg);
}

/* ======================================================== */

/*
 * The input pattern may contain many repeated sub-patterns. For example:
 *
 * ImplicationLink
 *   UnorderedLink
 *     VariableNode "$x"
 *     ConceptNode "this one"
 *   UnorderedLink
 *     VariableNode "$x"
 *     ConceptNode "this one"
 *
 * Suppose that we start searching the clause from VariableNode "$x" that
 * occures twice in the pattern under UnorderedLink. While we traverse
 * the pattern recursively we need to keep current state of permutations
 * of UnorderedLink-s. We do not know which permutation will match. It may
 * be different permutation for each occurence of UnorderedLink-s.
 * This is the reason why we use PatternTerm pointers instead of atom Handles
 * while traversing pattern tree. We need to keep permutation states for
 * each term pointer separately.
 *
 * Next suppose our joining atom repeats in several sub-branches of a single
 * ChoiceLink. For example:
 *
 * ChoiceLink
 *   UnorderedLink
 *     VariableNode "$x"
 *     ConceptNode "this one"
 *   UnorderedLink
 *     VariableNode "$x"
 *     ConceptNode "this one"
 *
 * We start pattern exploration for each occurence of joining atom. This
 * is required, due to the pruning done in explore_choice_branches()
 * when the first match is found. XXX This may need to be refactored.
 * For now, we iterate over all pattern terms associated with a given
 * atom handle.
 *
 */
bool PatternMatchEngine::explore_term_branches(const Handle& term,
                                               const Handle& hg,
                                               const Handle& clause_root)
{
	// The given term may appear in the clause in more than one place.
	// Each distinct location should be explored separately.
	auto pl = _pat->connected_terms_map.find({term, clause_root});
	OC_ASSERT(_pat->connected_terms_map.end() != pl, "Internal error");

	for (const PatternTermPtr &ptm : pl->second)
	{
		if (explore_link_branches(ptm, hg, clause_root))
			return true;
	}
	return false;
}

/// explore_up_branches -- look for groundings for the given term.
///
/// The argument passed to this function is a term that needs to be
/// grounded. One of this term's children has already been grounded:
/// the term's child is in `hp`, and the corresponding grounding is
/// in `hg`.  Thus, if the argument is going to be grounded, it will
/// be grounded by some atom in the incoming set of `hg`. Viz, we are
/// walking upwards in these trees, in lockstep.
///
/// This method wraps the major branch-point of the entire pattern
/// matching process. Each element of the incoming set is the start of
/// a different possible branch to be explored; each one might yeild
/// a grounding. Thus, when backtracking, after a failed grounding in
/// one branch, we backtrack to here, and try another branch. When
/// backtracking, all state must be popped and pushed again, to enter
/// the new branch. We don't pushd & pop here, we push-n-pop in the
/// explore_link_branches() method.
///
/// This method is part of a recursive chain that only terminates
/// when a grounding for *the entire pattern* was found (and the
/// grounding was accepted) or if all possibilities were exhaustively
/// explored.  Thus, this returns true only if entire pattern was
/// grounded.
///
bool PatternMatchEngine::explore_up_branches(const PatternTermPtr& ptm,
                                             const Handle& hg,
                                             const Handle& clause_root)
{
	// Move up the solution graph, looking for a match.
	IncomingSet iset = _pmc.get_incoming_set(hg);
	size_t sz = iset.size();
	DO_LOG({LAZY_LOG_FINE << "Looking upward for term=" << ptm->to_string()
	              << " have " << sz << " branches";})

	// Check if the pattern has globs in it.
	bool has_glob = (0 < _pat->globby_holders.count(ptm->getHandle()));
	size_t gstate_size = SIZE_MAX;

	bool found = false;
	for (size_t i = 0; i < sz; i++)
	{
		DO_LOG({LAZY_LOG_FINE << "Try upward branch " << i+1 << " of " << sz
		              << " for term=" << ptm->to_string()
		              << " propose=" << Handle(iset[i]).value();})

		// Before exploring the link branches, record the current
		// _glob_state size.  The idea is, if the ptm & hg is a match,
		// their state will be recorded in _glob_state, so that one can,
		// if needed, resume and try to ground those globs again in a
		// different way (e.g. backtracking from another branchpoint).
		// If there are no more possible ways to ground them, they
		// will be removed from glob_state. So simply by comparing the
		// _glob_state size before and after seems to be an OK way to
		// quickly check if we can move on to the next one or not.
		std::map<GlobPair, GlobState> saved_glob_state;
		if (has_glob)
		{
			saved_glob_state = _glob_state;
			gstate_size = _glob_state.size();
		}

		found = explore_link_branches(ptm, Handle(iset[i]), clause_root);

		// If there may be another way to ground it differently to the same
		// candidate, do it until exhausted.
		while (not found and has_glob and _glob_state.size() > gstate_size)
		{
			found = explore_link_branches(ptm, Handle(iset[i]), clause_root);
		}

		// Restore the saved state, for the next go-around.
		if (has_glob)
			_glob_state = saved_glob_state;

		if (found) break;
	}

	DO_LOG({LAZY_LOG_FINE << "Found upward soln = " << found;})
	return found;
}

/// explore_link_branches -- verify the suggested grounding.
///
/// There are two ways to understand this method. In the "simple" case,
/// where there are no unordered links, and no ChoiceLinks, this becomes
/// a simple wrapper around tree_compare(), and it just returns true or
/// false to indicate if the suggested grounding `hg` actually is a
/// match for the current term being grounded. Before calling
/// tree_compare(), it pushes all current state, and then pops it upon
/// return. In other words, this encapsulates a single up-branch
/// (incoming-set branch): grounding of that single branch succeeds or
/// fails. Failure backtracks to the caller of this method; upon return,
/// the current state has been restored; this routine leaves the current
/// state as it found it. For the simple case, this method is mis-named:
/// it should be called "explore_one_branch".
///
/// The non-simple case is a pattern that includes ChoiceLinks or
/// unordered links. These links represent branch-points themselves.
/// A ChoiceLink of arity N wraps N different possible branches to be
/// explored. An unordered link of arity N wraps N-factorial different
/// possible permuations, each of which must be explored. This method
/// controls the exploration of these different branches. For each
/// possible branch, it saves state, explores the branch, and pops the
/// state. If the exploration yielded nothing, then the next branch is
/// explored, until exhaustion of the possibilities.  Upon exhaustion,
/// it returns to the caller.
///
/// This method is part of a recursive chain that only terminates
/// when a grounding for *the entire pattern* was found (and the
/// grounding was accepted) or if all possibilities were exhaustively
/// explored.  Thus, this returns true only if entire pattern was
/// grounded.
///
bool PatternMatchEngine::explore_link_branches(const PatternTermPtr& ptm,
                                               const Handle& hg,
                                               const Handle& clause_root)
{
	const Handle& hp = ptm->getHandle();

	// If its not an unordered link, then don't try to iterate over
	// all permutations.
	Type tp = hp->get_type();
	if (not _nameserver.isA(tp, UNORDERED_LINK))
		return explore_choice_branches(ptm, hg, clause_root);

	do {
		// If the pattern was satisfied, then we are done for good.
		if (explore_choice_branches(ptm, hg, clause_root))
			return true;

		DO_LOG({logger().fine("Step to next permuation");})
		// If we are here, there was no match.
		// On the next go-around, take a step.
		take_step = true;
		have_more = false;
	} while (have_perm(ptm, hg));

	DO_LOG({logger().fine("No more unordered permutations");})

	return false;
}

/// See explore_link_branches() for a general explanation. This method
/// handles the ChoiceLink branch alternatives only.  It assumes
/// that the caller had handled the unordered-link alternative branches.
bool PatternMatchEngine::explore_choice_branches(const PatternTermPtr& ptm,
                                                 const Handle& hg,
                                                 const Handle& clause_root)
{
	const Handle& hp = ptm->getHandle();
	// If its not a choice link, then don't try to iterate.
	if (CHOICE_LINK != hp->get_type())
		return explore_single_branch(ptm, hg, clause_root);

	DO_LOG({logger().fine("Begin choice branchpoint iteration loop");})
	do {
		// XXX This `need_choice_push` thing is probably wrong; it probably
		// should resemble the perm_push() used for unordered links.
		// However, currently, no test case trips this up. so .. OK.
		// Whatever. This still probably needs fixing.
		if (_need_choice_push) choice_stack.push(_choice_state);
		bool match = explore_single_branch(ptm, hg, clause_root);
		if (_need_choice_push) POPSTK(choice_stack, _choice_state);
		_need_choice_push = false;

		// If the pattern was satisfied, then we are done for good.
		if (match)
			return true;

		DO_LOG({logger().fine("Step to next choice");})
		// If we are here, there was no match.
		// On the next go-around, take a step.
		choose_next = true;
	} while (have_choice(ptm, hg));

	DO_LOG({logger().fine("Exhausted all choice possibilities"
	              "\n----------------------------------");})
	return false;
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
                                               const Handle& clause_root)
{
	solution_push();

	DO_LOG({LAZY_LOG_FINE << "Checking pattern term=" << ptm->to_string()
	              << " for soln by " << hg.value();})

	bool match = tree_compare(ptm, hg, CALL_SOLN);

	if (not match)
	{
		DO_LOG({LAZY_LOG_FINE << "Pattern term=" << ptm->to_string()
		              << " NOT solved by " << hg.value();})
		solution_pop();
		return false;
	}

	DO_LOG({LAZY_LOG_FINE << "Pattern term=" << ptm->to_string()
	              << " solved by " << hg.value() << ", move up";})

	// XXX should not do perm_push every time... only selectively.
	// But when? This is very confusing ...
	perm_push();
	bool found = do_term_up(ptm, hg, clause_root);
	perm_pop();

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
///  * The parent is a ChoiceLink. In this case, the ChoiceLink
///    itself cannot be directly matched, as is; only its children can
///    be. So in this case, we fetch the ChoiceLink's parent, instead.
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
                                    const Handle& clause_root)
{
	depth = 1;

	// If we are here, then everything below us matches.  If we are
	// at the top of the clause, move on to the next clause. Else,
	// we are working on a term somewhere in the middle of a clause
	// and need to walk upwards.
	const Handle& hp = ptm->getHandle();
	if (clause_compare(ptm, clause_root))
		return clause_accept(clause_root, hg);

	// Move upwards in the term, and hunt for a match, again.
	// There are two ways to move upwards: for a normal term, we just
	// find its parent in the clause. For an evaluatable term, we find
	// the parent evaluatable in the clause, which may be many steps
	// higher.
	DO_LOG({LAZY_LOG_FINE << "Term = " << ptm->to_string() << " of clause UUID = "
	              << clause_root.value() << " has ground, move upwards";})

	if (0 < _pat->in_evaluatable.count(hp))
	{
		// If we are here, there are four possibilities:
		// 1) `hp` is not in any evaluatable that lies between it and
		//    the clause root.  In this case, we need to fall through
		//    to the bottom.
		// 2) The evaluatable is the clause root. We evaluate it, and
		//    consider the clause satisfied if the evaluation returns
		//    true. In that case, we continue to the next clause, else we
		//    backtrack.
		// 3) The evaluatable is in the middle of a clause, in which case,
		//    it's parent must be a logical connective: an AndLink, an
		//    OrLink or a NotLink. In this case, we have to loop over
		//    all of the evaluatables within this clause, and connect
		//    them as appropriate. The structure may be non-trivial, so
		//    that presents a challange.  However, it must be logical
		//    connectives all the way up to the root of the clause, so the
		//    easiest thing to do is simply to start at the top, and
		//    recurse downwards.  Ergo, this is much like case 2): the
		//    evaluation either suceeds or fails; we proceed or backtrack.
		// 4) The evaluatable is in the middle of something else. We don't
		//    know what that means, so we throw an error. Actually, this
		//    is too harsh. It may be in the middle of some function that
		//    expects a boolean value as an argument. But I don't know of
		//    any, just right now.
		//
		// Anyway, all of this talk abbout booleans is emphasizing the
		// point that, someday, we need to replace this crisp logic with
		// probabalistic logic of some sort.
		//
		// By the way, if we are here, then `hp` is surely a variable;
		// or, at least, it is, if we are working in the canonical
		// interpretation.

		auto evra = _pat->in_evaluatable.equal_range(hp);
		for (auto evit = evra.first; evit != evra.second; evit++)
		{
			if (not is_unquoted_in_tree(clause_root, evit->second))
				continue;

			logmsg("Term inside evaluatable, move up to it's top:",
			       evit->second);

			// All of the variables occurring in the term should have
			// grounded by now. If not, then its virtual term, and we
			// shouldn't even be here (we can't just backtrack, and
			// try again later).  So validate the grounding, but leave
			// the evaluation for the callback.
// XXX TODO count the number of ungrounded vars !!! (make sure its zero)

			bool found = _pmc.evaluate_sentence(clause_root, var_grounding);
			DO_LOG({logger().fine("After evaluating clause, found = %d", found);})
			if (found)
				return clause_accept(clause_root, hg);

			return false;
		}
	}

	PatternTermPtr parent = ptm->getParent();
	OC_ASSERT(PatternTerm::UNDEFINED != parent, "Unknown term parent");

	const Handle& hi = parent->getHandle();

	// Do the simple case first, ChoiceLinks are harder.
	bool found = false;
	if (CHOICE_LINK != hi->get_type())
	{
		if (explore_up_branches(parent, hg, clause_root)) found = true;
		DO_LOG({logger().fine("After moving up the clause, found = %d", found);})
	}
	else
	if (hi == clause_root)
	{
		DO_LOG({logger().fine("Exploring ChoiceLink at root");})
		if (clause_accept(clause_root, hg)) found = true;
	}
	else
	{
		// If we are here, we have an embedded ChoiceLink, i.e. a
		// ChoiceLink that is not at the clause root. It's contained
		// in some other link, and we have to get that link and
		// perform comparisons on it. i.e. we have to "hop over"
		// (hop up) past the ChoiceLink, before resuming the search.
		// The easiest way to hop is to do it recursively... i.e.
		// call ourselves again.
		DO_LOG({logger().fine("Exploring ChoiceLink below root");})

		OC_ASSERT(not have_choice(parent, hg),
		          "Something is wrong with the ChoiceLink code");

		_need_choice_push = true;
		if (do_term_up(parent, hg, clause_root)) found = true;
	}

	return found;
}

/// This is called when we've navigated to the top of a clause,
/// and so it is fully grounded, and we're essentially done.
/// However, let the callbacks have the final say on whether to
/// proceed onwards, or to backtrack.
///
bool PatternMatchEngine::clause_accept(const Handle& clause_root,
                                       const Handle& hg)
{
	// Is this clause a required clause? If so, then let the callback
	// make the final decision; if callback rejects, then it's the
	// same as a mismatch; try the next one.
	bool match;
	if (is_optional(clause_root))
	{
		clause_accepted = true;
		match = _pmc.optional_clause_match(clause_root, hg, var_grounding);
		DO_LOG({logger().fine("optional clause match callback match=%d", match);})
	}
	else
	{
		match = _pmc.clause_match(clause_root, hg, var_grounding);
		DO_LOG({logger().fine("clause match callback match=%d", match);})
	}
	if (not match) return false;

	if (not is_evaluatable(clause_root))
	{
		clause_grounding[clause_root] = hg;
		logmsg("---------------------\nclause:", clause_root);
		logmsg("ground:", hg);
	}

	// Now go and do more clauses.
	return do_next_clause();
}

// This is called when all previous clauses have been grounded; so
// we search for the next one, and try to ground that.
bool PatternMatchEngine::do_next_clause(void)
{
	clause_stacks_push();
	get_next_untried_clause();
	Handle joiner = next_joint;
	Handle curr_root = next_clause;

	// If there are no further clauses to solve,
	// we are really done! Report the solution via callback.
	bool found = false;
	if (nullptr == curr_root)
	{
		found = _pmc.grounding(var_grounding, clause_grounding);
		DO_LOG(logger().fine("==================== FINITO! accepted=%d", found);)
		DO_LOG(log_solution(var_grounding, clause_grounding);)
	}
	else
	{
		logmsg("Next clause is", curr_root);
		DO_LOG({LAZY_LOG_FINE << "This clause is "
		              << (is_optional(curr_root)? "optional" : "required");})
		DO_LOG({LAZY_LOG_FINE << "This clause is "
		              << (is_evaluatable(curr_root)?
		                  "dynamically evaluatable" : "non-dynamic");
		logmsg("Joining variable is", joiner);
		logmsg("Joining grounding is", var_grounding[joiner]); })

		// Else, start solving the next unsolved clause. Note: this is
		// a recursive call, and not a loop. Recursion is halted when
		// the next unsolved clause has no grounding.
		//
		// We continue our search at the atom that "joins" (is shared
		// in common) between the previous (solved) clause, and this
		// clause. If the "join" was a variable, look up its grounding;
		// else the join is a 'real' atom.

		clause_accepted = false;
		Handle hgnd(var_grounding[joiner]);
		OC_ASSERT(nullptr != hgnd, "Error: joining handle has not been grounded yet!");
		found = explore_clause(joiner, hgnd, curr_root);

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
		       (is_optional(curr_root)))
		{
			Handle undef(Handle::UNDEFINED);
			bool match = _pmc.optional_clause_match(joiner, undef, var_grounding);
			DO_LOG({logger().fine("Exhausted search for optional clause, cb=%d", match);})
			if (not match) {
				clause_stacks_pop();
				return false;
			}

			// XXX Maybe should push n pop here? No, maybe not ...
			clause_grounding[curr_root] = Handle::UNDEFINED;
			get_next_untried_clause();
			joiner = next_joint;
			curr_root = next_clause;

			DO_LOG({logmsg("Next optional clause is", curr_root);})
			if (nullptr == curr_root)
			{
				DO_LOG({logger().fine("==================== FINITO BANDITO!");
				log_solution(var_grounding, clause_grounding);})
				found = _pmc.grounding(var_grounding, clause_grounding);
			}
			else
			{
				// Now see if this optional clause has any solutions,
				// or not. If it does, we'll recurse. If it does not,
				// we'll loop around back to here again.
				clause_accepted = false;
				Handle hgnd = var_grounding[joiner];
				found = explore_term_branches(joiner, hgnd, curr_root);
			}
		}
	}

	// If we failed to find anything at this level, we need to
	// backtrack, i.e. pop the stack, and begin a search for
	// other possible matches and groundings.
	clause_stacks_pop();

	return found;
}

/**
 * Search for the next untried, (thus ungrounded, unsolved) clause.
 *
 * The "issued" set contains those clauses which are currently in play,
 * i.e. those for which a grounding is currently being explored. Both
 * grounded, and as-yet-ungrounded clauses may be in this set.  The
 * sole reason of this set is to avoid infinite resursion, i.e. of
 * re-identifying the same clause over and over as unsolved.
 *
 * The words "solved" and "grounded" are used as synonyms through out
 * the code.
 *
 * Additional complications are introduced by the presence of
 * evaluatable terms, black-box terms, and optional clauses. An
 * evaluatable term is any term that needs to be evaluated to determine
 * if it matches: such terms typically do not exist in the atomspace;
 * they are "virtual", and "exist" only when the evaluation returns
 * "true". Thus, these can only be grounded after all other possible
 * clauses are grounded; thus these are saved for last.  It is always
 * possible to save these for last, because earlier stages have
 * guaranteed that all of the non-virtual clauses are connected.
 * Anyway, evaluatables come in two forms: those that can be evaluated
 * quickly, and those that require a "black-box" evaluation of some
 * scheme or python code. Of the two, we save "black-box" for last.
 *
 * Then, after grounding all of the mandatory clauses (virtual or not),
 * we look for optional clauses, if any. Again, these might be virtual,
 * and they might be black...
 *
 * Thus, we use a helper function to broaden the search in each case.
 */
void PatternMatchEngine::get_next_untried_clause(void)
{
	// First, try to ground all the mandatory clauses, only.
	// no virtuals, no black boxes, no optionals.
	if (get_next_thinnest_clause(false, false, false)) return;

	// Don't bother looking for evaluatables if they are not there.
	if (not _pat->evaluatable_holders.empty())
	{
		if (get_next_thinnest_clause(true, false, false)) return;
		if (not _pat->black.empty())
		{
			if (get_next_thinnest_clause(true, true, false)) return;
		}
	}

	// If there are no optional clauses, we are done.
	if (_pat->optionals.empty())
	{
		// There are no more ungrounded clauses to consider. We are done.
		next_clause = Handle::UNDEFINED;
		next_joint = Handle::UNDEFINED;
		return;
	}

	// Try again, this time, considering the optional clauses.
	if (get_next_thinnest_clause(false, false, true)) return;
	if (not _pat->evaluatable_holders.empty())
	{
		if (get_next_thinnest_clause(true, false, true)) return;
		if (not _pat->black.empty())
		{
			if (get_next_thinnest_clause(true, true, true)) return;
		}
	}

	// If we are here, there are no more unsolved clauses to consider.
	next_clause = Handle::UNDEFINED;
	next_joint = Handle::UNDEFINED;
}

// Count the number of ungrounded variables in a clause.
//
// This is used to search for the "thinnest" ungrounded clause:
// the one with the fewest ungrounded variables in it. Thus, if
// there is just one variable that needs to be grounded, then this
// can be done in a direct fashion; it resembles the concept of
// "unit propagation" in the DPLL algorithm.
//
// XXX TODO ... Rather than counting the number of variables, we
// should instead look for one with the smallest incoming set.
// That is because the very next thing that we do will be to
// iterate over the incoming set of "pursue" ... so it could be
// a huge pay-off to minimize this.
//
// If there are two ungrounded variables in a clause, then the
// "thickness" is the *product* of the sizes of the two incoming
// sets. Thus, the fewer ungrounded variables, the better.
//
// Danger: this assumes a suitable dataset, as otherwise, the cost
// of this "optimization" can add un-necessarily to the overhead.
//
unsigned int PatternMatchEngine::thickness(const Handle& clause,
                                           const HandleSet& live)
{
	// If there are only zero or one ungrounded vars, then any clause
	// will do. Blow this pop stand.
	if (live.size() < 2) return 1;

	unsigned int count = 0;
	for (const Handle& v : live)
	{
		if (is_unquoted_in_tree(clause, v)) count++;
	}
	return count;
}

/// Same as above, but with three boolean flags:  if not set, then only
/// those clauses satsifying the criterion are considered, else all
/// clauses are considered.
///
/// Return true if we found the next ungrounded clause.
bool PatternMatchEngine::get_next_thinnest_clause(bool search_virtual,
                                                  bool search_black,
                                                  bool search_optionals)
{
	// Search for an as-yet ungrounded clause. Search for required
	// clauses first; then, only if none of those are left, move on
	// to the optional clauses.  We can find ungrounded clauses by
	// looking at the grounded vars, looking up the root, to see if
	// the root is grounded.  If its not, start working on that.
	Handle joint(Handle::UNDEFINED);
	Handle unsolved_clause(Handle::UNDEFINED);
	unsigned int thinnest_joint = UINT_MAX;
	unsigned int thinnest_clause = UINT_MAX;
	bool unsolved = false;

	// Make a list of the as-yet ungrounded variables.
	HandleSet ungrounded_vars;

	// Grounded variables ordered by the size of their grounding incoming set
	std::multimap<std::size_t, Handle> thick_vars;

	for (const Handle &v : _varlist->varset)
	{
		auto gnd = var_grounding.find(v);
		if (gnd != var_grounding.end())
		{
			std::size_t incoming_set_size = gnd->second->getIncomingSetSize();
			thick_vars.insert(std::make_pair(incoming_set_size, v));
		}
		else ungrounded_vars.insert(v);
	}

	// We are looking for a joining atom, one that is shared in common
	// with the a fully grounded clause, and an as-yet ungrounded clause.
	// The joint is called "pursue", and the unsolved clause that it
	// joins will become our next untried clause. We choose joining atom
	// with smallest size of its incoming set. If there are many such
	// atoms we choose one from clauses with minimal number of ungrounded
	// yet variables.
	for (auto tckvar : thick_vars)
	{
		std::size_t pursue_thickness = tckvar.first;
		const Handle& pursue = tckvar.second;

		if (pursue_thickness > thinnest_joint) break;

		auto root_list = _pat->connectivity_map.equal_range(pursue);

		for (auto it = root_list.first; it != root_list.second; it++)
		{
			const Handle& root = it->second;
			if ((issued.end() == issued.find(root))
			        and (search_virtual or not is_evaluatable(root))
			        and (search_black or not is_black(root))
			        and (search_optionals or not is_optional(root)))
			{
				unsigned int root_thickness = thickness(root, ungrounded_vars);
				if (root_thickness < thinnest_clause)
				{
					thinnest_clause = root_thickness;
					thinnest_joint = pursue_thickness;
					unsolved_clause = root;
					joint = pursue;
					unsolved = true;
				}
			}
		}
	}

	if (unsolved)
	{
		// Joint is a (variable) node that's shared between several
		// clauses. One of the clauses has been grounded, another
		// has not.  We want to now traverse upwards from this node,
		// to find the top of the ungrounded clause.
		next_clause = unsolved_clause;
		next_joint = joint;

		if (unsolved_clause)
		{
			issued.insert(unsolved_clause);
			return true;
		}
	}

	return false;
}

/* ======================================================== */
/**
 * Push all stacks related to the grounding of a clause. This push is
 * meant to be done only when a grounding for a clause has been found,
 * and the next clause is about the be attempted. It saves all of the
 * traversal data associated with the current clause, so that, later
 * on, traversal can be resumed where it was left off.
 *
 * This does NOT push and of the redex stacks because (with the current
 * redex design), all redex substitutions should have terminatated by
 * now, and returned to the main clause. i.e. the redex stack is assumed
 * to be empty, at this point.  (Its possible this design may change in
 * in the future if multi-clause redexes are allowed, whatever the heck
 * that may be!?)
 */
void PatternMatchEngine::clause_stacks_push(void)
{
	_clause_stack_depth++;
	DO_LOG({logger().fine("--- That's it, now push to stack depth=%d",
	              _clause_stack_depth);})

	var_solutn_stack.push(var_grounding);
	term_solutn_stack.push(clause_grounding);

	issued_stack.push(issued);
	choice_stack.push(_choice_state);

	perm_push();

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
	POPSTK(term_solutn_stack, clause_grounding);
	POPSTK(var_solutn_stack, var_grounding);
	POPSTK(issued_stack, issued);

	POPSTK(choice_stack, _choice_state);

	perm_pop();

	_clause_stack_depth --;

	DO_LOG({logger().fine("pop to depth %d", _clause_stack_depth);})
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
	OC_ASSERT(0 == term_solutn_stack.size());
	OC_ASSERT(0 == var_solutn_stack.size());
	OC_ASSERT(0 == issued_stack.size());
	OC_ASSERT(0 == choice_stack.size());
	OC_ASSERT(0 == perm_stack.size());
#else
	while (!term_solutn_stack.empty()) term_solutn_stack.pop();
	while (!var_solutn_stack.empty()) var_solutn_stack.pop();
	while (!issued_stack.empty()) issued_stack.pop();
	while (!choice_stack.empty()) choice_stack.pop();
	while (!perm_stack.empty()) perm_stack.pop();
#endif
}

void PatternMatchEngine::solution_push(void)
{
	var_solutn_stack.push(var_grounding);
	term_solutn_stack.push(clause_grounding);
}

void PatternMatchEngine::solution_pop(void)
{
	POPSTK(var_solutn_stack, var_grounding);
	POPSTK(term_solutn_stack, clause_grounding);
}

void PatternMatchEngine::solution_drop(void)
{
	var_solutn_stack.pop();
	term_solutn_stack.pop();
}

/* ======================================================== */

/**
 * explore_neighborhood - explore the local (connected) neighborhood
 * of the starter clause, looking for a match.  The idea here is that
 * it is much easier to traverse a connected graph looking for the
 * appropriate subgraph (pattern) than it is to try to explore the
 * whole atomspace, at random.  The user callback `initiate_search()`
 * should call this method, suggesting a clause to start with, and
 * where in the clause the search should begin.
 *
 * Inputs:
 * do_clause: must be one of the clauses previously specified in the
 *            clause list of the match() method.
 * term:      must be a sub-clause of do_clause; that is, must be a link
 *            that appears in do_clause. Must contain `grnd` below.
 * grnd:      must be a (non-variable) node in the `term` term.
 *            That is, this must be one of the outgoing atoms of the
 *            `term` link; it must be a node, and it must not be
 *            a variable node or glob node.
 *
 * Returns true if one (or more) matches are found
 *
 * This routine is meant to be invoked on every candidate atom taken
 * from the atom space. That atom is assumed to anchor some part of
 * a graph that hopefully will match the pattern.
 */
bool PatternMatchEngine::explore_neighborhood(const Handle& do_clause,
                                              const Handle& term,
                                              const Handle& grnd)
{
	clause_stacks_clear();
	return explore_redex(term, grnd, do_clause);
}

/**
 * Same as above, obviously; we just pick up the graph context
 * where we last left it.
 */
bool PatternMatchEngine::explore_redex(const Handle& term,
                                       const Handle& grnd,
                                       const Handle& first_clause)
{
	// Cleanup
	clear_current_state();

	// Match the required clauses.
	issued.insert(first_clause);
	return explore_clause(term, grnd, first_clause);
}

/**
 * Every clause in a pattern is one of two types:  it either
 * specifies a pattern to be matched, or it specifies an evaluatable
 * atom that must be evaluated to determine if it is to be accepted.
 * (In the default callback, evaluatable atoms are always crisp
 * boolean-logic formulas, although the infrastructure is designed
 * to handle other situations as well, e.g. Bayesian formulas, etc.)
 *
 * This method simply dispatches a given clause to be either pattern
 * matched, or to be evaluated.
 */
bool PatternMatchEngine::explore_clause(const Handle& term,
                                        const Handle& grnd,
                                        const Handle& clause)
{
	// If we are looking for a pattern to match, then ... look for it.
	// Evaluatable clauses are not patterns; they are clauses that
	// evaluate to true or false.
	if (not is_evaluatable(clause))
	{
		// Check if the pattern has globs in it, and record the glob_state.
		// Do this *before* exploring the term.
		bool has_glob = (0 < _pat->globby_holders.count(term));
		size_t gstate_size = _glob_state.size();

		DO_LOG({logger().fine("Clause is matchable; start matching it");})
		bool found = explore_term_branches(term, grnd, clause);

		// If there may be another way to ground it differently to the same
		// candidate, do it until exhausted.
		while (not found and has_glob and _glob_state.size() > gstate_size)
		{
			found = explore_term_branches(term, grnd, clause);
		}

		// If found is false, then there's no solution here.
		// Bail out, return false to try again with the next candidate.
		return found;
	}

	// If we are here, we have an evaluatable clause on our hands.
	DO_LOG({logger().fine("Clause is evaluatable; start evaluating it");})

	// Some people like to have a clause that is just one big
	// giant variable, matching almost anything. Keep these folks
	// happy, and record the suggested grounding. There's nowhere
	// else to do this, so we do it here.
	if (term->get_type() == VARIABLE_NODE)
		var_grounding[term] = grnd;

	bool found = _pmc.evaluate_sentence(clause, var_grounding);
	DO_LOG({logger().fine("Post evaluating clause, found = %d", found);})
	if (found)
		return clause_accept(clause, grnd);

	return false;
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

	// ChoiceLink state
	_choice_state.clear();
	_need_choice_push = false;
	choose_next = true;

	// UnorderedLink state
	have_more = false;
	take_step = true;
	_perm_state.clear();

	// GlobNode state
	_glob_state.clear();

	issued.clear();
}

bool PatternMatchEngine::explore_constant_evaluatables(const HandleSeq& clauses)
{
	bool found = true;
	for (const Handle& clause : clauses) {
		if (is_in(clause, _pat->evaluatable_holders)) {
			found = _pmc.evaluate_sentence(clause, HandleMap());
			if (not found)
				break;
		}
	}
	if (found)
		_pmc.grounding(HandleMap(), HandleMap());

	return found;
}

PatternMatchEngine::PatternMatchEngine(PatternMatchCallback& pmcb)
	: _pmc(pmcb),
	_nameserver(nameserver()),
	_varlist(nullptr),
	_pat(nullptr),
	clause_accepted(false)
{
	// current state
	depth = 0;

	// graph state
	_clause_stack_depth = 0;

	// choice link state
	_need_choice_push = false;
	choose_next = true;

	// unordered link state
	have_more = false;
	take_step = true;
}

void PatternMatchEngine::set_pattern(const Variables& v,
                                     const Pattern& p)
{
	_varlist = &v;
	_pat = &p;
}

/* ======================================================== */

#ifdef DEBUG
void PatternMatchEngine::log_solution(
	const HandleMap &vars,
	const HandleMap &clauses)
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
		Handle var(j.first);
		Handle soln(j.second);

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
	HandleMap::const_iterator m;
	int i = 0;
	for (m = clauses.begin(); m != clauses.end(); ++m, ++i)
	{
		if (not m->second)
		{
			Handle mf(m->first);
			logmsg("ERROR: ungrounded clause", mf);
			continue;
		}
		std::string str = m->second->to_short_string();
		logger().fine("%d.   %s", i, str.c_str());
	}
}

/**
 * For debug logging only
 */
void PatternMatchEngine::log_term(const HandleSet &vars,
                                  const HandleSeq &clauses)
{
	logger().fine("Clauses:");
	for (Handle h : clauses) log(h);

	logger().fine("Vars:");
	for (Handle h : vars) log(h);
}
#else

void PatternMatchEngine::log_solution(const HandleMap &vars,
                                      const HandleMap &clauses) {}

void PatternMatchEngine::log_term(const HandleSet &vars,
                                  const HandleSeq &clauses) {}
#endif

/* ===================== END OF FILE ===================== */
