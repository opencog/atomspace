/*
 * Recognizer.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_RECOGNIZER_H
#define _OPENCOG_RECOGNIZER_H

#include <set>

#include <opencog/query/DefaultPatternMatchCB.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atomutils/FindUtils.h>

#include "BindLinkAPI.h"

namespace opencog {

/**
 * Very rough, crude, experimental prototype for the idea that
 * pattern recognition is the dual of pattern matching.
 * That is, rather than using one query pattern to search a large
 * collection of data, this does the opposite: given a single piece of
 * data, it searches for all rules/queries which apply to it.
 *
 * The file /examples/aiml/recog.scm provides a very simple example.
 * The implementation here is very minimalistic, and does many things
 * wrong:
 * -- it fails to perform any type-checking to make sure the variable
 *    constraints are satisfied.
 * -- AIML wants a left-to-right traversal, this does an omni-
 *    directional exploration. (which is OK, but is not how AIML is
 *    defined...)
 * -- This hasn't been thought through thoroughly. There are almost
 *    surely some weird gotcha's.
 */
class Recognizer :
   public virtual DefaultPatternMatchCB
{
	private:
		bool bypass_node_match = false;
		std::vector<Variables> _soln_vars;
		void get_glob_decl(const Handle&);

	protected:
		const Pattern* _pattern;

		Handle _root;
		Handle _starter_term;
		size_t _cnt;
		bool do_search(PatternMatchEngine*, const Handle&);
		bool loose_match(const Handle&, const Handle&);

	public:
		OrderedHandleSet _rules;

		Recognizer(AtomSpace* as) :
			DefaultPatternMatchCB(as) {}

		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat)
		{
			_pattern = &pat;
			DefaultPatternMatchCB::set_pattern(vars, pat);
		}

		virtual bool initiate_search(PatternMatchEngine*);
		virtual bool node_match(const Handle&, const Handle&);
		virtual bool link_match(const PatternTermPtr&, const Handle&);
		virtual bool fuzzy_match(const Handle&, const Handle&);
		virtual bool grounding(const HandleMap &var_soln,
		                       const HandleMap &term_soln);
};

} // namespace opencog

#endif // _OPENCOG_RECOGNIZER_H

using namespace opencog;

// Uncomment below to enable debug print
#define DEBUG 1
#ifdef DEBUG
#define dbgprt(f, varargs...) logger().fine(f, ##varargs)
#else
#define dbgprt(f, varargs...)
#endif

/* ======================================================== */

void Recognizer::get_glob_decl(const Handle& h)
{
	IncomingSet scop = h->getIncomingSetByType(SCOPE_LINK, true);

	if (0 < scop.size())
	{
		for (const LinkPtr& lp : scop)
		{
			Handle s(lp);
			ScopeLinkPtr sl(ScopeLinkCast(s));
			if (NULL == sl)
				sl = createScopeLink(*LinkCast(s));
			_soln_vars.push_back(sl->get_variables());
		}
	}
	else
	{
		IncomingSet iset = h->getIncomingSet();
		if (iset.size() == 0) return;
		for (const LinkPtr& lp : iset)
			get_glob_decl(lp->getHandle());
	}
}

bool Recognizer::do_search(PatternMatchEngine* pme, const Handle& top)
{
	if (top->isLink())
	{
		// Recursively drill down and explore every possible node as
		// a search starting point. This is needed, as the patterns we
		// compare against might not be connected.
		for (const Handle& h : top->getOutgoingSet())
		{
			_starter_term = top;
			bool found = do_search(pme, h);
			if (found) return true;
		}
		return false;
	}

	IncomingSet iset = get_incoming_set(top);
	size_t sz = iset.size();
	for (size_t i = 0; i < sz; i++)
	{
		Handle h(iset[i]);
		dbgprt("rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr\n");
		dbgprt("Loop candidate (%lu - %s):\n%s\n", _cnt++,
		       top->toShortString().c_str(),
		       h->toShortString().c_str());
		bool found = pme->explore_neighborhood(_root, _starter_term, h);

		// Terminate search if satisfied.
		if (found) return true;
   }

	return false;
}

bool Recognizer::initiate_search(PatternMatchEngine* pme)
{
	const HandleSeq& clauses = _pattern->cnf_clauses;

	_cnt = 0;
	for (const Handle& h: clauses)
	{
		_root = h;
		bool found = do_search(pme, h);
		if (found) return true;
	}
	return false;
}

bool Recognizer::node_match(const Handle& npat_h, const Handle& nsoln_h)
{
	// Note: It is accepted right away as a side-by-side comparsion
	// has already been done in fuzzy_match, see link_match below.
	// So if we reach here, that means it passed already so don't
	// worry about it.
	if (bypass_node_match) return true;

	if (npat_h == nsoln_h) return true;
	if (VARIABLE_NODE == nsoln_h->getType()) return true;

	return false;
}

bool Recognizer::link_match(const PatternTermPtr& ptm, const Handle& lsoln)
{
	_soln_vars.clear();
	bypass_node_match = false;
	const Handle& lpat = ptm->getHandle();

	// Self-compares always proceed.
	if (lpat == lsoln) return true;

	// mis-matched types are a dead-end.
	if (lpat->getType() != lsoln->getType()) return false;

	// Find the type and interval restrictions under a ScopeLink, if any
	if (contains_atomtype(lsoln, GLOB_NODE))
	{
		get_glob_decl(lsoln);

		// TODO: Change to something better if possible...
		// What is happening here is to manually call the
		// fuzzy_match callback immediately if and only if
		// lsoln has one or more GlobNodes AND lpat and lsoln
		// have the same arity.
		// The reason is, if the pat and soln are having the
		// same arity, pattern matcher will then do a
		// side-by-side comparsion of their outgoing atoms.
		// In the typical use cases we are facing at the
		// moment, the comparsion will be done in the node_match
		// callback. However that will cause problems in some
		// situations, for example if we have:
		// === lpat ===      === lsoln ===
		// Concept "A"       Glob $x
		// Concept "B"       Concept "A"
		// Concept "C"       Glob $y
		// and both of the globs $x and $y have an interval
		// restriction of zero to infinity, it should be a
		// match by grounding $x to nothing and $y to Concept
		// "B" and "C". But a side-by-side comparsion here is
		// a node-level comparsion (i.e. A-$x, B-A, C-$y),
		// it makes it really hard to decide whether or not
		// to ground a particular glob without knowing the
		// rest of the sibling. And the reasons of calling the
		// fuzzy_match callback are that it itself is a link-
		// level comparsion and the glob-matching logic happens
		// to be all there...
		if (lpat->getArity() == lsoln->getArity())
		{
			if (fuzzy_match(lpat, lsoln))
			{
				bypass_node_match = true;
				return true;
			}
			else return false;
		}
	}

	return true;
}

bool Recognizer::loose_match(const Handle& npat_h, const Handle& nsoln_h)
{
	Type gtype = nsoln_h->getType();
	// Variable matches anything; move to next.
	if (VARIABLE_NODE == gtype) return true;

	// Strict match for link types.
	if (npat_h->getType() != gtype) return false;
	if (not npat_h->isNode()) return true;

	// If we are here, we know we have nodes. Ask for a strict match.
	if (npat_h != nsoln_h) return false;
	return true;
}

bool Recognizer::fuzzy_match(const Handle& npat_h, const Handle& nsoln_h)
{
	// If we are here, then there are probably glob nodes in the soln.
	// Try to match them, fairly rigorously. Exactly what constitutes
	// an OK match is still a bit up in the air.

	if (not npat_h->isLink() or not nsoln_h->isLink()) return false;

	const HandleSeq &osg = nsoln_h->getOutgoingSet();
	size_t osg_size = osg.size();

	// Lets not waste time, if there's no glob there.
	bool have_glob = false;
	for (size_t j=0; j<osg_size; j++)
	{
		if (osg[j]->getType() == GLOB_NODE)
		{
			have_glob = true;
			break;
		}
	}
	if (not have_glob) return false;

	const HandleSeq &osp = npat_h->getOutgoingSet();
	size_t osp_size = osp.size();
	size_t max_size = std::max(osg_size, osp_size);

	size_t ks = 0;
	bool is_scoped = (0 < _soln_vars.size());
	Variables svar;
	if (is_scoped) svar = _soln_vars[ks];

	// Do a side-by-side compare. This is not as rigorous as
	// PatternMatchEngine::tree_compare() nor does it handle the bells
	// and whistles (ChoiceLink, QuoteLink, etc).
	size_t ip=0, jg=0;
	for (; ip<osp_size or jg<osg_size; ip++, jg++)
	{
		bool pat_end = false;
		if (ip == osp_size)
		{
			pat_end = true;
			ip--;
		}
		if (jg == osg_size) jg--;

		if (GLOB_NODE != osg[jg]->getType())
		{
			if (loose_match(osp[ip], osg[jg])) continue;

			// Sometimes we end up here because the glob
			// in the previous iteration failed to
			// match anything, so although we are not
			// looking at a glob right now, we still
			// need to do the below to see if it can be
			// rejected yet.

			if (not is_scoped)
				return false;
			// If we have gone through all the scopes,
			// it's not a match and we are done.
			else if (ks+1 == _soln_vars.size())
				return false;
			// Otherwise reset everything and try again
			// with the next scope.
			else
			{
				// The for-loop increment will turn them back to zero.
				ip = -1;
				jg = -1;
				ks++;
				svar = _soln_vars[ks];
				continue;
			}
		}

		// If we are here, we have a glob in the soln.
		const Handle& glob = osg[jg];
		size_t match_cnt = 0;

		// If the glob is at the end, see if it can eat everything.
		if (jg+1 == osg_size)
		{
			// If the glob is not scoped, it eats everything, so its a match.
			if (not is_scoped) return true;

			while (ip < osp_size)
			{
				// Make sure it satisfies both type and interval restrictions.
				if (svar.is_type(glob, osp[ip]) and
				    svar.is_interval(glob, match_cnt+1))
				{
					match_cnt++;
					ip++;
				}
				else break;
			}

			// Return true if it managed to eat everything.
			if (ip == osp_size) return true;
			// If this ending glob can be grounded to nothing, it's a match!
			else if (svar.is_interval(glob, 0) and pat_end)
				return true;
			else
			{
				if (not is_scoped)
					return false;
				// If we have gone through all the scopes,
				// it's not a match and we are done.
				else if (ks+1 == _soln_vars.size())
					return false;
				// Otherwise reset everything and try again
				// with the next scope.
				else
				{
					// The for-loop increment will turn them back to zero.
					ip = -1;
					jg = -1;
					ks++;
					svar = _soln_vars[ks];
					continue;
				}
			}
		}

		const Handle& post(osg[jg+1]);
		while (ip < max_size and not loose_match(osp[ip], post))
		{
			// If it's scoped and either the type or interval
			// restirctions is not satisfied, break
			if (is_scoped and
			   ((not svar.is_type(glob, osp[ip])) or
			    (not svar.is_interval(glob, match_cnt+1))))
				break;

			match_cnt++;
			ip++;
		}
		// If ip ran past the end, then the post was not found. This is
		// a mismatch.
		if (not (ip < max_size))
		{
			if (not is_scoped)
				return false;
			// If we have gone through all the scopes,
			// it's not a match and we are done.
			else if (ks+1 == _soln_vars.size())
				return false;
			// Otherwise reset everything and try again
			// with the next scope.
			else
			{
				// The for-loop increment will turn them back to zero.
				ip = -1;
				jg = -1;
				ks++;
				svar = _soln_vars[ks];
				continue;
			}
		}

		// Go around again, look for more GlobNodes. Back up by one, so
		// that the for-loop increment gets us back on track.
		ip--;
	}

	// If we are here, then we should have matched up all the atoms;
	// any mismatch should have rejected already.
	return true;
}

bool Recognizer::grounding(const HandleMap& var_soln,
                           const HandleMap& term_soln)
{
	Handle rule = term_soln.at(_root);

	if (rule != _root) {
		_rules.insert(rule);
	}

	// Look for more groundings.
	return false;
}

Handle opencog::recognize(AtomSpace* as, const Handle& hlink)
{
	PatternLinkPtr bl(PatternLinkCast(hlink));
	if (NULL == bl)
		bl = createPatternLink(*LinkCast(hlink));

	Recognizer reco(as);
	bl->satisfy(reco);

	HandleSeq hs;
	for (const Handle& h : reco._rules) hs.push_back(h);

	return as->add_link(SET_LINK, hs);
}
