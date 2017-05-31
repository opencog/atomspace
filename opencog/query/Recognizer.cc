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
	if (npat_h == nsoln_h) return true;
	Type tso = nsoln_h->getType();
	if (VARIABLE_NODE == tso) return true;
	if (GLOB_NODE == tso)
	{
		if (0 < _soln_vars.size())
		{
			// Check if it satisfies the type restrictions, if any
			if (std::all_of(_soln_vars.begin(), _soln_vars.end(),
				[&](const Variables& v) {
					return (not v.is_type(nsoln_h, npat_h)); }))
				return false;

			// Check if it satisfies the interval restrictions, if any
			if (std::all_of(_soln_vars.begin(), _soln_vars.end(),
				[&](const Variables& v) {
					return (not v.is_interval(nsoln_h, 1)); }))
				return false;
		}
		// No reason to reject
		return true;
	}
	return false;
}

bool Recognizer::link_match(const PatternTermPtr& ptm, const Handle& lsoln)
{
	_soln_vars.clear();
	const Handle& lpat = ptm->getHandle();

	// Self-compares always proceed.
	if (lpat == lsoln) return true;

	// mis-matched types are a dead-end.
	if (lpat->getType() != lsoln->getType()) return false;

	// Find the type and interval restrictions under a ScopeLink, if any
	if (contains_atomtype(lsoln, GLOB_NODE))
		get_glob_decl(lsoln);

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

	// Do a side-by-side compare. This is not as rigorous as
	// PatternMatchEngine::tree_compare() nor does it handle the bells
	// and whistles (ChoiceLink, QuoteLink, etc).
	size_t ip=0, jg=0;
	for (; ip<osp_size and jg<osg_size; ip++, jg++)
	{
		if (GLOB_NODE != osg[jg]->getType())
		{
			if (loose_match(osp[ip], osg[jg])) continue;
			return false;
		}

		// If we are here, we have a glob in the soln. If the glob is at
		// the end, it eats everything, so its a match. Else, resume
		// matching at the end of the glob.
		if ((jg+1) == osg_size) return true;

		const Handle& post(osg[jg+1]);
		ip++;
		while (ip < max_size and not loose_match(osp[ip], post))
		{
			ip++;
		}
		// If ip ran past the end, then the post was not found. This is
		// a mismatch.
		if (not (ip < max_size)) return false;

		// Go around again, look for more GlobNodes. Back up by one, so
		// that the for-loop increment gets us back on track.
		ip--;
	}

	// If we are here, then we should have matched up all the atoms;
	// if we exited the loop because pattern or grounding was short,
	// then its a mis-match.
	if (ip != osp_size or jg != osg_size) return false;
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
