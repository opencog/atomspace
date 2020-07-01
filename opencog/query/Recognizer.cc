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

#include <opencog/util/oc_assert.h>

#include <opencog/atoms/core/FindUtils.h>
#include <opencog/query/PatternMatchEngine.h>
#include "Recognizer.h"

using namespace opencog;

// Uncomment below to enable debug print
// #define QDEBUG 1
#ifdef QDEBUG
#define dbgprt(f, varargs...) logger().fine(f, ##varargs)
#else
#define dbgprt(f, varargs...)
#endif

/* ======================================================== */

bool Recognizer::do_search(PatternMatchCallback& pmc, const Handle& top)
{
	if (top->is_link())
	{
		// Recursively drill down and explore every possible node as
		// a search starting point. This is needed, as the patterns we
		// compare against might not be connected.
		for (const Handle& h : top->getOutgoingSet())
		{
			_starter_term = top;
			bool found = do_search(pmc, h);
			if (found) return true;
		}
		return false;
	}

	PatternMatchEngine pme(pmc);
	pme.set_pattern(*_vars, *_pattern);

	// IncomingSet iset = get_incoming_set(top);
	IncomingSet iset = top->getIncomingSet(_as);
	size_t sz = iset.size();
	for (size_t i = 0; i < sz; i++)
	{
		Handle h(iset[i]);
		dbgprt("rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr\n");
		dbgprt("Loop candidate (%lu - %s):\n%s\n", _cnt++,
		       top->to_short_string().c_str(),
		       h->to_short_string().c_str());
		bool found = pme.explore_neighborhood(_starter_term, h, _root);

		// Terminate search if satisfied.
		if (found) return true;
   }

	return false;
}

bool Recognizer::perform_search(PatternMatchCallback& pmc)
{
	const PatternTermSeq& clauses = _pattern->pmandatory;

	_cnt = 0;
	for (const PatternTermPtr& ptm: clauses)
	{
		_root = ptm;
		bool found = do_search(pmc, ptm->getHandle());
		if (found) return true;
	}
	return false;
}

bool Recognizer::get_next_clause(const GroundingMap& var_grounding,
                                 PatternTermPtr& clause, Handle& joint)
{
	return false;
}

bool Recognizer::node_match(const Handle& npat_h, const Handle& nsoln_h)
{
	// If it's a match, we can accept it right away, the comparison
	// is done already, see link_match below.
	if (match) return true;

	if (npat_h == nsoln_h) return true;
	if (VARIABLE_NODE == nsoln_h->get_type()) return true;
	return false;
}

bool Recognizer::link_match(const PatternTermPtr& ptm, const Handle& lsoln)
{
	match = false;
	const Handle& lpat = ptm->getHandle();

	// Self-compares always proceed.
	if (lpat == lsoln) return true;

	// mis-matched types are a dead-end.
	if (lpat->get_type() != lsoln->get_type()) return false;

	// TODO: Change to something better if possible...
	// What is happening here is to manually call the
	// fuzzy_match callback immediately if and only if
	// lsoln has one or more GlobNodes AND lpat and lsoln
	// have the same arity.
	// The reason is, if the pat and soln are having the
	// size, pattern matcher will then do a side-by-side
	// comparison on their outgoing atoms.
	// In the typical use cases we have at the moment,
	// the comparison will be done in the node_match
	// callback. However that will cause problems in some
	// situations, for example if we have:
	// === lpat ===      === lsoln ===
	// Concept "A"       Glob $x
	// Concept "B"       Concept "A"
	// Concept "C"       Glob $y
	// and both of the globs $x and $y have an interval
	// restriction of zero to infinity, it should be a
	// match by grounding $x to nothing and $y to Concept
	// "B" and "C". But a side-by-side comparison here
	// only compares their nodes at the same position
	// (i.e. A-$x, B-A, C-$y), and decide whether to
	// reject it when there is a mis-match. As a result
	// a lot of candidates that we are expecting are
	// rejected...
	// And the reason of going to fuzzy_match is that
	// all the glob-matching logic is there, so it
	// should be able to handle this better.
	if (contains_atomtype(lsoln, GLOB_NODE) and
	    lpat->get_arity() == lsoln->get_arity())
	{
		if (fuzzy_match(lpat, lsoln))
		{
			match = true;
			return true;
		}
		else return false;
	}

	return true;
}

bool Recognizer::loose_match(const Handle& npat_h, const Handle& nsoln_h)
{
	Type gtype = nsoln_h->get_type();
	// Variable matches anything; move to next.
	if (VARIABLE_NODE == gtype) return true;

	// Strict match for link types.
	if (npat_h->get_type() != gtype) return false;
	if (not npat_h->is_node()) return true;

	// If we are here, we know we have nodes. Ask for a strict match.
	if (npat_h != nsoln_h) return false;
	return true;
}

bool Recognizer::fuzzy_match(const Handle& npat_h, const Handle& nsoln_h)
{
	// If we are here, then there are probably glob nodes in the soln.
	// Try to match them, fairly rigorously. Exactly what constitutes
	// an OK match is still a bit up in the air.

	if (not npat_h->is_link() or not nsoln_h->is_link()) return false;

	const HandleSeq &osg = nsoln_h->getOutgoingSet();
	size_t osg_size = osg.size();

	// Lets not waste time, if there's no glob there.
	bool have_glob = false;
	for (size_t j=0; j<osg_size; j++)
	{
		if (osg[j]->get_type() == GLOB_NODE)
		{
			have_glob = true;
			break;
		}
	}
	if (not have_glob) return false;

	const HandleSeq &osp = npat_h->getOutgoingSet();
	size_t osp_size = osp.size();

	// Do a side-by-side compare. This is not as rigorous as
	// PatternMatchEngine::tree_compare() nor does it handle the bells
	// and whistles (ChoiceLink, QuoteLink, etc).
	size_t ip=0, jg=0;
	for (; ip<osp_size or jg<osg_size; ip++, jg++)
	{
		if (ip == osp_size) ip--;
		if (jg == osg_size) jg--;

		if (GLOB_NODE != osg[jg]->get_type())
		{
			if (loose_match(osp[ip], osg[jg])) continue;
			return false;
		}

		// If we are here, we have a glob in the soln. If the glob is at
		// the end, it eats everything, so its a match. Else, resume
		// matching at the end of the glob.
		if ((jg+1) == osg_size) return true;

		const Handle& post(osg[jg+1]);

		// If the post is also a GlobNode, we are done for this one.
		if (GLOB_NODE == post->get_type()) return true;

		// Match as many as possible.
		while (ip < osp_size and not loose_match(osp[ip], post))
		{
			ip++;
		}

		// Go around again, look for more GlobNodes. Back up by one, so
		// that the for-loop increment gets us back on track.
		ip--;
	}

	// If we are here, then we should have matched up all the atoms.
	return true;
}

bool Recognizer::grounding(const GroundingMap& var_soln,
                           const GroundingMap& term_soln)
{
	const Handle& hroot = _root->getHandle();
	const Handle& rule = term_soln.at(hroot);

	if (rule != hroot) {
		LOCK_PE_MUTEX;
		_rules.insert(rule);
	}

	// Look for more groundings.
	return false;
}
