/*
 * Implicator.cc
 *
 * Copyright (C) 2009, 2014 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
#include <opencog/atoms/pattern/BindLink.h>

#include "BindLinkAPI.h"
#include "DefaultImplicator.h"
#include "PatternMatch.h"

using namespace opencog;

/**
 * This callback takes the reported grounding, runs it through the
 * instantiator, to create the implicand, and then records the result
 * in the `result_set`. Repeated solutions are skipped. If the number
 * of unique results so far is less than `max_results`, it then returns
 * false, to search for more groundings.  (The engine will halt its
 * search for a grounding once an acceptable one has been found; so,
 * to continue hunting for more, we return `false` here. We want to
 * find all possible groundings.)
 */
bool Implicator::grounding(const HandleMap &var_soln,
                           const HandleMap &term_soln)
{
	// PatternMatchEngine::print_solution(term_soln,var_soln);

	// Do not accept new solution if maximum number has been already reached
	if (_result_set.size() >= max_results)
		return true;

	// Ignore the case where the URE creates ill-formed links (due to
	// rules producing nothing). Ideally this should be treated as a
	// user error, that is the user should design rule pre-conditions
	// to prevent them from producing nothing.  In practice it is
	// difficult to insure so meanwhile this try-catch is used. See
	// issue #950 and pull req #962. XXX FIXME later.
	try {
		Handle h(HandleCast(inst.instantiate(implicand, var_soln, true)));
		insert_result(h);
	} catch (const SilentException& ex) {}

	// If we found as many as we want, then stop looking for more.
	return (_result_set.size() >= max_results);
}

void Implicator::insert_result(const Handle& h)
{
	if (h and _result_set.end() == _result_set.find(h))
	{
		_result_set.insert(h);
		_result_list.push_back(h);
	}
}

namespace opencog
{

/**
 * Simplified utility
 *
 * The `do_conn_check` flag stands for "do connectivity check"; if the
 * flag is set, and the pattern is disconnected, then an error will be
 * thrown. The URE explicitly allows disconnected graphs.
 *
 * Set the default to always allow disconnected graphs. This will
 * get naive users into trouble, but there are legit uses, not just
 * in the URE, for doing disconnected searches.
 */
Handle do_imply(AtomSpace* as,
                const Handle& hbindlink,
                Implicator& impl,
                bool do_conn_check=false)
{
	BindLinkPtr bl(BindLinkCast(hbindlink));

	impl.implicand = bl->get_implicand();

	bl->imply(impl, as, do_conn_check);

	// If we got a non-empty answer, just return it.
	if (0 < impl.get_result_list().size())
	{
		// The result_list contains a list of the grounded expressions.
		// (The order of the list has no significance, so it's really a set.)
		// Put the set into a SetLink, cache it, and return that.
		Handle rewr(createLink(impl.get_result_list(), SET_LINK));

#define PLACE_RESULTS_IN_ATOMSPACE
#ifdef PLACE_RESULTS_IN_ATOMSPACE
		// Shoot. XXX FIXME. Most of the unit tests require that the atom
		// that we return is in the atomspace. But it would be nice if we
		// could defer this indefinitely, until its really needed.
		rewr = as->add_atom(rewr);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */

		bl->set_rewrite(rewr);
		return rewr;
	}

	// If we are here, then there were zero matches.
	//
	// There are certain useful queries, where the goal of the query
	// is to determine that some clause or set of clauses are absent
	// from the AtomSpace. If the clauses are jointly not found, after
	// a full and exhaustive search, then we want to run the implicator,
	// and perform some action. Easier said than done, this code is
	// currently a bit of a hack. It seems to work, per the AbsentUTest
	// but is perhaps a bit fragile in its assumptions.
	//
	// Theoretical background: the atomspace can be thought of as a
	// Kripke frame: it holds everything we know "right now". The
	// AbsentLink is a check for what we don't know, right now.
	const Pattern& pat = bl->get_pattern();
	DefaultPatternMatchCB* intu =
		dynamic_cast<DefaultPatternMatchCB*>(&impl);
	if (0 == pat.mandatory.size() and 0 < pat.optionals.size()
	    and not intu->optionals_present())
	{
		Handle h(HandleCast(impl.inst.execute(impl.implicand, true)));
		impl.insert_result(h);
	}

	// Create a set holding all results of the implication, and cache it.
	Handle rewr(createLink(impl.get_result_list(), SET_LINK));

#ifdef PLACE_RESULTS_IN_ATOMSPACE
	// Shoot. XXX FIXME. Most of the unit tests require that the atom
	// that we return is in the atomspace. But it would be nice if we
	// could defer this indefinitely, until its really needed.
	rewr = as->add_atom(rewr);
#endif /* PLACE_RESULTS_IN_ATOMSPACE */
	bl->set_rewrite(rewr);

	return rewr;
}

/**
 * Evaluate a pattern and rewrite rule embedded in a BindLink
 *
 * Use the default implicator to find pattern-matches. Associated truth
 * values are completely ignored during pattern matching; if a set of
 * atoms that could be a ground are found in the atomspace, then they
 * will be reported.
 *
 * See the do_imply function documentation for details.
 */
Handle bindlink(AtomSpace* as, const Handle& hbindlink, size_t max_results)
{
#ifdef CACHED_IMPLICATOR
	CachedDefaultImplicator cachedImpl(as);
	Implicator& impl = cachedImpl;
#else
	DefaultImplicator impl(as);
#endif
	impl.max_results = max_results;
	// Now perform the search.
	return do_imply(as, hbindlink, impl);
}

}

/* ===================== END OF FILE ===================== */
