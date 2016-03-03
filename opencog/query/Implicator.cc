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
#include <opencog/truthvalue/SimpleTruthValue.h>
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
bool Implicator::grounding(const std::map<Handle, Handle> &var_soln,
                           const std::map<Handle, Handle> &term_soln)
{
	// PatternMatchEngine::print_solution(term_soln,var_soln);

	// Do not accept new solution if maximum number has been already reached
	if (_result_set.size() >= max_results)
		return true;

	Handle h = inst.instantiate(implicand, var_soln);
	insert_result(h);

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
static Handle do_imply(AtomSpace* as,
                       const Handle& hbindlink,
                       Implicator& impl,
                       bool do_conn_check=false)
{
	BindLinkPtr bl(BindLinkCast(hbindlink));
	if (NULL == bl)
		bl = createBindLink(*LinkCast(hbindlink));

	impl.implicand = bl->get_implicand();

	bl->imply(impl, do_conn_check);

	if (0 < impl.get_result_list().size())
	{
		// The result_list contains a list of the grounded expressions.
		// (The order of the list has no significance, so it's really a set.)
		// Put the set into a SetLink, and return that.
		Handle gl = as->add_link(SET_LINK, impl.get_result_list());
		return gl;
	}

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
		std::map<Handle, Handle> empty_map;
		Handle h = impl.inst.instantiate(impl.implicand, empty_map);
		impl.insert_result(h);
	}

	return as->add_link(SET_LINK, impl.get_result_list());
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
Handle bindlink(AtomSpace* as, const Handle& hbindlink)
{
#ifdef CACHED_IMPLICATOR
	CachedDefaultImplicator impl(as);
#else
	DefaultImplicator impl(as);
#endif
	
	// Now perform the search.
	return do_imply(as, hbindlink, impl);
}

/**
 * Evaluate an pattern and rewrite rule embedded in a BindLink
 *
 * Returns the first match only. Otherwise, the behavior is identical to
 * PatternMatch::bindlink above.
 *
 * See the do_imply function documentation for details.
 */
Handle single_bindlink (AtomSpace* as, const Handle& hbindlink)
{
	// Now perform the search.
	DefaultImplicator impl(as);
	impl.max_results = 1;
	return do_imply(as, hbindlink, impl);
}

/**
 * Evaluate an pattern and rewrite rule embedded in a BindLink
 *
 * Returns the first N matches. Otherwise, the behavior is identical to
 * PatternMatch::bindlink above.
 *
 * See the do_imply function documentation for details.
 */
Handle first_n_bindlink (AtomSpace* as, unsigned int first_n, const Handle& hbindlink)
{
	// Now perform the search.
	DefaultImplicator impl(as);
	impl.max_results = first_n;
	return do_imply(as, hbindlink, impl);
}

/**
 * Attentional Focus specific PatternMatchCallback implementation
 */
Handle af_bindlink(AtomSpace* as, const Handle& hbindlink)
{
	// Now perform the search.
	AFImplicator impl(as);
	return do_imply(as, hbindlink, impl, false);
}

}

/* ===================== END OF FILE ===================== */
