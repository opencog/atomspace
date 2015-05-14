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
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atoms/bind/BindLink.h>

#include "BindLink.h"
#include "DefaultImplicator.h"
#include "PatternMatch.h"

using namespace opencog;

/**
 * This callback takes the reported grounding, runs it through the
 * instantiator, to create the implicand, and then records the result
 * in the public member `result_list`.  If the number of results so
 * far is less than `max_results`, it then returns false, to search
 * for more groundings.  (The engine will halt its search for a
 * grounding once an acceptable one has been found; so, to continue
 * hunting for more, we return `false` here. We want to find all
 * possible groundings.)
 */
bool Implicator::grounding(const std::map<Handle, Handle> &var_soln,
                           const std::map<Handle, Handle> &term_soln)
{
	// PatternMatchEngine::print_solution(term_soln,var_soln);
	Handle h = inst.instantiate(implicand, var_soln);
	if (Handle::UNDEFINED != h)
		result_list.push_back(h);

	// If we found as many as we want, then stop looking for more.
	if (result_list.size() < max_results)
		return false;
	return true;
}


namespace opencog
{

/**
 * Simplified utility
 *
 * The `do_conn_check` flag stands for "do connectivity check";
 * if the flag is set, and the pattern is disconnected, then an
 * error will be thrown. PLN explicitly allows disconnected graphs.
 */
static Handle do_imply(AtomSpace* as,
                       const Handle& hbindlink,
                       Implicator& impl,
                       bool do_conn_check=true)
{
	BindLinkPtr bl(BindLinkCast(hbindlink));
	if (NULL == bl)
		bl = createBindLink(*LinkCast(hbindlink));

	impl.implicand = bl->get_implicand();

	bl->imply(impl, do_conn_check);

	if (0 < impl.result_list.size())
	{
		// The result_list contains a list of the grounded expressions.
		// (The order of the list has no significance, so it's really a set.)
		// Put the set into a SetLink, and return that.
		Handle gl = as->addLink(SET_LINK, impl.result_list);
		return gl;
	}

	// OK, there are two ways to explain what is happening here; the
	// fancy way and the pedestrian day.  Lets try the fancy way first...
	//
	// The code below struggles to handle a conversion of the
	// intuitionistic logic used during the course of pattern matching
	// into a final classical logic form, by implementing "Reductio ad
	// absurdum" (RAA). RAA says "If something is not not false, then
	// it must be true".  See Dirk van Dalen, "Logic and Structure",
	// (2003) Springer; chapter 5 for an explantion, or the Stanford
	// Encyclopedia of Philosophy article on Intuitionistic Logic
	// http://plato.stanford.edu/entries/logic-intuitionistic/
	//
	// So: the pattern matcher uses intuitionistic logic to avoid the
	// the Turing machine halting problem: either a pattern is in the
	// atomspace (i.e. it is "provable") or it is absent (it may be
	// provable, but we do not know of either a proof or a dis-proof).
	// Think of the atomspace as a Kripke semantics database; it holds
	// everything that we know at time t.
	//
	// However, there are certain useful queries, where the goal of
	// the query is to determine that some clause or set of clauses
	// are absent in the AtomSpace. If the clauses are jointly not
	// found, after a full and exhaustive search, then we want to run
	// the implicator, and perform some action. Easier said than done,
	// this code is currently a hack.

	const Pattern& pat = bl->get_pattern();
	DefaultPatternMatchCB* intu =
		dynamic_cast<DefaultPatternMatchCB*>(&impl);
	if (0 == pat.mandatory.size() and 0 < pat.optionals.size()
	    and not intu->optionals_present())
	{
		std::map<Handle, Handle> empty_map;
		Handle h = impl.inst.instantiate(impl.implicand, empty_map);
		if (Handle::UNDEFINED != h)
			impl.result_list.push_back(h);
	}

	return as->addLink(SET_LINK, impl.result_list);
}

/**
 * Evaluate an ImplicationLink embedded in a BindLink
 *
 * Use the default implicator to find pattern-matches. Associated truth
 * values are completely ignored during pattern matching; if a set of
 * atoms that could be a ground are found in the atomspace, then they
 * will be reported.
 *
 * See the do_bindlink function documentation for details.
 */
Handle bindlink(AtomSpace* as, const Handle& hbindlink)
{
	// Now perform the search.
	DefaultImplicator impl(as);
	return do_imply(as, hbindlink, impl);
}

/**
 * Evaluate an ImplicationLink embedded in a BindLink
 *
 * Returns the first match only. Otherwise, the behavior is identical to
 * PatternMatch::bindlink above.
 *
 * See the do_bindlink function documentation for details.
 */
Handle single_bindlink (AtomSpace* as, const Handle& hbindlink)
{
	// Now perform the search.
	DefaultImplicator impl(as);
	impl.max_results = 1;
	return do_imply(as, hbindlink, impl);
}

/**
 * PLN specific PatternMatchCallback implementation
 */
Handle pln_bindlink(AtomSpace* as, const Handle& hbindlink)
{
	// Now perform the search.
	PLNImplicator impl(as);
	return do_imply(as, hbindlink, impl, false);
}

}

/* ===================== END OF FILE ===================== */
