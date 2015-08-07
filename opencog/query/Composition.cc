/*
 * Composition.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  aPRIL 2015
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

#include <opencog/atoms/bind/BetaRedex.h>
#include <opencog/atoms/bind/PatternLink.h>

#include "PatternMatchEngine.h"
#include "PatternMatchCallback.h"

using namespace opencog;

// Uncomment below to enable debug print
// #define DEBUG
#ifdef DEBUG
	#define dbgprt(f, varargs...) printf(f, ##varargs)
#else
	#define dbgprt(f, varargs...)
#endif

static inline void prtmsg(const char * msg, const Handle& h)
{
#ifdef DEBUG
	if (h == Handle::UNDEFINED) {
		printf("%s (invalid handle)\n", msg);
		return;
	}
	std::string str = h->toShortString();
	printf("%s %s\n", msg, str.c_str());
#endif
}

/* ================================================================= */
/*
TODO:
-- for non-connected satisfaction links, the ctor should take each
   connected comp, and compute the mandatories, the optionals, the
   evaluatables and the connectivity map for each comp, and store
   thse in a meta-pseudo satisfactonLink.  Should invent a new link
   type for this...

-- where the heck is type enforcement done, again??? Need type
   enforcement during the redex...
*/
/* ================================================================= */

/* Reset the current variable grounding to the last grounding pushed
 * onto the stack. */
#define POPGND(soln,stack) {         \
	OC_ASSERT(not stack.empty(), "Unbalanced grounding stack"); \
	soln = stack.top();               \
	stack.pop();                      \
}

/* ================================================================= */

void PatternMatchEngine::push_redex(void)
{
	_stack_variables.push(_varlist);
	_stack_pattern.push(_pat);
}

void PatternMatchEngine::pop_redex(void)
{
	_varlist = _stack_variables.top();
	_stack_variables.pop();

	_pat = _stack_pattern.top();
	_stack_pattern.pop();
}

bool PatternMatchEngine::redex_compare(const LinkPtr& lp,
                                       const LinkPtr& lg)
{
	// If we are here, the pattern is defined in a DefineLink. We
	// must match to that. There seem to be two strategies for doing
	// that:  Method A: rename all of the variables in the defined
	// pattern to be the variables we are actually using in the
	// top-level search.  This seems easy, but it is wrong, for two
	// reasons. One reason is that, after renaming, we will have
	// created a pattern that is probably not in the atomspace.
	// That means that the pattern will have atoms with invalid UUID's
	// in them, causing trouble down the line. The other problem is
	// that the variables in the defined target now look like perfectly
	// good grounding candidates, and so get found and reported as valid
	// grounds. So, for these two reasons, the simple, "obvious" method
	// A is out. Instead, we implement method B: we rename the variables
	// that the match engine is carrying, to correspond with the variable
	// names that are native to the definition. This way, inside the body
	// of the definition, everything looks "normal", and should thus
	// proceed as formal.  Of course, on exit, we have to unmasquerade.
	//
	// By "everything looks normal", we really mean "treat this as if it
	// was a brand-new pattern matching problem".  To do this, it is very
	// empting to just create a new PME, and let it run. The problem with
	// that is that we have no particularly good way of integrating the
	// new pme state, with the existing pme state. So we don't.  Instead,
	// we push all pme state, clear the decks, (almost as if starting from
	// scratch) and then pop all pme state when we are done.

	BetaRedexPtr cpl(BetaRedexCast(lp));

	// Get the variales and clauses that make up the redex.
	// We expect the redex body to be a PatternLink.
	// XXX TODO perhaps we should create a special sat-redex-only
	// link type?
	Handle hsat(cpl->get_definition());
	PatternLinkPtr sat_link(PatternLinkCast(hsat));
	if (NULL == sat_link)
		throw InvalidParamException(TRACE_INFO,
			"Expecting PatternLink, got %s",
				hsat->toString().c_str());

	push_redex();
	_varlist = &sat_link->get_variables();
	_pat = &sat_link->get_pattern();

	// To explore this redex, we've got to translate the current
	// traversal state into the "local frame". Do this by tranlsating
	// (masquerading) any grounded variables we may have so far.
	const Variables& local_args(cpl->get_local_args());
	const HandleSeq& redex_args(cpl->get_args());

	clause_stacks_push();
	clear_current_state();
// XXX TODO handle clause_grounding as well ?? why

	SolnMap local_grounding;
	size_t sz = redex_args.size();
	for (size_t i=0; i< sz; i++)
	{
		// Relabel (masquerade) the grounded vars.
		auto iter = var_grounding.find(redex_args[i]);
		if (iter == var_grounding.end()) continue;
		local_grounding.insert({local_args.varseq[i], iter->second});
	}
	var_grounding = local_grounding;

	if (1 != _pat->cnf_clauses.size())
		throw InvalidParamException(TRACE_INFO,
			"Redex can currently handle only one clause!");

	// Since there is just a single clause, just compare it as a tree
	clause_accepted = false;

	Handle hp(_pat->cnf_clauses[0]);
	throw RuntimeException(TRACE_INFO, "Unimplemented yet");
	// TODO: wrap by PatternTermPtr
	// bool found = tree_compare(hp, Handle(lg), CALL_COMP);
	bool found = false;

#if 0
	Handle join;
	Handle root;
	// Follow the connectivity graph, to find a joint to
	// somethig we know about...
	for (const ConnectPair& vk : _connectivity_map)
	{
		join = vk.first;
		if (var_grounding[join])
		{
			root = vk.second[0];
			break;
		}
	}
	if (Handle::UNDEFINED == root)
		throw InvalidParamException(TRACE_INFO,
			"Badly structured redex!");

	prtmsg("redex starting with clause: ", root);
	curr_root = root;
	curr_term_handle = join;
	clause_accepted = false;

   bool found = soln_up(curr_soln_handle);
#endif

	dbgprt("redex finishing; found match=%d\n", found);

	// No match; restore original grounding and quit
	if (not found)
	{
		clause_stacks_pop();
		pop_redex();
		return false;
	}

	// If there is a match, then maybe we grounded some variables.
	// If so, we need to unmasquerade them.
	local_grounding = var_grounding;
	clause_stacks_pop();
	for (size_t i=0; i< sz; i++)
	{
		auto iter = local_grounding.find(local_args.varseq[i]);
		if (iter != local_grounding.end())
			var_grounding.insert({redex_args[i], iter->second});
	}

	pop_redex();
	return true;
}

/* ===================== END OF FILE ===================== */
