/*
 * PatternJit.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  April 2015
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

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/pattern/PatternLink.h>

using namespace opencog;

/* ======================================================== */
/**
 * Just-In-Time analysis of patterns. Patterns we could not unpack
 * earlier, because the definitions for them might not have been
 * present, or may have changed since the pattern was initially created.
 */
PatternLinkPtr PatternLink::jit_analyze(void)
{
	PatternLinkPtr jit = PatternLinkCast(get_handle());

	// If there are no definitions, there is nothing to do.
	if (0 == _pat.defined_terms.size()) return jit;

	// Now is the time to look up the definitions!
	// We loop here, so that all recursive definitions are expanded
	// as well.  XXX Except that this is wrong, if any of the
	// definitions are actually recursive. That is, this will be
	// an infinite loop if a defintion is self-referencing; so
	// really we need to expand, one level at a time, during
	// evaluation, and only expand if really, really needed. (Which
	// then brings up ideas like tail recursion, etc.)  Anyway, most
	// of this code should probably be moved to PatternLink::jit_expand()
	while (0 < jit->_pat.defined_terms.size())
	{
		Variables vset;
		GroundingMap defnmap;
		for (const Handle& name : jit->_pat.defined_terms)
		{
			Handle defn = DefineLink::get_definition(name);
			if (not defn) continue;

			// Extract the variables in the definition.
			// Either they are given in a LambdaLink, or, if absent,
			// we just hunt down and bind all of them.
			if (nameserver().isA(LAMBDA_LINK, defn->get_type()))
			{
				LambdaLinkPtr lam = LambdaLinkCast(defn);
				vset.extend(lam->get_variables());
				defn = lam->get_body();
			}
			else
			{
				Variables freevars;
				freevars.find_variables(defn);
				vset.extend(freevars);
			}

			defnmap.insert({name, defn});
		}

		// Rebuild the pattern, expanding all DefinedPredicateNodes
		// to one level. Note that `newbody` is not being placed in
		// any atomspace; but I think that is OK...
		Handle newbody = Replacement::replace_nocheck(jit->_pat.body, defnmap);

		// We need to let both the PME know about the new clauses
		// and variables, and also let master callback class know,
		// too, since we are just one mixin in the callback class;
		// the other mixins need to be updated as well.
		vset.extend(jit->_variables);

		jit = createPatternLink(vset, newbody);
	}

#ifdef QDEBUG
	jit->debug_log("JIT expanded!");
#endif

	return jit;
}

/* ===================== END OF FILE ===================== */
