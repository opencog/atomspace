/*
 * PrenexLink.cc
 *
 * Copyright (C) 2017 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string>

#include <opencog/util/mt19937ar.h>
#include <opencog/util/random.h>
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "PrenexLink.h"

using namespace opencog;

void PrenexLink::init(void)
{
	Type t = get_type();
	if (not classserver().isA(t, PRENEX_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PrenexLink, got %s", tname.c_str());
	}
}

PrenexLink::PrenexLink(const Handle& vars, const Handle& body)
	: RewriteLink(HandleSeq({vars, body}), PRENEX_LINK)
{
	init();
}

PrenexLink::PrenexLink(const HandleSeq& oset, Type t)
	: RewriteLink(oset, t)
{
	if (skip_init(t)) return;
	init();
}

PrenexLink::PrenexLink(const Link &l)
	: RewriteLink(l)
{
	if (skip_init(l.get_type())) return;
	init();
}

/* ================================================================= */

// Handle s

Handle PrenexLink::beta_reduce(const HandleMap& vmap) const
{
	HandleMap vm = vmap;

	// If any of the mapped values are ScopeLinks, we need to discover
	// and collect up the variables that they bind. We also need to
	// make sure that they are "fresh", i.e. don't have naming
	// collisions.
	HandleSeq final_varlist;
	HandleSet used_vars;

	Variables vars = get_variables();
	for (const Handle& var : vars.varseq)
	{
		// If we are not substituting for this, copy it over.
		const auto& pare = vm.find(var);
		if (vm.find(var) == vm.end())
		{
			// Is there a collision?
			if (used_vars.find(var) == used_vars.end())
			{
				final_varlist.push_back(var);
				used_vars.insert(var);
			}
			else
			{
				// Aiiee, there is a collision, make a new name!
				Handle alt;
				do
				{
					std::string altname = randstr(var->get_name() + "-");
					alt = createNode(VARIABLE_NODE, altname);
				} while (used_vars.find(alt) != used_vars.end());
				vm.insert({var, alt});
				used_vars.insert(alt);
			}
			continue;
		}

		// If we are here, then var is in to be beta-reduced.
		// Is the value a ScopeLink?
		Type vtype = pare->second->get_type();
		if (classserver().isA(vtype, SCOPE_LINK))
		{
			ScopeLinkPtr sc = ScopeLinkCast(pare->second);
			Variables bound = sc->get_variables();
printf("duuude ist scope\n");
			for (const Handle& bv : bound.varseq)
			{
				final_varlist.push_back(bv);
				used_vars.insert(bv);
			}
			vm[pare->first] = sc->get_body();
		}
	}

	// XXX this is wrong.
	return RewriteLink::beta_reduce(vm);
}

/* ================================================================= */

DEFINE_LINK_FACTORY(PrenexLink, PRENEX_LINK);

/* ===================== END OF FILE ===================== */
