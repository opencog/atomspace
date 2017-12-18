/*
 * opencog/atoms/core/ComposeLink.cc
 *
 * Copyright (C) 2017 Nil Geisweiller
 * All Rights Reserved
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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/RewriteLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ComposeLink.h"

using namespace opencog;

void ComposeLink::check() const
{
	if (not classserver().isA(get_type(), COMPOSE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ComposeLink");

	if (get_arity() != 2)
		throw InvalidParamException(TRACE_INFO, "Expecting two outgoings");
}

Variables ComposeLink::variables_intersection(const HandleSeq& scopes) const
{
	Variables variables;
	unsigned maxpro = 0; // maximum index of the projected argument
	for (const Handle& h :  scopes) {
		ScopeLinkPtr sc = ScopeLinkCast(h);
		if (sc) {
			if (variables.empty()) {
				variables = sc->get_variables();
			} else {
				// Make sure all variable declarations are
				// alpha-equivalent
				OC_ASSERT(variables == sc->get_variables());
			}
		} else {
			maxpro = std::max(maxpro, projection_index(h));
		}
	}
	// Make sure that no projection is going out of bound
	OC_ASSERT(maxpro < variables.size(), "Projection out of bound");

	return variables;
}

unsigned ComposeLink::projection_index(const Handle& projection)
{
	OC_ASSERT(projection->get_type() == PROJECT_LINK);
	NumberNodePtr num = NumberNodeCast(projection->getOutgoingAtom(0));
	OC_ASSERT(num != nullptr);
	OC_ASSERT(0 <= num->get_value());
	return num->get_value();
}

ComposeLink::ComposeLink(const HandleSeq oset, Type t) : FunctionLink(oset, t)
{
	check();
}

ComposeLink::ComposeLink(const Link& l) : FunctionLink(l)
{
	check();
}

Handle ComposeLink::execute() const
{
	Handle g = getOutgoingAtom(0);
	Handle f = getOutgoingAtom(1);
	RewriteLinkPtr g_sc = RewriteLinkCast(g);
	OC_ASSERT(g_sc != nullptr, "First atom must be a RewriteLink");

	const Variables& g_vars = g_sc->get_variables();
	if (g_vars.size() == 1) {
		// g has one variable only, thus we expect f to be a scope as
		// opposed to a list of scopes
		RewriteLinkPtr f_sc = RewriteLinkCast(f);
		OC_ASSERT(f_sc != nullptr, "Second atom must be a RewriteLink");

		return compose(f_sc->get_vardecl(), {f_sc->get_body()});
	}

	// f must be a list of scopes
	OC_ASSERT(f->get_type() == LIST_LINK);
	// With an equal number of variables from g vardecl
	OC_ASSERT(f->get_arity() == g_vars.size());
	const HandleSeq& scopes = f->getOutgoingSet();

	// Extract the variable declaration intersection
	Variables n_vars = variables_intersection(scopes);

	// Build sequence of values (body functions) for substitution
	HandleSeq values;
	for (const Handle& fi : f->getOutgoingSet()) {
		if (fi->get_type() == PROJECT_LINK) {
			values.push_back(n_vars.varseq[projection_index(fi)]);
		} else {
			RewriteLinkPtr fi_sc = RewriteLinkCast(fi);
			OC_ASSERT(fi_sc != nullptr);
			// Make sure its variables have the same named as the new
			// variable declaration
			Handle afi = fi_sc->alpha_conversion(n_vars.varseq);
			RewriteLinkPtr afi_sc = RewriteLinkCast(afi);
			values.push_back(afi_sc->get_body());
		}
	}
	return compose(n_vars, values);
}

Handle ComposeLink::compose(const Handle& nvardecl,
                            const HandleSeq& values) const
{
	Handle g = getOutgoingAtom(0);
	RewriteLinkPtr g_sc = RewriteLinkCast(g);
	OC_ASSERT(g_sc != nullptr, "First outgoing must be a scope");

	HandleSeq comp_hs = g_sc->partial_substitute_bodies(nvardecl, values);

	// Insert fvardecl if the outgoings if defined
	if (nvardecl)
		comp_hs.insert(comp_hs.begin(), nvardecl);

	// Create composed scope
	return createLink(comp_hs, g->get_type());
}

Handle ComposeLink::compose(const Variables& nvars,
                            const HandleSeq& values) const
{
	return compose(nvars.get_vardecl(), values);
}

DEFINE_LINK_FACTORY(ComposeLink, COMPOSE_LINK);
