/*
 * RuleLink.cc
 *
 * Copyright (C) 2009, 2014, 2015, 2019, 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/query/Implicator.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "RuleLink.h"

using namespace opencog;

void RuleLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, RULE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a RuleLink, got %s", tname.c_str());
	}

	// If we are quoted, don't bother to try to do anything.
	if (_quoted) return;

	extract_variables(_outgoing);
}

RuleLink::RuleLink(const Handle& vardecl,
                   const Handle& body,
                   const Handle& rewrite)
	: RuleLink(HandleSeq{vardecl, body, rewrite})
{}

RuleLink::RuleLink(const Handle& body, const Handle& rewrite)
	: RuleLink(HandleSeq{body, rewrite})
{}

RuleLink::RuleLink(const HandleSeq&& hseq, Type t)
	: PrenexLink(std::move(hseq), t)
{
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
/// On top of that, initialize _body and _implicand with the
/// clauses and the rewrite rule(s). (Multiple implicands are
/// allowed, this can save some CPU cycles when one search needs to
/// create several rewrites.)
///
void RuleLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (sz < 1)
		throw SyntaxException(TRACE_INFO,
			"Expecting a non-empty outgoing set");

	// Old-style declarations had variables in the first
	// slot. If they are there, then respect that.
	// Otherwise, the first slot holds the body.
	size_t boff = 0;
	Type vt = oset[0]->get_type();
	if (VARIABLE_LIST == vt or
	    VARIABLE_SET == vt or
	    TYPED_VARIABLE_LINK == vt or
	    VARIABLE_NODE == vt or
	    GLOB_NODE == vt)
	{
		_vardecl = oset[0];
		init_scoped_variables(_vardecl);
		boff = 1;
	}

	// We know sz=1 or greater. 
	if (sz == boff)
		throw SyntaxException(TRACE_INFO,
			"Expecting a delcaration of a body/premise!");

	_body = oset[boff];
	for (size_t i=boff+1; i < sz; i++)
		_implicand.push_back(oset[i]);

	// Hunt for variables only if they were not declared.
	// Mixing both styles together breaks unit tests.
	if (0 == boff)
	{
		_variables.find_variables(_body);
		_variables.find_variables(_implicand);
	}
}

DEFINE_LINK_FACTORY(RuleLink, RULE_LINK)

/* ===================== END OF FILE ===================== */
