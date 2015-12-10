/*
 * BindLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/core/FreeLink.h>

#include "BindLink.h"

using namespace opencog;

void BindLink::init(void)
{
	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
	setup_components();
	_pat.redex_name = "anonymous BindLink";
}

BindLink::BindLink(const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(BIND_LINK, hseq, tv, av)
{
	init();
}

BindLink::BindLink(Type t, const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(t, hseq, tv, av)
{
	init();
}

BindLink::BindLink(Link &l)
	: PatternLink(l)
{
	Type t = l.getType();
	if (not classserver().isA(t, BIND_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a BindLink, got %s", tname.c_str());
	}

	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
/// On top of that initialize _body and _implicand with the
/// clauses and the rewrite rule.
///
void BindLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (3 < sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of at most two, got %d", sz);

	// If the outgoing set size is one, then there are no variable
	// declarations; extract all free variables.
	if (2 == sz)
	{
		_vardecl = Handle::UNDEFINED;
		_body = oset[0];
		_implicand = oset[1];
		_varlist.find_variables(oset[0]);
		return;
	}

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_vardecl = oset[0];
	_body = oset[1];
	_implicand = oset[2];

	// Initialize _varlist with the scoped variables
	init_scoped_variables(oset[0]);
}

/* ===================== END OF FILE ===================== */
