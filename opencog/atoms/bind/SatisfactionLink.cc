/*
 * SatisfactionLink.cc
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
#include <opencog/atomutils/FindUtils.h>

#include "SatisfactionLink.h"

using namespace opencog;

void SatisfactionLink::init(void)
{
	extract_variables(_outgoing);
	unbundle_clauses(_body);
	common_init();
}

SatisfactionLink::SatisfactionLink(const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(SATISFACTION_LINK, hseq, tv, av)
{
	init();
}

SatisfactionLink::SatisfactionLink(const Handle& body,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(SATISFACTION_LINK, HandleSeq({body}), tv, av)
{
	init();
}

SatisfactionLink::SatisfactionLink(const Handle& vars, const Handle& body,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(SATISFACTION_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

SatisfactionLink::SatisfactionLink(Type t, const HandleSeq& hseq,
                   TruthValuePtr tv, AttentionValuePtr av)
	: PatternLink(t, hseq, tv, av)
{
	// BindLink has a different clause initialization sequence
	if (SATISFACTION_LINK != t) return;
	init();
}

SatisfactionLink::SatisfactionLink(Link &l)
	: PatternLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, SATISFACTION_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a SatisfactionLink, got %s", tname.c_str());
	}

	// Derived types willl have a different initialization sequence
	if (SATISFACTION_LINK != tscope) return;
	init();
}

/// Constructor that takes a pre-determined set of variables, and
/// a list of clauses to solve.  This is currently kind-of crippled,
/// since no variable type restricions are possible, and no optionals,
/// either.  By contrast, the PatternLink constructor does allow these
/// things, but it does not allow virtual links. Alas.
SatisfactionLink::SatisfactionLink(const std::set<Handle>& vars,
                                   const HandleSeq& clauses)
	: PatternLink(SATISFACTION_LINK, HandleSeq())
{
	_varlist.varset = vars;
	_pat.clauses = clauses;
	common_init();
}

/* ===================== END OF FILE ===================== */
