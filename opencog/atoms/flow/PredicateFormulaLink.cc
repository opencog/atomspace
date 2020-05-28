/*
 * PredicateFormulaLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atomspace/AtomSpace.h>
#include "PredicateFormulaLink.h"

using namespace opencog;

/// Custom variable extraction routine.
///
/// So, this is like a ScopeLink, except there do not need to be
/// any explicit variable decls, and the variables are usually free,
/// not typed, and there are *two* bodies, each body returning one
/// component of the final truth value...
///
/// XXX FIXME - in the future, some user is going to want to include
/// variable declarations, and/or an explicit Lambda in the body, for
/// some reason that I cannot imagine.  The code below will then fail.
/// For now, ignore this possibility.
void PredicateFormulaLink::init(void)
{
}

PredicateFormulaLink::PredicateFormulaLink(const HandleSeq&& oset, Type t)
	: ScopeLink(std::move(oset), t)
{
	if (not nameserver().isA(t, PREDICATE_FORMULA_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an PredicateFormulaLink, got %s", tname.c_str());
	}
	if (2 != _outgoing.size() and 3 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting two or three arguments, got %s", to_string().c_str());

	init();
}

// ---------------------------------------------------------------

/// When evaluated, this will return the TruthValue
TruthValuePtr PredicateFormulaLink::evaluate(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (1 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting one atom!");

	return getTruthValue();
}

DEFINE_LINK_FACTORY(PredicateFormulaLink, PREDICATE_FORMULA_LINK)

/* ===================== END OF FILE ===================== */
