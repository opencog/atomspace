/*
 * Satisfier.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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
#include <opencog/atoms/bind/PatternLink.h>

#include "BindLinkAPI.h"
#include "Satisfier.h"

using namespace opencog;

bool Satisfier::grounding(const std::map<Handle, Handle> &var_soln,
                          const std::map<Handle, Handle> &term_soln)
{
	// PatternMatchEngine::print_solution(var_soln, term_soln);
	_result = TruthValue::TRUE_TV();

	// Look for more groundings.
	return false;
}

bool SatisfyingSet::grounding(const std::map<Handle, Handle> &var_soln,
                              const std::map<Handle, Handle> &term_soln)
{
	// PatternMatchEngine::print_solution(var_soln, term_soln);

	if (1 == _varseq.size())
	{
		_satisfying_set.push_back(var_soln.at(_varseq[0]));
		return false;
	}

	// If more than one variable, encapsulate in sequential order,
	// in a ListLink.
	HandleSeq vargnds;
	for (const Handle& hv : _varseq)
	{
		vargnds.push_back(var_soln.at(hv));
	}
	_satisfying_set.push_back(Handle(createLink(LIST_LINK, vargnds)));

	// Look for more groundings.
	return false;
}

TruthValuePtr opencog::satisfaction_link(AtomSpace* as, const Handle& hlink)
{
	PatternLinkPtr bl(PatternLinkCast(hlink));
	if (NULL == bl)
	{
		// If it is a BindLink (for example), we want to use that ctor
		// instead of the default ctor.
		if (classserver().isA(hlink->getType(), SATISFACTION_LINK))
			bl = createPatternLink(*LinkCast(hlink));
		else
			bl = createPatternLink(hlink);
	}

	Satisfier sater(as);
	bl->satisfy(sater);

	return sater._result;
}

Handle opencog::satisfying_set(AtomSpace* as, const Handle& hlink)
{
	PatternLinkPtr bl(PatternLinkCast(hlink));
	if (NULL == bl)
	{
		// If it is a BindLink (for example), we want to use that ctor
		// instead of the default ctor.
		if (classserver().isA(hlink->getType(), GET_LINK))
			bl = createPatternLink(*LinkCast(hlink));
		else
			bl = createPatternLink(hlink);
	}

	SatisfyingSet sater(as);
	bl->satisfy(sater);

	return as->add_link(SET_LINK, sater._satisfying_set);
}

/* ===================== END OF FILE ===================== */
