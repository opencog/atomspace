/*
 * ContinuationMixin.cc
 *
 * Copyright (C) 2021 Linas Vepstas
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

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>

#include <opencog/atoms/core/Replacement.h>
#include <opencog/atoms/execution/EvaluationLink.h>

#include "ContinuationMixin.h"

using namespace opencog;

// #define QDEBUG 1
#ifdef QDEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif


/**
 * Exception thrown for continuations
 */
class ContinuationException : public SilentException
{
public:
    ContinuationException(void) {}
};

/* ======================================================== */

/**
 */
bool ContinuationMixin::evaluate_sentence(const Handle& top,
                                          const GroundingMap& gnds)
{
	if (CONTINUATION_LINK == top->get_type())
	{
		_continuation = Replacement::replace_nocheck(top, gnds);
		throw ContinuationException();
	}
	return TermMatchMixin::evaluate_sentence(top, gnds);
}

int cnt = 0;
bool ContinuationMixin::perform_search(PatternMatchCallback& pmc)
{
printf("duude %d enter perf search; this=%p\n", cnt, this);
	try
	{
		return InitiateSearchMixin::perform_search(pmc);
	}
	catch (const ContinuationException& ex)
	{
// printf("duude %d caught %s\n", cnt, _continuation->to_string().c_str());

		Handle plk = createLink(_continuation->getOutgoingSet(), PUT_LINK);
// printf("duude make %s\n", plk->to_string().c_str());

cnt++;
if (300 < cnt) return true;

		Handle red = HandleCast(plk->execute());
 printf("duude red %s\n", red->to_string().c_str());

		bool crispy = satisfy(PatternLinkCast(red));
printf("duuude %p crispy=%d\n", this, crispy);
return true;
	}
}

/* ===================== END OF FILE ===================== */
