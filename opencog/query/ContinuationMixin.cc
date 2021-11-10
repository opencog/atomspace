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

static thread_local bool in_continuation = false;

int cnt = 0;
bool ContinuationMixin::perform_search(PatternMatchCallback& pmc)
{
printf("duude %d %d enter perf search; this=%p\n", cnt, in_continuation, this);
	if (in_continuation)
		throw ContinuationException();

printf("duude %d base case this=%p\n", cnt, this);
	try
	{
		in_continuation = true;
		bool done = InitiateSearchMixin::perform_search(pmc);
		in_continuation = false;
		return done;
	}
	catch (const ContinuationException& ex) {}

	Handle plk = createLink(_continuation->getOutgoingSet(), PUT_LINK);

printf("duuude %d %d post catch %p\n", cnt, in_continuation, this);
	AtomSpace* tas = TermMatchMixin::_temp_aspace;
	while (true)
	{
cnt++;
if (40 < cnt) return true;

printf("duuude %d %d enter loop %p\n", cnt, in_continuation, this);
printf("its %s\n", plk->to_short_string().c_str());
		tas->clear();
		try
		{
			bool crispy = EvaluationLink::crisp_eval_scratch(tas, plk, tas);
printf("duuude %d %d %p crispy=%d\n", cnt, in_continuation, this, crispy);
			if (crispy) return crispy;
		}
		catch (const ContinuationException& ex)
		{
printf("duuude %d %d caught on eval %p\n", cnt, in_continuation, this);
		}
	}
}

/* ===================== END OF FILE ===================== */
