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

#include <setjmp.h>

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
static thread_local PatternLinkPtr localpat = nullptr;
static thread_local jmp_buf begining;
static thread_local int cnt = 0;

bool ContinuationMixin::satisfy(const PatternLinkPtr& form)
{
printf("duude %d %d enter perf search; this=%p\n", cnt, in_continuation, this);
	if (in_continuation)
	{
		localpat = form;
		throw ContinuationException();
	}

	PatternLinkPtr lform = form;

	int rc = setjmp(begining);
printf("duuude setjump rc=%d\n", rc);
	if (rc) lform = localpat;
	else cnt = 0;

printf("duuude %d ======= base case %p\n", cnt, this);
	try
	{
		in_continuation = true;
		bool done = SatisfyMixin::satisfy(lform);
		in_continuation = false;
printf("duuude %d return from satsify mixing %p\n", cnt, this);
		return done;
	}
	catch (const ContinuationException& ex) {}

	cnt++;
	if (40 < cnt)
		throw RuntimeException(TRACE_INFO,
			"Suspect an infinite recursion loop! Are you sure? Form = %s",
			lform->to_string().c_str());

	Handle plk = createLink(_continuation->getOutgoingSet(), PUT_LINK);

printf("duuude %d %d post catch %p\n", cnt, in_continuation, this);
	AtomSpace* tas = TermMatchMixin::_temp_aspace;

printf("duuude %d %d enter loop %p\n", cnt, in_continuation, this);
printf("its %s\n", plk->to_short_string().c_str());
	tas->clear();
	try
	{
		bool crispy = EvaluationLink::crisp_eval_scratch(tas, plk, tas);
printf("duuude %d %d %p crispy=%d\n", cnt, in_continuation, this, crispy);
		if (crispy)
		{
			in_continuation = false;
			crispy = search_finished(crispy);
			return crispy;
		}
	}
	catch (const ContinuationException& ex) {}

printf("duuude %d %d caught on eval %p\n", cnt, in_continuation, this);
	longjmp(begining, 666);

	return true;
}

/* ===================== END OF FILE ===================== */
