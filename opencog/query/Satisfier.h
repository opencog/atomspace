/*
 * Satisfier.h
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#ifndef _OPENCOG_SATISFIER_H
#define _OPENCOG_SATISFIER_H

#include <vector>

#include <opencog/atoms/value/QueueValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include <opencog/query/ContinuationMixin.h>

namespace opencog {

/**
 * class Satisfier -- pattern matching callback for checking satisfaction.
 *
 * This class is meant to be used with the pattern matcher. When the
 * pattern matcher calls the callback, it will do so with a particular
 * grounding of the search pattern.
 *
 * This will set the result TV to TRUE_TV if a grounding is found. More
 * sophisticated TV calculations can be obtained by overloading this class.
 */

class Satisfier :
	public ContinuationMixin
{
	public: // Arghhh. OpenPsi accesses these directly...
		Handle _pattern_body;
		bool _have_variables;

	public:
		Satisfier(AtomSpace* as) :
			ContinuationMixin(as),
			_result(false) {}

		DECLARE_PE_MUTEX;
		HandleSeq _varseq;
		Handle _ground;
		bool _result;

		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat)
		{
			_varseq = vars.varseq;
			ContinuationMixin::set_pattern(vars, pat);
			_have_variables = not vars.varseq.empty();
			_pattern_body = pat.body;
		}

		// Return true if a satisfactory grounding has been
		// found. Note that in case where you want all possible
		// groundings, this will usually return false, so the
		// patternMatchEngine can keep looking for ever more
		// groundings.
		virtual bool propose_grounding(const GroundingMap &var_soln,
		                               const GroundingMap &term_soln);

		// Final pass, if no grounding was found.
		virtual bool search_finished(bool);
};

/**
 * class SatisfyingSet -- pattern matching callback for finding satsifaction.
 *
 * This class is meant to be used with the pattern matcher. When the
 * pattern matcher calls the callback, it will do so with a particular
 * grounding of the search pattern.
 *
 * This will record every grounding that is found. Thus, after running,
 * the SatisfyingSet can be examined to see all the groundings that were
 * found.
 */

class SatisfyingSet :
	public ContinuationMixin
{
	protected:
		AtomSpace* _as;
		DECLARE_PE_MUTEX;
		HandleSeq _varseq;
		QueueValuePtr _result_queue;

		ValuePtr wrap_result(const GroundingMap &var_soln);
		size_t _num_results;
		std::map<GroundingMap, ValueSet> _groups;

	public:
		SatisfyingSet(AtomSpace* as) :
			ContinuationMixin(as),
			_as(as), _num_results(0), max_results(SIZE_MAX) {}

		size_t max_results;

		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat)
		{
			_varseq = vars.varseq;
			ContinuationMixin::set_pattern(vars, pat);
		}

		// Return true if a satisfactory grounding has been
		// found. Note that in case where you want all possible
		// groundings, this will usually return false, so the
		// patternMatchEngine can keep looking for ever more
		// groundings.
		virtual bool propose_grounding(const GroundingMap &var_soln,
		                               const GroundingMap &term_soln);
		virtual bool propose_grouping(const GroundingMap &var_soln,
		                              const GroundingMap &term_soln,
		                              const GroundingMap &group);

		virtual bool start_search(void);
		virtual bool search_finished(bool);

		virtual QueueValuePtr get_result_queue()
		{ return _result_queue; }
};

}; // namespace opencog

#endif // _OPENCOG_SATISFIER_H
