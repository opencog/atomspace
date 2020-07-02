/*
 * Recognizer.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_RECOGNIZER_H
#define _OPENCOG_RECOGNIZER_H

#include <opencog/query/TermMatchMixin.h>
#include <opencog/query/SatisfyMixin.h>

namespace opencog {

/**
 * Pattern recognition is the dual of pattern matching.
 * That is, rather than using one query pattern to search a large
 * collection of data, this does the opposite: given a single piece of
 * data, it searches for all rules/queries which apply to it.
 *
 * The file /examples/pattern-matcher/recog.scm provides a simple
 * example: it implements a pseudo-AIML lookup, where an input sentence
 * (for example, `I love you`) will match the rewrite rules
 *    `I * you   --> I * you too`
 *    `I love *  --> I like * a lot!`
 *
 * The is, the constant clause `I love you` can be recognized as
 * grounding two different graphs with variables in them: the graph
 * `I * you` and `I love *`.
 */
class Recognizer :
	public TermMatchMixin,
	public SatisfyMixin
{
	private:
		bool match = false;

	protected:
		const Pattern* _pattern;

		DECLARE_PE_MUTEX;
		PatternTermPtr _root;
		Handle _starter_term;
		size_t _cnt;
		bool do_search(PatternMatchCallback&, const Handle&);
		bool loose_match(const Handle&, const Handle&);

	public:
		HandleSet _rules;

		Recognizer(AtomSpace* as) :
		    TermMatchMixin(as),
		    _pattern(nullptr),
		    _cnt(0)
		{}

		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat)
		{
			_pattern = &pat;
			TermMatchMixin::set_pattern(vars, pat);
		}

		virtual bool node_match(const Handle&, const Handle&);
		virtual bool link_match(const PatternTermPtr&, const Handle&);
		virtual bool fuzzy_match(const Handle&, const Handle&);
		virtual bool grounding(const GroundingMap &var_soln,
		                       const GroundingMap &term_soln);
		virtual bool perform_search(PatternMatchCallback&);
		virtual void next_connections(const GroundingMap&);
		virtual bool get_next_clause(PatternTermPtr&, Handle&);
};

} // namespace opencog

#endif // _OPENCOG_RECOGNIZER_H
