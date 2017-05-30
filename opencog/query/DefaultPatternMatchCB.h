/*
 * DefaultPatternMatchCB.h
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
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
 *
 * Created by Linas Vepstas January 2009
 */

#ifndef _OPENCOG_DEFAULT_PATTERN_MATCH_H
#define _OPENCOG_DEFAULT_PATTERN_MATCH_H

#include <opencog/atoms/base/types.h>
#include <opencog/atoms/base/Quotation.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/query/PatternMatchCallback.h>
#include <opencog/query/PatternMatchEngine.h>

namespace opencog {

/**
 * Callback mixin class, used to provide a default node and link
 * matching behaviour. This class is a pure virtual class, since
 * it does not implement either the `initiate_search()` method,
 * nor the `solution()` method.
 *
 * It provides is node and link matching, assuming the canonical
 * meaning of VariableNodes and QuoteLinks. It also implements
 * crisp-logic handling of AndLink, OrLink, NotLink when these
 * are combined with AbsentLink, EqualLink, GreaterThanLink, and
 * other clear-box evaluatable link types.
 *
 * It handles AbsentLink using standard intuitionist logic,
 * and provides tracking of an _optionals_present flag, so that
 * conversion to explicit classical logic can be performed at the
 * conclusion of the search.  The default implicator performs
 * conversion; see the notes there for details.
 */
class DefaultPatternMatchCB : public virtual PatternMatchCallback
{
	public:
		DefaultPatternMatchCB(AtomSpace*);
		~DefaultPatternMatchCB();
		virtual void set_pattern(const Variables&, const Pattern&);

		virtual bool node_match(const Handle&, const Handle&);
		virtual bool variable_match(const Handle&, const Handle&);
		virtual bool scope_match(const Handle&, const Handle&);

		virtual bool link_match(const PatternTermPtr&, const Handle&);
		virtual bool post_link_match(const Handle&, const Handle&);
		virtual void post_link_mismatch(const Handle&, const Handle&);

		virtual bool clause_match(const Handle&, const Handle&,
		                          const HandleMap&);
		/**
		 * Typically called for AbsentLink
		 */
		virtual bool optional_clause_match(const Handle& pattrn,
		                                   const Handle& grnd,
		                                   const HandleMap&);

		virtual IncomingSet get_incoming_set(const Handle&);

		/**
		 * Called when a virtual link is encountered. Returns false
		 * to reject the match.
		 */
		virtual bool evaluate_sentence(const Handle& pat, const HandleMap& gnds)
		{ return eval_sentence(pat, gnds); }

		virtual const std::set<Type>& get_connectives(void)
		{
			return _connectives;
		}

		bool optionals_present(void) { return _optionals_present; }
	protected:

		ClassServer& _classserver;

		const Variables* _vars = NULL;
		const OrderedHandleSet* _dynamic = NULL;
		bool _have_evaluatables = false;
		const OrderedHandleSet* _globs = NULL;

		bool _have_variables;
		Handle _pattern_body;

		bool is_self_ground(const Handle&, const Handle&,
		                    const HandleMap&, const OrderedHandleSet&,
		                    Quotation quotation=Quotation());

		// Variables that should be ignored, because they are bound
		// (scoped) in the current context (i.e. appear in a ScopeLink
		// that is being matched.)
		const Variables* _pat_bound_vars;
		const Variables* _gnd_bound_vars;

		// Temp atomspace used for test-groundings of virtual links.
		AtomSpace* _temp_aspace;
		Instantiator* _instor;

		// The transient atomspace cache. The goal here is to
		// avoid the overhead of constantly creating/deleting
		// the temp atomspaces above. So instead, just keep a
		// cache of empty ones, ready to go.
		static std::mutex s_transient_cache_mutex;
		static std::vector<AtomSpace*> s_transient_cache;
		static AtomSpace* grab_transient_atomspace(AtomSpace* parent);
		static void release_transient_atomspace(AtomSpace* atomspace);

#ifdef CACHED_IMPLICATOR
		virtual void ready(AtomSpace*);
		virtual void clear();
#endif
		// Crisp-logic evaluation of evaluatable terms
		std::set<Type> _connectives;
		bool eval_term(const Handle& pat, const HandleMap& gnds);
		bool eval_sentence(const Handle& pat, const HandleMap& gnds);

		bool _optionals_present = false;
		AtomSpace* _as;
};

} // namespace opencog

#endif // _OPENCOG_DEFAULT_PATTERN_MATCH_H
