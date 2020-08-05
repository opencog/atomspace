/*
 * TermMatchMixin.h
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

#ifndef _OPENCOG_TERM_MATCH_MIXIN_H
#define _OPENCOG_TERM_MATCH_MIXIN_H

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog {

/**
 * Callback mixin class, used to provide a default node and link
 * matching behaviour. This class is a pure virtual class, since
 * it does not implement either the `perform_search()` method,
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
class TermMatchMixin : public virtual PatternMatchCallback
{
	public:
		TermMatchMixin(AtomSpace*);
		~TermMatchMixin();
		virtual void set_pattern(const Variables&, const Pattern&);

		virtual bool node_match(const Handle&, const Handle&);
		virtual bool variable_match(const Handle&, const Handle&);
		virtual bool scope_match(const Handle&, const Handle&);

		virtual bool link_match(const PatternTermPtr&, const Handle&);
		virtual bool post_link_match(const Handle&, const Handle&);
		virtual void post_link_mismatch(const Handle&, const Handle&);

		virtual bool clause_match(const Handle&, const Handle&,
		                          const GroundingMap&);

		/** Called for AbsentLink */
		virtual bool optional_clause_match(const Handle& pattrn,
		                                   const Handle& grnd,
		                                   const GroundingMap&);

		/** Called for AlwaysLink */
		virtual bool always_clause_match(const Handle& pattrn,
		                                 const Handle& grnd,
		                                 const GroundingMap&);

		virtual IncomingSet get_incoming_set(const Handle&, Type);

		/**
		 * Called when a virtual link is encountered. Returns false
		 * to reject the match.
		 */
		virtual bool evaluate_sentence(const Handle& pat, const GroundingMap& gnds)
		{ return eval_sentence(pat, gnds); }

		virtual const TypeSet& get_connectives(void)
		{
			return _connectives;
		}

		// Remarks:
		// 1) This could be made virtual, if someone wants to over-load.
		// 2) End-users should stop depending on this, and should start
		//    using `(GreaterThan (TruthValueOf X) (Number 0.5))` instead.
		bool crisp_truth_from_tv(const TruthValuePtr& tvp)
		{ return tvp->get_mean() >= 0.5; }

		bool optionals_present(void) { return _optionals_present; }

	protected:
		NameServer& _nameserver;

		const Variables* _vars = nullptr;

		bool is_self_ground(const Handle&, const Handle&,
		                    const GroundingMap&, const HandleSet&,
		                    Quotation quotation=Quotation());

		// Variables that should be ignored, because they are bound
		// (scoped) in the current context (i.e. appear in a ScopeLink
		// that is being matched.)
		const Variables* _pat_bound_vars;
		const Variables* _gnd_bound_vars;

		// Temp atomspace used for test-groundings of virtual links.
		AtomSpace* _temp_aspace;

		// Crisp-logic evaluation of evaluatable terms
		TypeSet _connectives;
		bool eval_term(const Handle& pat, const GroundingMap& gnds);
		bool eval_sentence(const Handle& pat, const GroundingMap& gnds);

		bool _optionals_present = false;
		AtomSpace* _as;
};

} // namespace opencog

#endif // _OPENCOG_TERM_MATCH_MIXIN_H
