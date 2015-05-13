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

#include <opencog/atomspace/types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/Instantiator.h>
#include <opencog/query/PatternMatchCallback.h>
#include <opencog/query/PatternMatchEngine.h>

namespace opencog {

/**
 * Callback mixin class, used to provide a default node and link
 * matching behaviour. This class is still a pure virtual class,
 * since it does not implement the solution method.
 *
 * The *only* thing it provides is node and link matching; it does
 * not consider any truth values in establishing a match.
 *
 * Well, OK, it also provide search initiation via callback...
 */
class DefaultPatternMatchCB : public virtual PatternMatchCallback
{
	public:
		DefaultPatternMatchCB(AtomSpace*);
		virtual void set_pattern(const Variables&, const Pattern&);

		virtual bool node_match(const Handle&, const Handle&);
		virtual bool variable_match(const Handle&, const Handle&);
		virtual bool link_match(const LinkPtr&, const LinkPtr&);
		virtual bool post_link_match(const LinkPtr&, const LinkPtr&);

		/**
		 * Typically called for AbsentLink
		 */
		virtual bool optional_clause_match(const Handle& pattrn,
		                                   const Handle& grnd);

		/**
		 * Called when a virtual link is encountered. Returns false
		 * to reject the match.
		 */
		virtual bool evaluate_sentence(const Handle& pat,
		                           const std::map<Handle,Handle>& gnds)
		{ return eval_sentence(pat, gnds); }

		virtual const std::set<Type>& get_connectives(void)
		{
			return _connectives;
		}
	protected:

		ClassServer& _classserver;

		const VariableTypeMap* _type_restrictions;
		const std::set<Handle>* _dynamic;
		bool _have_evaluatables;

		// Temp atomspace used for test-groundings of virtual links.
		AtomSpace _temp_aspace;
		Instantiator _instor;

		// Crisp-logic evaluation of evaluatable terms
		std::set<Type> _connectives;
		bool eval_term(const Handle& pat,
		             const std::map<Handle,Handle>& gnds);
		bool eval_sentence(const Handle& pat,
		             const std::map<Handle,Handle>& gnds);

		AtomSpace* _as;
};

} // namespace opencog

#endif // _OPENCOG_DEFAULT_PATTERN_MATCH_H
