/*
 * InitiateSearchCB.h
 *
 * Copyright (C) 2015 Linas Vepstas <linasvepstas@gmail.com>
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
 * Created by Linas Vepstas April 2015
 */

#ifndef _OPENCOG_INITIATE_SEARCH_H
#define _OPENCOG_INITIATE_SEARCH_H

#include <opencog/atomspace/types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/PatternMatchCallback.h>
#include <opencog/query/PatternMatchEngine.h>

namespace opencog {

/**
 * Callback mixin class, used to provide a default atomspace search.
 * This class is a pure virtual class, it does not implement any
 * of the matching callbacks.
 *
 * The *only* thing it provides is search initiation.
 */
class InitiateSearchCB : public virtual PatternMatchCallback
{
	public:
		InitiateSearchCB(AtomSpace*);

		/**
		 * Called to perform the actual search. This makes some default
		 * assumptions about the kind of things that might be matched,
		 * in order to drive a reasonably-fast search.
		 */
		virtual void set_pattern(const Variables&, const Pattern&);
		virtual bool initiate_search(PatternMatchEngine *);

	protected:

		ClassServer& _classserver;

		const Variables* _variables;
		const Pattern* _pattern;
		const VariableTypeMap* _type_restrictions;
		const std::set<Handle>* _dynamic;

		Handle _root;
		Handle _starter_term;

		virtual Handle find_starter(const Handle&, size_t&, Handle&, size_t&);
		virtual Handle find_starter_recursive(const Handle&, size_t&, Handle&,
		                                      size_t&);
		virtual Handle find_thinnest(const HandleSeq&,
		                             const std::set<Handle>&,
		                             Handle&, size_t&);
		virtual void find_rarest(const Handle&, Handle&, size_t&);

		bool _search_fail;
		virtual bool neighbor_search(PatternMatchEngine *);
		virtual bool disjunct_search(PatternMatchEngine *);
		virtual bool link_type_search(PatternMatchEngine *);
		virtual bool variable_search(PatternMatchEngine *);

		AtomSpace *_as;
};

} // namespace opencog

#endif // _OPENCOG_INITIATE_SEARCH_H
