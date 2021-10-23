/*
 * InitiateSearchMixin.h
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

#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog {

class AtomSpace;

/**
 * Callback mixin class, used to provide a default atomspace search.
 * This class is a pure virtual class, it does not implement any
 * of the matching callbacks.
 *
 * The *only* thing it provides is search initiation.
 */
class InitiateSearchMixin : public virtual PatternMatchCallback
{
public:
	InitiateSearchMixin(AtomSpace*);

	/**
	 * Called to perform the actual search. This makes some default
	 * assumptions about the kind of things that might be matched,
	 * in order to drive a reasonably-fast search.
	 */
	virtual void set_pattern(const Variables&, const Pattern&);
	virtual bool perform_search(PatternMatchCallback&);

	virtual void push(void);
	virtual void pop(void);
	virtual void next_connections(const GroundingMap&);
	virtual bool get_next_clause(PatternTermPtr&, PatternTermPtr&);

	std::string to_string(const std::string& indent=empty_string) const;

protected:

	NameServer& _nameserver;

	const Variables* _variables;
	const Pattern* _pattern;
	bool _recursing;

	PatternTermPtr _root;
	PatternTermPtr _starter_term;
	HandleSeq _search_set;

	struct Choice
	{
		PatternTermPtr clause;
		PatternTermPtr start_term;
		HandleSeq search_set;
	};
	PatternTermPtr _curr_clause;
	std::vector<Choice> _start_choices;

	virtual Handle find_starter(const PatternTermPtr&,
	                            size_t&, PatternTermPtr&, size_t&);
	virtual Handle find_starter_recursive(const PatternTermPtr&,
	                                      size_t&, PatternTermPtr&, size_t&);
	virtual Handle find_thinnest(const PatternTermSeq&,
	                             PatternTermPtr&, PatternTermPtr&);
	virtual void find_rarest(const PatternTermPtr&, PatternTermPtr&,
	                         size_t&, Quotation quotation=Quotation());

	const PatternTermSeq& get_clause_list(void);

	bool setup_neighbor_search(const PatternTermSeq&);
	bool setup_no_search(void);
	bool setup_deep_type_search(const PatternTermSeq&);
	bool setup_link_type_search(const PatternTermSeq&);
	bool setup_variable_search(const PatternTermSeq&);

	bool disjoin_search(PatternMatchCallback&, const PatternTermSeq&);
	bool conjoin_search(PatternMatchCallback&, const PatternTermSeq&);
	bool legacy_search(PatternMatchCallback&);
	bool choice_loop(PatternMatchCallback&, const std::string);
	bool search_loop(PatternMatchCallback&, const std::string);

	static PatternTermPtr term_of_handle(const Handle&, const PatternTermPtr&);
	static PatternTermSeq term_choices_of_handle(const Handle&, const PatternTermPtr&);

	// --------------------------------------------
	// Methods and state that select the next clause to be grounded.
	typedef std::set<PatternTermPtr> IssuedSet;

	// Set of clauses for which a grounding is currently being attempted.
	IssuedSet _issued;     // stacked on _issued_stack
	std::stack<IssuedSet> _issued_stack;

	typedef std::vector<Choice> ChoiceList;
	ChoiceList _next_choices;
	std::stack<ChoiceList> _choice_stack;

	Handle get_glob_embedding(const GroundingMap&, const Handle&);
	bool get_next_thinnest_clause(const GroundingMap&, bool, bool);
	unsigned int thickness(const PatternTermPtr&, const HandleSet&);

	AtomSpace *_as;
};

// Primarily for gdb debugging, see
// https://wiki.opencog.org/w/Development_standards#Pretty_Print_OpenCog_Objects
std::string oc_to_string(const InitiateSearchMixin& iscb,
                         const std::string& indent=empty_string);

} // namespace opencog

#endif // _OPENCOG_INITIATE_SEARCH_H
