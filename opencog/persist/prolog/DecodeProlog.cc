/*
 * DecodeProlog.cc
 *
 * Copyright (C) 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  September 2022
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>

#include "Prolog.h"

using namespace opencog;

/// Parse one or more clauses, e.g.
/// foo(X) :- bar(X). bing(bang,bong). food(pizza).
HandleSeq Prolog::parse(const std::string& sexpr, size_t& l, size_t& r)
{
	HandleSeq clauses;
	while (std::string::npos != l)
	{
		Handle h(get_next_expr(sexpr, l, r));
		clauses.emplace_back(h);
printf("duuude made %s\n", h->to_short_string().c_str());

		if (std::string::npos == l)
			throw SyntaxException(TRACE_INFO, "Expecting period at end.");

		l++;
		l = sexpr.find_first_not_of(" \t\n", l);
	}

	return clauses;
}

// ---------------------------------------------------------------

// Lower-case tokens, or quoted tokens, are concept nodes.
// Upper-case tokens are Variables.
static Handle make_tok(const std::string& tok)
{
	char c = tok[0];

	// Variables begin with an underscore, or uppercase letter.
	if ('_' == c or isupper(c))
		return HandleCast(createNode(VARIABLE_NODE, tok));

	return HandleCast(createNode(CONCEPT_NODE, tok));
}

// Parse factual assertions such as
// likes(john, mary) or food(pizza)
// but also fragments of clauses, such as
// likes(X,Y)
static Handle get_fact(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	r = sexpr.find_first_of("( \t\n", l+1);
	const std::string& spred = sexpr.substr(l, r-l);
	Handle pred = createNode(PREDICATE_NODE, std::move(spred));

	l = sexpr.find_first_not_of(" \t\n", r);
	if (std::string::npos == l or '(' != sexpr[l])
		throw SyntaxException(TRACE_INFO, "Expecting open-paren");

	l++; // step past open paren

	HandleSeq clist;
	while (std::string::npos != l)
	{
		// Matching quotes, if quoted.
		if ('\'' == sexpr[l])
		{
			r = sexpr.find('\'', l+1);
			if (std::string::npos != r) r++;
			else
				throw SyntaxException(TRACE_INFO, "Unbalanced quotes");
		}
		else
			r = sexpr.find_first_of(",) \t\n", l);
		const std::string& litrl = sexpr.substr(l, r-l);

		if ('\'' == litrl[0] or std::string::npos == litrl.find('('))
			clist.emplace_back(make_tok(litrl));
		else
		{
			// The literal is yet another fact. Yuck.
			// Go and nest them. (For now? Maybe fix later?)
			clist.emplace_back(get_fact(sexpr, l, r));
			r = l;
		}

		if (')' == sexpr[r]) break;
		l = sexpr.find_first_not_of(", \t\n", r);
	}
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Expecting close-paren");

	l = sexpr.find_first_not_of(" \t\n", r+1);
	r = std::string::npos;

	Handle evl = createLink(EVALUATION_LINK,
		pred, createLink(std::move(clist), LIST_LINK));
	return evl;
}

// ---------------------------------------------------------------
//
// Stub for handling queries.
// There are several basic types:
// 1) ?- likes(alice,john).
//    Return true of false; tests for presence. (SatisfactionLink) Awkward!
// 2) ?- likes(alice, _).
//    Return true of false;, alice likes someone, but its indeterminate.
// 3) ?- likes(alice, Who).
//    Return grounding of Who. Standard GetLink.
//    Caution: this may require chaining to solve. Ugh.
// 4) Unification, chaining, ... etc.

static Handle formulate_query(const std::string& sexpr, size_t& l, size_t &r)
{
#if 0
	// Skip past the ?- at the start.
	l = sexpr.find_first_not_of(" \t\n", l+2);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Lets see if we can come up with one of the simple cases.
	// Looking for case 3.
	Handle fac = get_fact(sexpr, l, r);
#endif

	throw SyntaxException(TRACE_INFO, "Queries are not (yet) supported!");
}

// ---------------------------------------------------------------
// Parse clauses such as
// likes(john, mary).
// or
// child(X,Y) :- parent(Y,X).
// Clauses must be terminated by a period.
Handle Prolog::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Maybe it's a query?
	if ('?' == sexpr[l])
		return formulate_query(sexpr, l, r);

	// Not a query. Get first part of clause.
	Handle fac = get_fact(sexpr, l, r);

	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Expecting terminating period");

	// Just a single fact.
	if ('.' == sexpr[l])
		return fac;

	// Is it a Horn clause?
	if (sexpr.substr(l, 2) != ":-")
		throw SyntaxException(TRACE_INFO, "Badly formed expression");

	l += 2;

	// Loop over conjunctions.
	HandleSeq premis;
	while (std::string::npos != l)
	{
		Handle fac = get_fact(sexpr, l, r);
		premis.emplace_back(fac);
		if (std::string::npos == l)
			throw SyntaxException(TRACE_INFO, "Badly formed expression");
		if ('.' == sexpr[l]) break;
		if (',' != sexpr[l])
			throw SyntaxException(TRACE_INFO, "Badly formed expression");
		l++;
	}

	// Not a conjunction, just a single term.
	if (1 == premis.size())
		return HandleCast(createLink(IMPLICATION_LINK, premis[0], fac));

	// Conjunction of multiple terms.
	Handle imp = createLink(IMPLICATION_LINK,
		createLink(std::move(premis), AND_LINK), fac);
	return imp;
}

/* ===================== END OF FILE ===================== */
