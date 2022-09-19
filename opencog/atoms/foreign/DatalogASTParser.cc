/*
 * DatalogASTParser.cc
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

#include "DatalogAST.h"

using namespace opencog;

void DatalogAST::parse(const std::string& sexpr)
{
	size_t l = 0;
	size_t r = 0;
	while (std::string::npos != r)
	{
		Handle h(get_next_expr(sexpr, l, r));
		_outgoing.emplace_back(h);
printf("duuude made %s\n", h->to_short_string().c_str());
	}
}

// ---------------------------------------------------------------

// Lower-case tokens, or quoted tokens, are concept nodes.
// Upper-case tokens are Variables.
static Handle make_tok(const std::string& tok)
{
	char c = tok[0];
	if ('\'' == c or islower(c))
		return HandleCast(createNode(CONCEPT_NODE, tok));

	return HandleCast(createNode(VARIABLE_NODE, tok));
}

// Parse factual assertions such as
// likes(john, mary) or food(pizza)
// but also fragments of clauses, such as
// likes(X,Y)
Handle get_fact(const std::string& sexpr, size_t& l, size_t &r)
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
		const std::string& cept = sexpr.substr(l, r-l);
		clist.emplace_back(make_tok(cept));

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
// Parse expressions such as
// likes(john, mary) or food(pizza)
Handle DatalogAST::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
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

	if (1 == premis.size())
		return HandleCast(createLink(IMPLICATION_LINK, premis[0], fac));

	Handle imp = createLink(IMPLICATION_LINK,
		createLink(std::move(premis), AND_LINK), fac);
	return imp;
}

/* ===================== END OF FILE ===================== */
