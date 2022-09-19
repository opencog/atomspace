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

// Parse expressions such as
// likes(John, Mary).
Handle DatalogAST::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	r = sexpr.find_first_of("( \t\n", l+1);
	const std::string& spred = sexpr.substr(l, r-l);
printf("duuude got pred >>%s<<\n", spred.c_str());
	Handle pred = createNode(PREDICATE_NODE, std::move(spred));

	l = sexpr.find_first_not_of(" \t\n", r);
	if (std::string::npos == l or '(' != sexpr[l])
		throw SyntaxException(TRACE_INFO, "Expecting open-paren");

	l++; // step past open paren

	HandleSeq clist;
	while (std::string::npos != l and '.' != sexpr[l])
	{
		r = sexpr.find_first_of(",) \t\n", l);
		const std::string& cept = sexpr.substr(l, r-l);
printf("duuude got concept >>%s<<\n", cept.c_str());

		clist.emplace_back(createNode(CONCEPT_NODE, cept));
		if (')' == sexpr[r]) break;
		l = sexpr.find_first_not_of(", \t\n", r);
	}

	l = sexpr.find_first_of(".", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Expecting terminating period");

	l++;
	r = sexpr.find_first_not_of(" \t\n", l);

	Handle evl = createLink(EVALUATION_LINK,
		pred, createLink(std::move(clist), LIST_LINK));
	return evl;
}

/* ===================== END OF FILE ===================== */
