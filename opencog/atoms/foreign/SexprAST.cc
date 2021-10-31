/*
 * SexprAST.cc
 *
 * Copyright (C) 2021 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  October 2021
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

#include "SexprAST.h"

using namespace opencog;

SexprAST::SexprAST(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, SEXPR_AST))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SexprAST, got %s", tname.c_str());
	}

	for (const Handle& h: _outgoing)
		if (not nameserver().isA(h->get_type(), SEXPR_AST))
			throw InvalidParamException(TRACE_INFO,
				"Expecting an SexprAST, got %s", h->to_string().c_str());
}

SexprAST::SexprAST(const std::string& sexpr)
	: Link(SEXPR_AST)
{
	size_t l = sexpr.find_first_not_of(" \t\n");

	if ('(' != sexpr[l])
	{
		// If its a literal, we are done.
		size_t r = sexpr.find_first_of(" \t\n", l);
		if (std::string::npos == r)
		{
			_name = sexpr.substr(l);
			return;
		}
		size_t l2 = sexpr.find_first_not_of(" \t\n", r+1);
		if (std::string::npos == l2)
		{
			_name = sexpr.substr(l, r-l);
			return;
		}
	}

	// Its a paren. Loop
}

// ---------------------------------------------------------------

Handle SexprAST::get_next_expr(const std::string& s, size_t& l, size_t &r)
{

	return Handle();
}

// ---------------------------------------------------------------

std::string SexprAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return _name;

	return "foobar";
}

DEFINE_LINK_FACTORY(SexprAST, SEXPR_AST)

/* ===================== END OF FILE ===================== */
