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

void SexprAST::init()
{
	if (not nameserver().isA(_type, SEXPR_AST))
	{
		const std::string& tname = nameserver().getTypeName(_type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SexprAST, got %s", tname.c_str());
	}
}

SexprAST::SexprAST(const HandleSeq&& oset, Type t)
	: ForeignAST(std::move(oset), t)
{
	init();

	for (const Handle& h: _outgoing)
		if (not nameserver().isA(h->get_type(), SEXPR_AST))
			throw InvalidParamException(TRACE_INFO,
				"Expecting an SexprAST, got %s", h->to_string().c_str());
}

SexprAST::SexprAST(const std::string& sexpr)
	: ForeignAST(SEXPR_AST)
{
	parse(sexpr);
}

void SexprAST::parse(const std::string& sexpr)
{
printf("yasss %s\n", sexpr.c_str());
	size_t l = sexpr.find_first_not_of(" \t\n");
	if (std::string::npos == l)
	{
		_name = "";
		return;
	}

	// Look to see if it is a simple literal.
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

	// If we are here, it is not a literal.
	size_t r = 0;
	while (std::string::npos != r)
	{
		Handle h(get_next_expr(sexpr, l, r));
printf("yo %lu %lu handy=%s\n", l, r, h->to_short_string().c_str());
		_outgoing.emplace_back(h);
	}
}

// ---------------------------------------------------------------

Handle SexprAST::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
// printf("enter %lu %lu %s\n", l, r, sexpr.substr(l, r-l).c_str());
printf("enter %lu %lu c=%c %s\n", l, r, sexpr[l], sexpr.c_str());

	if ('(' == sexpr[l]) l++;

	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// If its a literal, we are done.
	r = sexpr.find_first_of(" \t\n)", l);
printf("duuude now its %lu %s\n", r, sexpr.substr(l).c_str());
	if (std::string::npos == r)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Found the closing paren; just wrap the string.
	if (')' == sexpr[r])
	{
		const std::string& tok = sexpr.substr(l, r-l);
		l = r;
		r = std::string::npos;
		return HandleCast(createSexprAST(tok));
	}

	// If we are here, r points to whitespace, and l points to the first
	// thing after the initial opening paren.
	const std::string& tok = sexpr.substr(l, r-l);
printf("duuude toke extractt its %lu %lu %s\n", l, r, tok.c_str());
	l = r;
	r = 0;
	return HandleCast(createSexprAST(tok));
}

// ---------------------------------------------------------------

std::string SexprAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return _name;

	return "foobar";
}

std::string SexprAST::to_short_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return ">>" + _name + "<<";

	return "foobar";
}

// Content-based comparison.
bool SexprAST::operator==(const Atom& other) const
{
	// If other points to this, then have equality.
	if (this == &other) return true;

	// Let Link do most of the work.
	bool linkeq = Link::operator==(other);
	if (not linkeq) return false;

	// Names must match.
	return 0 == _name.compare(SexprASTCast(other.get_handle())->_name);
}

ContentHash SexprAST::compute_hash() const
{
	// hack alert .. for now.
   return Link::compute_hash();
}

Handle SexprAST::factory(const Handle& base)
{
	/* If it's castable, nothing to do. */
	if (SexprASTCast(base)) return base;

	if (0 < base->get_arity())
		return HandleCast(createSexprAST(std::move(base->getOutgoingSet())));

	return HandleCast(createSexprAST(std::move(base->get_name())));
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init_sexprast_factory(void)
{
	classserver().addFactory(SEXPR_AST, &SexprAST::factory);
}

/* ===================== END OF FILE ===================== */
