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
}

SexprAST::SexprAST(Type t)
	: ForeignAST(t)
{
}

SexprAST::SexprAST(const std::string& sexpr)
	: ForeignAST(SEXPR_AST)
{
	parse(sexpr);
}

void SexprAST::parse(const std::string& sexpr)
{
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

	// If we are here, l points to the open-paren.
	l++; // step past open-paren
	size_t r = 0;
	while (std::string::npos != r)
	{
		Handle h(next_expr(sexpr, l, r));
		_outgoing.emplace_back(h);
	}
}

// ---------------------------------------------------------------

Handle SexprAST::next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	return get_next_expr(sexpr, l, r);
}

Handle SexprAST::get_next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// If another opening paren, recurse
	if ('(' == sexpr[l])
	{
		l++; // step past open-paren
		HandleSeq oset;
		while (std::string::npos != r)
		{
			Handle h(get_next_expr(sexpr, l, r));
			oset.emplace_back(h);
		}

		// l will be pointing at the trailing paren, so move past that.
		l++;
		l = sexpr.find_first_not_of(" \t\n", l);
		r = 0;
		if (')' == sexpr[l]) r = std::string::npos;
		return HandleCast(createSexprAST(std::move(oset)));
	}

	// If its a literal, we are done.
	r = sexpr.find_first_of(" \t\n)", l);
	if (std::string::npos == r)
		throw SyntaxException(TRACE_INFO, "Failed to find closing parenthesis");

	// Found the closing paren; just wrap the string.
	if (')' == sexpr[r])
	{
		const std::string& tok = sexpr.substr(l, r-l);
		l = sexpr.find_first_not_of(" \t\n", r);
		r = std::string::npos;
		return HandleCast(createSexprAST(tok));
	}

	// If we are here, r points to whitespace, and l points to the first
	// thing after the initial opening paren.
	while ('(' == sexpr[r-1]) r--;
	const std::string& tok = sexpr.substr(l, r-l);
	l = sexpr.find_first_not_of(" \t\n", r);
	if (')' == sexpr[l])
		r = std::string::npos;

	return HandleCast(createSexprAST(tok));
}

// ---------------------------------------------------------------

std::string SexprAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return indent + "(SexprAst \"" + _name + "\") ; " + id_to_string();

	std::string rv = indent + "(SexprAst\n";
	for (const Handle& h: _outgoing)
		rv += h->to_string(indent + "  ") + "\n";

	rv += indent + ") ; " + id_to_string();
	return rv;
}

std::string SexprAST::to_short_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
	{
		if (0 != indent.size()) return _name;

		return _name + "\n" + to_string(";") + "\n";
	}

	std::string rv = "(";
	for (const Handle& h: _outgoing)
	{
		if (SEXPR_AST == h->get_type())
			rv += h->to_short_string("xx") + " ";
		else
			rv += "(atomese " + h->to_short_string("") + ") ";
	}

	rv[rv.size()-1] = ')';

	// Debugging print
	if (0 == indent.size()) rv += "\n" + to_string(";") + "\n";
	return rv;
}

// ---------------------------------------------------------------
// Custom factory, because its a hermaphrodite. The ForgeinAST will
// pass us a string, behaving like a node, which we parse into an
// expression tree.

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
