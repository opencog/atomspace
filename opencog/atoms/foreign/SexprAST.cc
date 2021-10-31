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

	// If we are here, l points to the open-paren.
	l++;
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
printf("enter %lu %lu c=%c s=%s\n", l, r, sexpr[l], sexpr.substr(l).c_str());

	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// If another opening paren, recurse
	if ('(' == sexpr[l])
	{
printf("---- duuude recurse at %lu %s\n", l, sexpr.substr(l).c_str());
		l++;
		HandleSeq oset;
		while (std::string::npos != r)
		{
			Handle h(get_next_expr(sexpr, l, r));
			oset.emplace_back(h);
		}

		// l will be pointing at the trailing paren, so move past that.
		l++;
		r = 0;
		Handle h = HandleCast(createSexprAST(std::move(oset)));
printf("---- duuude done recurse whats left=%lu >>%s\n", l, sexpr.substr(l).c_str());
		return h;
	}

	// If its a literal, we are done.
	r = sexpr.find_first_of(" \t\n)", l);
	if (std::string::npos == r)
		throw SyntaxException(TRACE_INFO, "Failed to find closing parenthesis");

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
	l = sexpr.find_first_not_of(" \t\n", r);
	if (')' == sexpr[l])
	{
		l++;
		r = std::string::npos;
	}
	return HandleCast(createSexprAST(tok));
}

// ---------------------------------------------------------------

std::string SexprAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return indent + "(SexprAST \"" + _name + "\") ; " + id_to_string();

	std::string rv = indent + "(SexprAST\n";
	for (const Handle& h: _outgoing)
		rv += h->to_string(indent + "  ") + "\n";

	rv += indent + ") ; " + id_to_string();
	return rv;
}

std::string SexprAST::to_short_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return _name;

	std::string rv = "(";
	for (const Handle& h: _outgoing)
		rv += h->to_short_string() + " ";

	rv[rv.size()-1] = ')';

	// Debugging print
	rv += "\n" + to_string(";");
	return rv;
}

// ---------------------------------------------------------------

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

// ---------------------------------------------------------------

ContentHash SexprAST::compute_hash() const
{
   ContentHash hsh = Link::compute_hash();
	hsh += std::hash<std::string>()(_name);

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1ULL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	return hsh;
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
