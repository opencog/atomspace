/*
 * LispAST.cc
 *
 * Copyright (C) 2021, 2022 Linas Vepstas
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

#include "LispAST.h"

using namespace opencog;

void LispAST::init()
{
	if (not nameserver().isA(_type, LISP_AST))
	{
		const std::string& tname = nameserver().getTypeName(_type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an LispAST, got %s", tname.c_str());
	}
}

LispAST::LispAST(const HandleSeq&& oset, Type t)
	: SexprAST(std::move(oset), t)
{
	init();
}

LispAST::LispAST(const std::string& sexpr)
	: SexprAST(LISP_AST)
{
	parse(sexpr);
}

// ---------------------------------------------------------------

Handle LispAST::make_tok(const std::string& tok)
{
printf("duuude tok=%s\n", tok.c_str());
	return HandleCast(createLispAST(tok));
}

Handle LispAST::make_seq(const HandleSeq&& oset)
{
	return HandleCast(createLispAST(std::move(oset)));
}

// ---------------------------------------------------------------

std::string LispAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return indent + "(LispAst \"" + _name + "\") ; " + id_to_string();

	std::string rv = indent + "(LispAst\n";
	for (const Handle& h: _outgoing)
		rv += h->to_string(indent + "  ") + "\n";

	rv += indent + ") ; " + id_to_string();
	return rv;
}

std::string LispAST::to_short_string(const std::string& indent) const
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

// Content-based comparison.
bool LispAST::operator==(const Atom& other) const
{
	// If other points to this, then have equality.
	if (this == &other) return true;

	// Let Link do most of the work.
	bool linkeq = Link::operator==(other);
	if (not linkeq) return false;

	// Names must match.
	return 0 == _name.compare(LispASTCast(other.get_handle())->_name);
}

// ---------------------------------------------------------------

ContentHash LispAST::compute_hash() const
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

Handle LispAST::factory(const Handle& base)
{
	/* If it's castable, nothing to do. */
	if (LispASTCast(base)) return base;

	if (0 < base->get_arity())
		return HandleCast(createLispAST(std::move(base->getOutgoingSet())));

	return HandleCast(createLispAST(std::move(base->get_name())));
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init_lispast_factory(void)
{
	classserver().addFactory(LISP_AST, &LispAST::factory);
}

/* ===================== END OF FILE ===================== */
