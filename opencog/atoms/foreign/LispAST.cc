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

#include <opencog/persist/metta/MeTTa.h>
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

LispAST::LispAST(const HandleSeq&& oset, const std::string&& sexpr)
	: SexprAST(std::move(oset), LISP_AST)
{
	init();
	_name = sexpr;
}

LispAST::LispAST(const std::string& sexpr)
	: SexprAST(LISP_AST)
{
	SexprAST::parse(sexpr);

	// Parser fails to wrap simple tokens. Try again.
	if (0 == _outgoing.size())
	{
		size_t l=0, r=0;
		_outgoing.emplace_back(next_expr(_name, l, r));
	}
}

Handle LispAST::next_expr(const std::string& expr, size_t& l, size_t& r)
{
	return MeTTa::next_expr(expr, l, r);
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
	if (0 == indent.size())
		return _name + "\n" + to_short_string(";") + "\n";

	// Debugging print
	if (0 == _outgoing.size()) // this should never happen!
		return _name + "XXX-borken";

	std::string rv = "";
	for (const Handle& h: _outgoing)
	{
		if (LISP_AST == h->get_type())
			rv += h->to_short_string("xx") + " ";
		else
			rv += indent + h->to_short_string(indent);
	}

	return rv;
}

// ---------------------------------------------------------------
// Custom factory, because its a hermaphrodite. The ForgeinAST will
// pass us a string, behaving like a node, which we parse into an
// expression tree.

Handle LispAST::factory(const Handle& base)
{
	/* If it's castable, nothing to do. */
	if (LispASTCast(base)) return base;

	if (0 == base->get_arity())
		return HandleCast(createLispAST(std::move(base->get_name())));

	return HandleCast(createLispAST(
		std::move(base->getOutgoingSet()),
		std::move(prt_metta(base))));
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init_lispast_factory(void)
{
	classserver().addFactory(LISP_AST, &LispAST::factory);
}

/* ===================== END OF FILE ===================== */
