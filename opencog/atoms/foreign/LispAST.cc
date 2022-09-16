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

LispAST::LispAST(const HandleSeq&& oset, const std::string&& sexpr)
	: SexprAST(std::move(oset), LISP_AST)
{
	init();
	_name = sexpr;
}

LispAST::LispAST(const std::string& sexpr)
	: SexprAST(LISP_AST)
{
	parse(sexpr);
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
// Custom factory, because its a hermaphrodite. The ForgeinAST will
// pass us a string, behaving like a node, which we parse into an
// expression tree.

Handle LispAST::factory(const Handle& base)
{
	/* If it's castable, nothing to do. */
	if (LispASTCast(base)) return base;

	if (0 < base->get_arity())
	{
		return HandleCast(createLispAST(
			std::move(base->getOutgoingSet()),
			std::move(prt_metta(base))));
	}

	return HandleCast(createLispAST(std::move(base->get_name())));
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init_lispast_factory(void)
{
	classserver().addFactory(LISP_AST, &LispAST::factory);
}

/* ===================== END OF FILE ===================== */
