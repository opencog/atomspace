/*
 * LispASTPrinter.cc
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

// ---------------------------------------------------------------

/// Convert Atomese to MeTTa-style strings
std::string LispAST::prt_metta(const Handle& h)
{
	Type t = h->get_type();
	if (h->is_node())
		return h->get_name() + " ";

	std::string rv = "(";
	if (LISP_AST == t)
		rv += "LispAst ";
	else if (DEFINE_LINK == t)
		rv += "= ";
	else if (LAMBDA_LINK == t)
		rv += "λ ";
	else if (PLUS_LINK == t)
		rv += "+ ";
	else if (MINUS_LINK == t)
		rv += "- ";
	else if (TIMES_LINK == t)
		rv += "* ";
	else if (DIVIDE_LINK == t)
		rv += "/ ";
	else if (LESS_THAN_LINK == t)
		rv += "< ";
	else if (GREATER_THAN_LINK == t)
		rv += "> ";
	else if (COND_LINK == t)
		rv += "if ";
	else if (VARIABLE_LIST == t)
		rv += "";
	else if (EXECUTION_OUTPUT_LINK == t)
		rv += "";
	else if (LIST_LINK == t)
		rv += "";
	else
		rv += nameserver().getTypeName(t) + " ";
		// throw SyntaxException(TRACE_INFO, "Unknown link type");

	for (const Handle& ho: h->getOutgoingSet())
		rv += prt_metta(ho);

	rv += ") ";
	return rv;
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
	if (0 == _outgoing.size())
		return _name;

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

/* ===================== END OF FILE ===================== */
