/*
 * EncodeMeTTa.cc
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
std::string MeTTa::prt_metta(const Handle& h)
{
	Type t = h->get_type();
	if (h->is_node())
		return h->get_name() + " ";

	if (TRUE_LINK == t)
		return "true ";

	if (FALSE_LINK == t)
		return "false ";

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
	else if (BOOL_AND_LINK == t)
		rv += "and ";
	else if (BOOL_OR_LINK == t)
		rv += "or ";
	else if (BOOL_NOT_LINK == t)
		rv += "not ";
	else if (VARIABLE_LIST == t)
		rv += "";
	else if (EXECUTION_OUTPUT_LINK == t)
	{
		// Special handling to unbundle any list links.
		const Handle& args = h->getOutgoingAtom(1);
		if (LIST_LINK == args->get_type())
		{
			rv += prt_metta(h->getOutgoingAtom(0));
			for (const Handle& ho: args->getOutgoingSet())
				rv += prt_metta(ho);

			// Remove trailing blank space.
			rv.pop_back();
			return rv + ") ";
		}
	}
	else if (LIST_LINK == t)
		rv += "";
	else
		rv += nameserver().getTypeName(t) + " ";
		// throw SyntaxException(TRACE_INFO, "Unknown link type");

	for (const Handle& ho: h->getOutgoingSet())
		rv += prt_metta(ho);

	// Remove trailing blank space.
	rv.pop_back();
	rv += ") ";
	return rv;
}

/* ===================== END OF FILE ===================== */
