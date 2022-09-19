/*
 * DatalogASTPrinter.cc
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

// ---------------------------------------------------------------

std::string DatalogAST::prt_datalog(const Handle& h)
{
	Type t = h->get_type();
	if (h->is_node())
		return h->get_name();

	std::string rv = "";
	if (DATALOG_AST == t)
	{
		rv += "(DatalogAst \"";
		for (const Handle& ho: h->getOutgoingSet())
			rv += prt_datalog(ho);
		rv += "\")";
	}
	else if (EVALUATION_LINK == t)
	{
		// First comes a PredicateNode
		rv += prt_datalog(h->getOutgoingAtom(0));

		rv += "(";
		// Next comes a ListLink
		const Handle& ll = h->getOutgoingAtom(1);
		for (const Handle& ho : ll->getOutgoingSet())
		{
			rv += prt_datalog(ho);
			rv += ", ";
		}
		rv.pop_back();
		rv.pop_back();
		rv += ").";
	}
	else if (IMPLICATION_LINK == t)
	{
		if (2 != h->get_arity())
			throw SyntaxException(TRACE_INFO, "Bad arity for implication.");

		const Handle& premis = h->getOutgoingAtom(0);
		const Handle& conclu = h->getOutgoingAtom(1);
		rv += prt_datalog(conclu);
		rv += " :- ";

		if (AND_LINK == premis->get_type())
		{
			for (const Handle& po : premis->getOutgoingSet())
				rv += prt_datalog(po) + ", ";
			rv.pop_back();
			rv.pop_back();
			rv += ")";
		}
		else
			rv += prt_datalog(premis);

		rv += ".";
	}
	else
		throw SyntaxException(TRACE_INFO, "Unknown exprssion.");

	return rv;
}

// ---------------------------------------------------------------

std::string DatalogAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return indent + "(DatalogAst \"" + _name + "\") ; " + id_to_string();

	std::string rv = indent + "(DatalogAst\n";
	for (const Handle& h: _outgoing)
		rv += h->to_string(indent + "  ") + "\n";

	rv += indent + ") ; " + id_to_string();
	return rv;
}

std::string DatalogAST::to_short_string(const std::string& indent) const
{
	if (0 == indent.size())
		return _name + "\n" + to_short_string(";") + "\n";

	// Debugging print
	if (0 == _outgoing.size()) // this should never happen
		return _name + "XXX-borken";

	std::string rv = "";
	for (const Handle& h: _outgoing)
	{
		if (DATALOG_AST == h->get_type())
			rv += h->to_short_string("xx") + " ";
		else
			rv += indent + h->to_short_string(indent);
	}

	return rv;
}

/* ===================== END OF FILE ===================== */
