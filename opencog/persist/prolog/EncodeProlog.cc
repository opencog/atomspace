/*
 * EncodeProlog.cc
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

#include "Prolog.h"

using namespace opencog;

// ---------------------------------------------------------------

std::string Prolog::prt_datalog(const Handle& h, bool rec)
{
	Type t = h->get_type();
	if (h->is_node())
		return h->get_name();

	std::string rv = "";
	if (DATALOG_AST == t)
	{
		rv += "(DatalogAst \"";
		for (const Handle& ho: h->getOutgoingSet())
			rv += prt_datalog(ho, false) + " ";
		rv.pop_back();
		rv += "\")";
	}
	else if (EVALUATION_LINK == t)
	{
		// First comes a PredicateNode
		rv += prt_datalog(h->getOutgoingAtom(0), true);

		rv += "(";
		// Next comes a ListLink
		const Handle& ll = h->getOutgoingAtom(1);
		for (const Handle& ho : ll->getOutgoingSet())
		{
			rv += prt_datalog(ho, true);
			rv += ", ";
		}
		rv.pop_back();
		rv.pop_back();
		rv += ")";
		if (not rec) rv += ".";
	}
	else if (IMPLICATION_LINK == t)
	{
		if (2 != h->get_arity())
			throw SyntaxException(TRACE_INFO, "Bad arity for implication.");

		const Handle& premis = h->getOutgoingAtom(0);
		const Handle& conclu = h->getOutgoingAtom(1);
		rv += prt_datalog(conclu, true);
		rv += " :- ";

		if (AND_LINK == premis->get_type())
		{
			for (const Handle& po : premis->getOutgoingSet())
				rv += prt_datalog(po, true) + ", ";
			rv.pop_back();
			rv.pop_back();
			rv += ")";
		}
		else
			rv += prt_datalog(premis, true);

		rv += ".";
	}
	else
		throw SyntaxException(TRACE_INFO, "Unknown exprssion.");

	return rv;
}

/* ===================== END OF FILE ===================== */
