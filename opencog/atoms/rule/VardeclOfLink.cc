/*
 * VardeclOfLink.cc
 *
 * Copyright (C) 2015, 2018, 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atomspace/AtomSpace.h>
#include "RuleLink.h"
#include "VardeclOfLink.h"

using namespace opencog;

VardeclOfLink::VardeclOfLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, VARDECL_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an VardeclOfLink, got %s", tname.c_str());
	}
	init();
}

void VardeclOfLink::init(void)
{
	if (1 != _outgoing.size())
		throw SyntaxException(TRACE_INFO, "Expecting only one argument!");

	const Handle& ho = _outgoing[0];
	if (not ho->is_type(RULE_LINK))
		throw SyntaxException(TRACE_INFO, "Expecting a RuleLink!");

	const RuleLinkPtr& rule = RuleLinkCast(ho);
	_vardecl = rule->get_vardecl();

	// If we found it, we are home-free.
	if (_vardecl) return;

	// Ask Variables::get_vardecl() to do the heavy lifting.
	_vardecl = rule->get_variables().get_vardecl();
}

// ---------------------------------------------------------------

ValuePtr VardeclOfLink::execute(AtomSpace* as, bool silent)
{
	// We need tp as->add, if Variables::get_varecl() was called.
	return as->add_atom(_vardecl);
}

DEFINE_LINK_FACTORY(VardeclOfLink, VARDECL_OF_LINK)

/* ===================== END OF FILE ===================== */
