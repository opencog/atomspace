/*
 * SetValueLink.cc
 *
 * Copyright (C) 2015, 2018, 2020 Linas Vepstas
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
#include <opencog/atoms/core/FunctionLink.h>
#include "SetValueLink.h"

using namespace opencog;

SetValueLink::SetValueLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, SET_VALUE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SetValueLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will attach the indicated value at the indicated
/// key. The attached value is returned.
ValuePtr SetValueLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (3 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting three atoms!");

	// Obtain the value that we will be setting.
	ValuePtr pap = _outgoing[2]->execute(as, silent);

	// We cannot set Values unless we are working with the unique
	// version of the atom that sits in the AtomSpace!
	Handle ah(as->get_atom(_outgoing[0]));
	Handle ak(as->get_atom(_outgoing[1]));
	if (ah and ak)
	{
		ah->setValue(ak, pap);
		return pap;
	}

	// Hmm. shouldn't this be SilentException?
	if (silent)
		throw SilentException();

	throw InvalidParamException(TRACE_INFO,
		"No atom %s or no key %s",
		_outgoing[0]->to_string().c_str(),
		_outgoing[1]->to_string().c_str());
}

DEFINE_LINK_FACTORY(SetValueLink, SET_VALUE_LINK)

/* ===================== END OF FILE ===================== */
