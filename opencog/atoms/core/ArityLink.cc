/*
 * ArityLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/core/NumberNode.h>

#include "ArityLink.h"

using namespace opencog;

ArityLink::ArityLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, ARITY_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ArityLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return the Arity, as a NumberNode.  Contrast this with
/// ArityValueOf, which returns a FloatValue, instead.
ValuePtr ArityLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = 0;
	for (Handle h : _outgoing)
	{
		if (h->is_executable())
		{
			ValuePtr pap(h->execute(as, silent));
			Type ptype = pap->get_type();

			if (nameserver().isA(ptype, FLOAT_VALUE))
			{
				const std::vector<double>& dvec(FloatValueCast(h)->value());
				ary += dvec.size();
				continue;
			}

			if (nameserver().isA(ptype, STRING_VALUE))
			{
				const std::vector<std::string>& svec(StringValueCast(h)->value());
				ary += svec.size();
				continue;
			}

			if (nameserver().isA(ptype, LINK_VALUE))
			{
				const std::vector<ValuePtr>& pvec(LinkValueCast(h)->value());
				ary += pvec.size();
				continue;
			}

			if (not h->is_atom())
				continue;

			// Fall through
			h = HandleCast(pap);
		}

		if (h->is_link())
			ary += h->get_arity();
		else if (NUMBER_NODE == h->get_type())
		{
			const std::vector<double>& dvec(NumberNodeCast(h)->value());
			ary += dvec.size();
		}
		else
			ary++;
	}

	return ValuePtr(createNumberNode(ary));
}

DEFINE_LINK_FACTORY(ArityLink, ARITY_LINK)

/* ===================== END OF FILE ===================== */
