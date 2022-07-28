/*
 * opencog/atoms/reduct/ImpulseLink.cc
 *
 * Copyright (C) 2015,2018,2022 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include "NumericFunctionLink.h"
#include "ImpulseLink.h"

using namespace opencog;

ImpulseLink::ImpulseLink(const HandleSeq&& oset, Type t)
    : FunctionLink(std::move(oset), t)
{
	init();
}

void ImpulseLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, IMPULSE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an ImpulseLink");
}

// ===========================================================

/// Convert a BoolValue into a FloatValue.
ValuePtr ImpulseLink::execute(AtomSpace* as, bool silent)
{
	ValuePtr bptr = NumericFunctionLink::get_value(as, silent,
		_outgoing[0]);

	if (nameserver().isA(bptr->get_type(), BOOLEAN_VALUE))
	{
		const std::vector<bool>& bv = bptr->get_value();
		std::vector<double> fv(bv.size());
		for (bool b : bv)
			fv.emplace_back(b? 1.0 : 0.0);
		return createFloatValue(fv);
	}

	return shared_from_this();
}

// ===========================================================
