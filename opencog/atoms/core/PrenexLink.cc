/*
 * PrenexLink.cc
 *
 * Copyright (C) 2017 Linas Vepstas
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

#include <string>

#include <opencog/util/mt19937ar.h>
#include <opencog/util/random.h>
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atomutils/TypeUtils.h>
#include <opencog/atomutils/FindUtils.h>

#include "PrenexLink.h"

using namespace opencog;

void PrenexLink::init(void)
{
	Type t = get_type();
	if (not classserver().isA(t, PRENEX_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a PrenexLink, got %s", tname.c_str());
	}
}

PrenexLink::PrenexLink(const Handle& vars, const Handle& body)
	: RewriteLink(HandleSeq({vars, body}), PRENEX_LINK)
{
	init();
}

PrenexLink::PrenexLink(const HandleSeq& oset, Type t)
	: RewriteLink(oset, t)
{
	if (skip_init(t)) return;
	init();
}

PrenexLink::PrenexLink(const Link &l)
	: RewriteLink(l)
{
	if (skip_init(l.get_type())) return;
	init();
}

/* ================================================================= */

Handle PrenexLink::beta_reduce(const HandleSeq& vals) const
{
	// XXX this is wrong.
	return RewriteLink::beta_reduce(vals);
}

/* ================================================================= */

DEFINE_LINK_FACTORY(PrenexLink, PRENEX_LINK);

/* ===================== END OF FILE ===================== */
