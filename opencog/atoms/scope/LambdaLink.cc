/*
 * LambdaLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#include <opencog/atoms/base/ClassServer.h>

#include "LambdaLink.h"

using namespace opencog;

LambdaLink::LambdaLink(const Handle& vars, const Handle& body)
	: PrenexLink(HandleSeq({vars, body}), LAMBDA_LINK)
{
}

LambdaLink::LambdaLink(const HandleSeq&& oset, Type t)
	: PrenexLink(std::move(oset), t)
{
	if (not nameserver().isA(t, LAMBDA_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a LambdaLink, got %s", tname.c_str());
	}
}

DEFINE_LINK_FACTORY(LambdaLink, LAMBDA_LINK)

/* ===================== END OF FILE ===================== */
