/*
 * ForeignAST.cc
 *
 * Copyright (C) 2021 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  October 2021
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

#include "ForeignAST.h"

using namespace opencog;

ForeignAST::ForeignAST(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, FOREIGN_AST))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ForeignAST, got %s", tname.c_str());
	}
}

ForeignAST::ForeignAST(Type t)
	: Link(t)
{
	if (not nameserver().isA(t, FOREIGN_AST))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ForeignAST, got %s", tname.c_str());
	}
}

ForeignAST::ForeignAST(Type t, const std::string& str)
	: Link(t)
{
	_name = str;
}

// ---------------------------------------------------------------

// Content-based comparison.
bool ForeignAST::operator==(const Atom& other) const
{
	// If other points to this, then have equality.
	if (this == &other) return true;

	// Let Link do most of the work.
	bool linkeq = Link::operator==(other);
	if (not linkeq) return false;

	// Names must match.
	return 0 == _name.compare(ForeignASTCast(other.get_handle())->_name);
}

// ---------------------------------------------------------------

ContentHash ForeignAST::compute_hash() const
{
   ContentHash hsh = Link::compute_hash();
	hsh += std::hash<std::string>()(_name);

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1ULL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	return hsh;
}

/* ===================== END OF FILE ===================== */
