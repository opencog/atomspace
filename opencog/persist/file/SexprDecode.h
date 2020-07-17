/*
 * SexprDecode.h
 * Fast Decoding of Atomese in s-expression format.
 *
 * Copyright (C) 2020 Linas Vepstas
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

#ifndef _SEXPR_ECODE_H
#define _SEXPR_ECODE_H

#include <string>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class SexprDecode
{
public:
	static int get_next_expr(const std::string&,
                            size_t& l, size_t& r, size_t line_cnt);
	static Handle decode_atom(const std::string& s,
                             size_t l, size_t r, size_t line_cnt);

	static Handle decodeAtom(std::string& s) {
		size_t junk = 0;	
		return decode_atom(s, junk);
	}

	static Handle decode_atom(std::string& s, size_t& pos) {
		size_t junk = s.length() - pos;
		return decode_atom(s, pos, junk, 0);
	}
	static ValuePtr decode_value(std::string&, size_t&);
	static void decode_alist(Handle&, std::string&);
};

/** @}*/
} // namespace opencog

#endif // _SEXPR_ECODE_H
