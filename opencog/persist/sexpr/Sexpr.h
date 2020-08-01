/*
 * Sexpr.h
 * Encoding and Decoding of Atomese in s-expression format.
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

class Sexpr
{
public:
	/// Decode the s-expression containing an atom, starting at
	/// location `pos`. Return the Atom, and update `pos` to point
	/// just past the end of the trailing parenthesis.
	static Handle decode_atom(const std::string& s, size_t& pos)
	{
		size_t start = pos;
		size_t end = s.length();
		get_next_expr(s, start, end, 0);
		pos = end;
		return decode_atom(s, start, end, 0);
	}

	static Handle decode_atom(const std::string& s) {
		size_t junk = 0;
		return decode_atom(s, junk);
	}

	static ValuePtr decode_value(const std::string&, size_t&);
	static Type decode_type(const std::string& s, size_t& pos);

	static void decode_slist(Handle&, const std::string&, size_t&);
	static void decode_alist(Handle&, const std::string&, size_t&);
	static void decode_alist(Handle& h, const std::string& s) {
		size_t junk = 0;
		decode_alist(h, s, junk);
	}

	// -------------------------------------------
	// API more suitable to very long, file-driven I/O.
	static int get_next_expr(const std::string&,
                            size_t& l, size_t& r, size_t line_cnt);
	static Handle decode_atom(const std::string& s,
                             size_t l, size_t r, size_t line_cnt);

	static ValuePtr add_atoms(AtomSpace*, const ValuePtr&);

	// -------------------------------------------
	// Encoding functions
	static std::string encode_atom(const Handle&);
	static std::string encode_value(const ValuePtr&);
	static std::string encode_atom_values(const Handle&);
};

/** @}*/
} // namespace opencog

#endif // _SEXPR_ECODE_H
