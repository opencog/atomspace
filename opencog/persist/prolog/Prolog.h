/*
 * Prolog.h
 * Encoding and Decoding of subsets of Atomese as prolog (datalog).
 *
 * Copyright (C) 2020,2022 Linas Vepstas
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

#ifndef _PROLOG_ECODE_H
#define _PROLOG_ECODE_H

#include <string>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class Prolog
{
public:

	// -------------------------------------------
	// API more suitable to very long, file-driven I/O.
	static HandleSeq parse(const std::string&,
                          size_t& l, size_t& r);

	static Handle get_next_expr(const std::string&,
                              size_t& l, size_t& r);

	// -------------------------------------------
	// Encoding functions
	static std::string encode_atom(const Handle&, bool=false);
};

/** @}*/
} // namespace opencog

#endif // _PROLOG_ECODE_H
