/*
 * opencog/atoms/foreign/LispAST.h
 *
 * Copyright (C) 2021, 2022 Linas Vepstas
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

#ifndef _OPENCOG_LISP_AST_H
#define _OPENCOG_LISP_AST_H

#include <opencog/atoms/foreign/SexprAST.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LispAST holds Lisp-like, Scheme-like abstract syntax trees,
/// printed as Lisp-like s-expressions. Quasi-compatible with current
/// experimental MeTTA code.
///
class LispAST : public SexprAST
{
	void init();

	virtual Handle next_expr(const std::string&, size_t& l, size_t& r);
	static Handle get_next_expr(const std::string&, size_t& l, size_t& r);

	virtual ContentHash compute_hash() const;

public:
	LispAST(const HandleSeq&&, Type = LISP_AST);
	LispAST(const LispAST&) = delete;
	LispAST& operator=(const LispAST&) = delete;

	LispAST(const std::string&);

	virtual std::string to_string(const std::string& indent) const;
	virtual std::string to_short_string(const std::string& indent) const;

	virtual bool operator==(const Atom&) const;
	static Handle factory(const Handle&);
};

LINK_PTR_DECL(LispAST)
#define createLispAST CREATE_DECL(LispAST)

/** @}*/
}

#endif // _OPENCOG_LISP_AST_H
