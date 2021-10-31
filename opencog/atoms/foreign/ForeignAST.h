/*
 * opencog/atoms/foreign/ForeignAST.h
 *
 * Copyright (C) 2021 Linas Vepstas
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

#ifndef _OPENCOG_FOREIGN_AST_H
#define _OPENCOG_FOREIGN_AST_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ForeignAST holds generic abstract syntax trees
class ForeignAST : public Link
{
protected:
	std::string _name;

public:
	ForeignAST(const HandleSeq&&, Type = FOREIGN_AST);
	ForeignAST(Type);
	ForeignAST(Type, const std::string&);
	ForeignAST(const ForeignAST&) = delete;
	ForeignAST& operator=(const ForeignAST&) = delete;

	virtual const std::string& get_name() const { return _name; }
};

typedef std::shared_ptr<ForeignAST> ForeignASTPtr;
static inline ForeignASTPtr ForeignASTCast(const Handle& h)
	{ return std::dynamic_pointer_cast<ForeignAST>(h); }
static inline ForeignASTPtr ForeignASTCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<ForeignAST>(a); }


template< class... Args >
Handle createForeignAST( Args&&... args )
{
	Handle tmp(std::make_shared<ForeignAST>(std::forward<Args>(args) ...));
	return classserver().factory(tmp);
}

/** @}*/
}

#endif // _OPENCOG_FOREIGN_AST_H
