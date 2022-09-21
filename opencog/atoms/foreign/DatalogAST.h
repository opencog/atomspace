/*
 * opencog/atoms/foreign/DatalogAST.h
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

#ifndef _OPENCOG_DATALOG_AST_H
#define _OPENCOG_DATALOG_AST_H

#include <opencog/atoms/foreign/ForeignAST.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DatalogAST holds a tiny subset of Prolog, some (unspecified)
/// subset of Datalog, i.e. simple assertions.
///
class DatalogAST : public ForeignAST
{
	void init();

	static std::string prt_datalog(const Handle&, bool=false);

protected:
	void parse(const std::string&);

public:
	DatalogAST(const HandleSeq&&, Type = DATALOG_AST);
	DatalogAST(const HandleSeq&&, const std::string&&);
	DatalogAST(const DatalogAST&) = delete;
	DatalogAST& operator=(const DatalogAST&) = delete;

	DatalogAST(const std::string&);

	virtual std::string to_string(const std::string& indent) const;
	virtual std::string to_short_string(const std::string& indent) const;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DatalogAST)
#define createDatalogAST CREATE_DECL(DatalogAST)

/** @}*/
}

#endif // _OPENCOG_DATALOG_AST_H
