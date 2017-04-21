/*
 * opencog/atoms/execution/ExecutionOutputLink.h
 *
 * Copyright (C) 2013,2015 Linas Vepstas
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

#ifndef _OPENCOG_EXECUTION_OUTPUT_LINK_H
#define _OPENCOG_EXECUTION_OUTPUT_LINK_H

#include <stdlib.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class ExecutionOutputLink : public FunctionLink
{
private:
	static Handle do_execute(AtomSpace*, const Handle& schema, const Handle& args,
	                         bool silent=false);

	void check_schema(const Handle& schema) const;

public:
	/**
	 * Given a grounded schema name like "py: foo", extract
	 * 1. the language, like "py"
	 * 2. the library, like "" if there is none
	 * 3. the function, like "foo"
	 */
	static void lang_lib_fun(const std::string& schema,
	                         std::string& lang,
	                         std::string& lib,
	                         std::string& fun);;

	ExecutionOutputLink(const HandleSeq&, Type=EXECUTION_OUTPUT_LINK);
	ExecutionOutputLink(const Handle& schema, const Handle& args);
	ExecutionOutputLink(const Link& l);

	virtual Handle execute(AtomSpace* as=NULL, bool silent=false) const;

	Handle get_schema(void) const { return getOutgoingAtom(0); }
	Handle get_args(void) const { return getOutgoingAtom(1); }

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<ExecutionOutputLink> ExecutionOutputLinkPtr;
static inline ExecutionOutputLinkPtr ExecutionOutputLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<ExecutionOutputLink>(a); }
static inline ExecutionOutputLinkPtr ExecutionOutputLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<ExecutionOutputLink>(a); }

// XXX temporary hack ...
#define createExecutionOutputLink std::make_shared<ExecutionOutputLink>

/** @}*/
}

#endif // _OPENCOG_EXECUTION_OUTPUT_LINK_H
