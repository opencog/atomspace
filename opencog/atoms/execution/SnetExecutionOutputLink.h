/*
 * opencog/atoms/execution/SnetExecutionOutputLink.h
 *
 * Copyright (C) 2019 Vitaly Bogdanov <vsbogd@gmail.com>
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

#ifndef _OPENCOG_SNET_EXECUTION_OUTPUT_LINK_H
#define _OPENCOG_SNET_EXECUTION_OUTPUT_LINK_H

#include <opencog/atoms/execution/ExecutionOutputLink.h>

namespace opencog
{

class SnetExecutionOutputLink : public ExecutionOutputLink
{
protected:
	static void check_schema(const Handle& schema);

public:
	SnetExecutionOutputLink(const HandleSeq& oset, Type t);

	virtual Handle execute(AtomSpace* as, bool silent = false) const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<SnetExecutionOutputLink> SnetExecutionOutputLinkPtr;

static inline SnetExecutionOutputLinkPtr SnetExecutionOutputLinkCast(const Handle& h)
{
	return std::dynamic_pointer_cast<SnetExecutionOutputLink>(h);
}
static inline SnetExecutionOutputLinkPtr SnetExecutionOutputLinkCast(ValuePtr v)
{
	return std::dynamic_pointer_cast<SnetExecutionOutputLink>(v);
}

template<typename ... Type>
static inline SnetExecutionOutputLinkPtr createSnetExecutionOutputLink(Type&&... args)
{
	return std::make_shared<SnetExecutionOutputLink>(std::forward<Type>(args)...);
}

}

#endif // _OPENCOG_SNET_EXECUTION_OUTPUT_LINK_H

