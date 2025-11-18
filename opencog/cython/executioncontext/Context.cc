/*
 * Copyright (C) 2019 OpenCog Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 */

#include "Context.h"

namespace opencog {

thread_local std::deque<AtomSpacePtr> current = std::deque<AtomSpacePtr>();

AtomSpacePtr get_context_atomspace(void)
{
	if (current.empty())
		return nullptr;
	return current.back();
}

void push_context_atomspace(const AtomSpacePtr& asp)
{
	current.push_back(asp);
}

void push_context_atomspace(const ValuePtr& vasp)
{
	current.push_back(AtomSpaceCast(vasp));
}

AtomSpacePtr pop_context_atomspace(void)
{
	AtomSpacePtr result = get_context_atomspace();
	current.pop_back();
	return result;
}

void clear_context(void)
{
	current.clear();
}

}
