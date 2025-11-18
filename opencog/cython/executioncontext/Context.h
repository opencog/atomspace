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
#include <opencog/atomspace/AtomSpace.h>
#include <deque>

#ifndef _OPENCOG_CONTEXT_H
#define _OPENCOG_CONTEXT_H

namespace opencog
{

AtomSpacePtr get_context_atomspace(void);
void push_context_atomspace(const AtomSpacePtr&);
void push_context_atomspace(const ValuePtr&);
AtomSpacePtr pop_context_atomspace(void);
void clear_context(void);

// Simple RAII guard for the current python conext AtomSpace.
struct ASGuard
{
	ASGuard(AtomSpace* as)
	{ push_context_atomspace(AtomSpaceCast(as)); }
	ASGuard(const AtomSpacePtr& asp)
	{ push_context_atomspace(asp); }
	~ASGuard() { pop_context_atomspace(); }
	ASGuard(const ASGuard&) = delete;
	ASGuard& operator=(const ASGuard&) = delete;
};

}

#endif // _OPENCOG_CONTEXT_H
