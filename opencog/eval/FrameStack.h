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

#ifndef _OPENCOG_FRAMESTACK_H
#define _OPENCOG_FRAMESTACK_H

namespace opencog
{

AtomSpacePtr get_frame(void);
void push_frame(const AtomSpacePtr&);
void push_frame(const ValuePtr&);
AtomSpacePtr pop_frame(void);
void set_frame(const AtomSpacePtr&);
void set_frame(const ValuePtr&);
void clear_frame_stack(void);

// Simple RAII guard for the current AtomSpace frame.
struct ASGuard
{
	ASGuard(AtomSpace* as)
	{ push_frame(AtomSpaceCast(as)); }
	ASGuard(const AtomSpacePtr& asp)
	{ push_frame(asp); }
	~ASGuard() { pop_frame(); }
	ASGuard(const ASGuard&) = delete;
	ASGuard& operator=(const ASGuard&) = delete;
};

}

#endif // _OPENCOG_FRAMESTACK_H
