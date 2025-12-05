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

#ifndef _OPENCOG_FRAMESTACK_H
#define _OPENCOG_FRAMESTACK_H

#include <deque>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{

// Thread-local stack of AtomSpaces. Declared extern to ensure a single
// instance shared across all shared libraries. Definition in FrameStack.cc.
extern thread_local std::deque<AtomSpacePtr> frame_stack;

inline const AtomSpacePtr& get_frame(void)
{
	static AtomSpacePtr nullasp;
	if (frame_stack.empty())
		return nullasp;
	return frame_stack.back();
}

inline void push_frame(const AtomSpacePtr& asp)
{
	frame_stack.push_back(asp);
}

inline void push_frame(const ValuePtr& vasp)
{
	frame_stack.push_back(AtomSpaceCast(vasp));
}

inline AtomSpacePtr pop_frame(void)
{
	AtomSpacePtr result = get_frame();
	frame_stack.pop_back();
	return result;
}

inline void set_frame(const AtomSpacePtr& asp)
{
	if (frame_stack.empty())
		frame_stack.push_back(asp);
	else
		frame_stack.back() = asp;
}

inline void set_frame(const ValuePtr& vasp)
{
	set_frame(AtomSpaceCast(vasp));
}

}

#endif // _OPENCOG_FRAMESTACK_H
