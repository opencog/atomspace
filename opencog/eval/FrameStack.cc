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

#include "FrameStack.h"

namespace opencog {

thread_local std::deque<AtomSpacePtr> frame_stack = std::deque<AtomSpacePtr>();

AtomSpacePtr get_frame(void)
{
	if (frame_stack.empty())
		return nullptr;
	return frame_stack.back();
}

void push_frame(const AtomSpacePtr& asp)
{
	frame_stack.push_back(asp);
}

void push_frame(const ValuePtr& vasp)
{
	frame_stack.push_back(AtomSpaceCast(vasp));
}

AtomSpacePtr pop_frame(void)
{
	AtomSpacePtr result = get_frame();
	frame_stack.pop_back();
	return result;
}

void set_frame(const AtomSpacePtr& asp)
{
	if (frame_stack.empty())
		frame_stack.push_back(asp);
	else
		frame_stack.back() = asp;
}

void set_frame(const ValuePtr& vasp)
{
	set_frame(AtomSpaceCast(vasp));
}

void clear_frame_stack(void)
{
	frame_stack.clear();
}

}
