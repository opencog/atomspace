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

// Entry tracking a pushed atomspace and the atomspace that was current
// before the push. This allows set_frame() to change the current atomspace
// without corrupting the push/pop stack.
struct PushEntry
{
	AtomSpacePtr saved;   // The atomspace before the push
	AtomSpacePtr pushed;  // The atomspace created by push_frame
};

// Thread-local current atomspace. Declared extern to ensure a single
// instance shared across all shared libraries. Definition in FrameStack.cc.
extern thread_local AtomSpacePtr current_frame;

// Thread-local stack of pushed atomspaces.
extern thread_local std::deque<PushEntry> pushed_stack;

inline const AtomSpacePtr& get_frame(void)
{
	return current_frame;
}

// Create a new atomspace (child of current) and push it onto the stack.
// Sets the current frame to the new child atomspace.
inline void push_frame(void)
{
	AtomSpacePtr parent = current_frame;
	AtomSpacePtr child = createAtomSpace(parent);

	// Track this push so we can properly clean up on pop
	pushed_stack.push_back({parent, child});

	// Set current to the new child
	current_frame = child;
}

// Pop the most recently pushed atomspace.
// Restores the saved atomspace as current.
// Clears the pushed atomspace.
// Removes the pushed atomspace from its parent.
inline void pop_frame(void)
{
	if (pushed_stack.empty())
		return;

	PushEntry entry = pushed_stack.back();
	pushed_stack.pop_back();

	// Restore the saved atomspace as current
	current_frame = entry.saved;

	// Clear the pushed atomspace, making any atoms in it into orphans.
	entry.pushed->clear();

	// Remove the transient atomspace from its base.
	entry.saved->extract_atom(HandleCast(entry.pushed));
}

// Set the current atomspace. Does NOT affect the push/pop stack.
inline void set_frame(const AtomSpacePtr& asp)
{
	current_frame = asp;
}

inline void set_frame(const ValuePtr& vasp)
{
	set_frame(AtomSpaceCast(vasp));
}

}

#endif // _OPENCOG_FRAMESTACK_H
