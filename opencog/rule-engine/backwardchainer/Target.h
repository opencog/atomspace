/*
 * Target.h
 *
 * Author: William Ma <https://github.com/williampma>
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#ifndef _OPENCOG_TARGET_H
#define _OPENCOG_TARGET_H

#include <opencog/rule-engine/Rule.h>

namespace opencog
{

class Target
{
	friend class TargetSet;

public:
	~Target() {}

	// Comparison
	bool operator==(const Target& t) const
	{
		return _htarget_internal == t._htarget_internal;
	}
	bool operator<(const Target& t) const
	{
		return _htarget_internal < t._htarget_internal;
	}

	void store_step(const Rule& r, const HandleSeq& premises);
	uint rule_count(const Rule& r);

	void increment_selection_count() { _selection_count++; }

	Handle get_handle() const { return _htarget_external; }
	float get_weight() { return 1.0f; }

	// XXX TODO stores the rule applied, and its resulting targets
	// and the change to its own TV
	// actually, write the above information to a inference atom

private:
	Target(AtomSpace* as, const Handle& h);

	Handle _htarget_external;
	Handle _htarget_internal;

	uint _selection_count;

	AtomSpace* _as;
};


class TargetSet
{
public:
	TargetSet() {}
	~TargetSet() {}

	void emplace(Handle& h);
	uint size();
	Target& select();

private:
	std::unordered_map<Handle, Target> _targets_map;
	AtomSpace _history_space;
};

}


#endif // TARGET_H
