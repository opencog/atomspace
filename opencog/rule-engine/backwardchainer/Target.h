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
public:
	Target(const Handle& h);
	~Target();

	// Comparison
	bool operator==(const Target& t) const
	{
		return _htarget == t._htarget;
	}
	bool operator<(const Target& t) const
	{
		return _htarget < t._htarget;
	}

	void add_rule(const Rule& r) { _applied_rules.push_back(r); }

	Handle get_handle() const { return _htarget; }
	float get_weight() { return 1.0f; }

private:

	Handle _htarget;
	std::list<Rule> _applied_rules;
};

// allow the Target class to be used in unordered_set
struct target_hash
{
    size_t operator()(const Target& t ) const
    {
        return handle_hash()(t.get_handle());
    }
};

}

#endif // TARGET_H
