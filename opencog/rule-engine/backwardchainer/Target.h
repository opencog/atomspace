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

// Contains the fitness type of the target. For instance whether the
// target is a variable query such that the variables maximize the
// target TV in a certain way, etc.
class TargetFitness
{
};

// A Target, also a back-inference tree node.
class Target
{
public:
	Target(const Handle& h);

	// Target handle
	Handle handle;

	// Target fitness
	TargetFitness fitness;

	// Or-children at the rule level, as multiple rules, or rule
	// variations (partially unified, etc) can yield the same target.
	std::vector<Rule> rules;
};

// Back-inference tree. A back-inference tree is an and-or tree, where
// there are 2 types of children, or-children and
// and-children. Specifically the or-children are represented by
// Target::rules, because multiple rules or rule variations can infer
// the same target. Then within each rule or rule variation, the rule
// premises are and-children because in order to apply a certain rule
// all premises must be fulfilled.
class TargetSet : public std::unordered_map<Handle, Target>
{
};

// obsolete code to be removed as soon as the new code is functional
#if 0
class TargetOld
{
	friend class TargetSet;

public:

	~TargetOld() {}

	// Comparison
	bool operator==(const Target& t) const
	{
		return _htarget == t._htarget;
	}
	bool operator<(const Target& t) const
	{
		return _htarget < t._htarget;
	}

	void store_step(const Rule& r, const HandleSeq& premises);
	void store_varmap(HandleMultimap& vm);
	void store_varmap(HandleMap& vm);
	unsigned int rule_count(const Rule& r) const;

	/**
	 * Increment the internal counter.
	 *
	 * Useful for storing how many times a Target is selected.
	 */
	void increment_selection_count() { _selection_count++; }

	/**
	 * Get the external Handle referred to be Target.
	 *
	 * @return the handle
	 */
	Handle get_handle() const { return _htarget; }

	/**
	 * Get the "free" variables list in HandleSeq.
	 *
	 * @return the HandleSeq
	 */
	HandleSeq get_varseq() const
	{
		return VariableListCast(_vardecl)->get_variables().varseq;
	}

	/**
	 * Get the "free" variables list in OrderedHandleSet
	 *
	 * @return the OrderedHandleSet
	 */
	OrderedHandleSet get_varset() const
	{
		return VariableListCast(_vardecl)->get_variables().varset;
	}

	Handle get_vardecl() const { return _vardecl; }

	/**
	 * Get the stored free variable mappings.
	 *
	 * @return a HandleMultimap object of the mappings
	 */
	const HandleMultimap& get_varmap() const { return _varmap; }

	unsigned int get_selection_count() const { return _selection_count; }

	/**
	 * Get the weight associated with this Target.
	 *
	 * Useful for perhaps target selection.
	 * XXX TODO actually calculate something here
	 *
	 * @return the weight in float
	 */
	float get_weight() { return 1.0f; }

	std::string to_string();

private:
	Target(AtomSpace& as, const Handle& h, const Handle& hvardecl);

	Handle _htarget;
	unsigned int _selection_count;

	Handle _vardecl;
	HandleMultimap _varmap;

	AtomSpace& _as;
};

class TargetSetOld
{
public:
	TargetSet();
	~TargetSet();

	void clear();
	void emplace(Handle h, Handle hvardecl);
	unsigned int size();
	Target& select();
	Target& get(Handle h);

private:
	std::map<Handle, Target> _targets_map;
	AtomSpace _history_space;
	unsigned int _total_selection;
};
#endif

}


#endif // TARGET_H
