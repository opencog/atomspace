/*
 * Target.h
 *
 * Authors: William Ma <https://github.com/williampma>
 *          Nil Geisweiller
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
	Target(const Handle& body, const Handle& vardecl = Handle::UNDEFINED,
	       const TargetFitness& fit = TargetFitness());

	// Target handle
	Handle body;

	// Target variable declaration
	Handle vardecl;

	// Target fitness
	TargetFitness fitness;

	// Or-children at the rule level, as multiple rules, or rule
	// variations (partially unified, etc) can yield the same target.
	RuleSeq rules;

	std::string to_string() const;
};

// TODO: we need a set of and-branches. Each and-branch wil composed
// of its leaves (not necessarily being without children) and the
// associated atomese representation.
	
// Back-inference tree. A back-inference tree is an and-or tree, where
// there are 2 types of children, or-children and and-children. The
// or-children are represented by Target::rules, because multiple
// rules or rule variations can infer the same target. Then within
// each rule or rule variation, the rule premises are and-children
// because in order to apply a certain rule all premises must be
// fulfilled.
class TargetSet : public std::unordered_map<Handle, Target>
{
};

// Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Target& target);

} // ~namespace opencog

#endif // TARGET_H
