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
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/**
 * Contains the fitness type of the target. For instance whether the
 * target is a variable query such that the variables maximize the
 * target TV in a certain way, etc.
 */
class TargetFitness
{
};

/**
 * A Target, also a back-inference tree node and how to related to the
 * back-inference tree. A back-inference tree is an and-or-tree, where
 * there are 2 types of children, or-children and and-children. The
 * or-children are represented by Target::rules, because multiple
 * rules or rule variations can infer the same target. Then within
 * each rule or rule variation, the rule premises are and-children
 * because in order to apply a certain rule all premises must be
 * fulfilled.
 */
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

typedef std::shared_ptr<Target> TargetPtr;

/**
 * Mappings from and-tree to forward chaining strategy. The and-tree is
 * represented by its set of leaves.
 *
 * The forward chaining strategy is represented according to
 * https://github.com/opencog/atomspace/issues/903. TODO: copy/paste
 * the doc here and in the wiki as well.
 */
typedef std::unordered_map<OrderedHandleSet, Handle> AndTreeFCMap;

/**
 * Mapping from Handle to TargetPtr in order to quickly access the
 * Target of a certain body. This is useful because the premises of a
 * rule are returned in terms of Handle, not Target.
 */
typedef std::unordered_map<Handle, TargetPtr> HandleTargetPtrMap;
	
// Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Target& target);
std::string oc_to_string(const TargetPtr& target_ptr);

} // ~namespace opencog

#endif // TARGET_H
