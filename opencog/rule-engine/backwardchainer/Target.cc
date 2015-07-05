/*
 * Target.cc
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

#include <opencog/util/random.h>
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atomutils/AtomUtils.h>

#include "Target.h"

using namespace opencog;

/**
 * Constructor of Target.
 *
 * Only the TargetSet class can create a Target object.
 *
 * @param as  the AtomSpace in which to store temporary information
 * @param h   the original external Handle of the Target
 */
Target::Target(AtomSpace& as, const Handle& h) : _as(as)
{
	_htarget_external = h;
	_htarget_internal = _as.add_atom(h);
	_selection_count = 0;

	_vars = get_free_vars_in_tree(h);

	// _varmap is a map that bases on the external space
	for (auto& hv : _vars)
		_varmap[hv] = UnorderedHandleSet();
}

/**
 * Store a specific inference step for the Target into the AtomSpace.
 *
 * @param r         the rule applied
 * @param premises  the premises selected to be the rule's input
 */
void Target::store_step(const Rule& r, const HandleSeq& premises)
{
	// XXX TODO think of a good structure for storing the inference step
	// XXX TODO if the rule was actually applied, store the change to the TV?
	_as.add_link(SET_LINK,
	             _htarget_internal,
	             _as.addNode(CONCEPT_NODE, r.get_name()),
	             _as.addLink(LIST_LINK, premises));
}

/**
 * Store new variable mappings.
 *
 * @param vm  a VarMultimap object containing additional mappings
 */
void Target::store_varmap(VarMultimap& vm)
{
	for (auto& p : vm)
	{
		if (_varmap.count(p.first) == 1)
			_varmap[p.first].insert(p.second.begin(), p.second.end());
	}
}

/**
 * Store new variable mapping.
 *
 * @param vm  a VarMap object containing additional mapping
 */
void Target::store_varmap(VarMap& vm)
{
	for (auto& p : vm)
	{
		if (_varmap.count(p.first) == 1)
			_varmap[p.first].insert(p.second);
	}
}

/**
 * Count how many times  a Rule was selected for the Target.
 *
 * This method follow the inference tree atom structure to find all usage.
 *
 * @param r  the Rule to search
 * @return   the number of times applied
 */
unsigned int Target::rule_count(const Rule& r)
{
	Handle hname = _as.add_node(CONCEPT_NODE, r.get_name());
	HandleSeq q = get_neighbors(_htarget_internal, false, true,
	                            SET_LINK, false);

	return std::count(q.begin(), q.end(), hname);
}


//==================================================================


/**
 * Constructor.
 */
TargetSet::TargetSet() : _total_selection(0)
{

}

/**
 * Destructor.
 */
TargetSet::~TargetSet()
{

}

/**
 * Clear the TargetSet.
 */
void TargetSet::clear()
{
	_history_space.clear();
	_targets_map.clear();
}

/**
 * Add a new Target into the set.
 *
 * @param h  the atom to which the Target will be created
 */
void TargetSet::emplace(Handle& h)
{
	if (_targets_map.count(h) == 1)
		return;

	_targets_map.insert(std::pair<Handle, Target>(h, Target(_history_space, h)));
}

/**
 * Get the size of the TargetSet.
 */
unsigned int TargetSet::size()
{
	return _targets_map.size();
}

/**
 * Select a Target from the set using some fitness criteria.
 *
 * Currently uses the selection count to apply weighted random selection.
 *
 * XXX TODO use criteria such as
 * - how many steps from the initial target
 * - how much was gained on this target the last time it was chosen
 * etc
 *
 * @return a reference to the selected Target
 */
Target& TargetSet::select()
{
	HandleSeq handles;
	std::vector<unsigned int> weights;
	for (auto& p : _targets_map)
	{
		handles.push_back(p.first);

		// XXX TODO add more criteria to the weight calculation
		weights.push_back(_total_selection - p.second.get_selection_count() + 1);
	}

	// XXX use cogutil MT19937RandGen's intenal randomGen member possible?
	std::mt19937 generator(std::chrono::system_clock::now().time_since_epoch().count());
	std::discrete_distribution<int> distribution(weights.begin(), weights.end());

	Target& t = _targets_map.at(handles[distribution(generator)]);
	t.increment_selection_count();

	_total_selection++;

	return t;
}

/**
 * Get a specific Target.
 *
 * @param h  the handle of the Target
 * @return   a reference to the Target
 */
Target& TargetSet::get(Handle& h)
{
	return _targets_map.at(h);
}
