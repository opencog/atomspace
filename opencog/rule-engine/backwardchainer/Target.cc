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

#include <opencog/atomutils/AtomUtils.h>

#include "Target.h"

using namespace opencog;

Target::Target(AtomSpace* as, const Handle& h)
{
	_as = as;
	_htarget_external = h;
	_htarget_internal = _as->addAtom(h);
	_selection_count = 0;
}

void Target::store_step(const Rule& r, const HandleSeq& premises)
{
	// XXX TODO think of a good structure for storing the inference step
	// XXX TODO if the rule was actually applied, store the change to the TV?
	_as->addLink(REFERENCE_LINK,
	             _htarget_internal,
	             _as->addNode(CONCEPT_NODE, r.get_name()),
	             _as->addLink(LIST_LINK, premises));
}

void Target::store_varmap(VarMultimap& vm)
{
	for (auto& p : vm)
		_varmap[p.first].insert(p.second.begin(), p.second.end());
}

uint Target::rule_count(const Rule& r)
{
	Handle hname = _as->addNode(CONCEPT_NODE, r.get_name());
	HandleSeq q = getNeighbors(_htarget_internal, false, true, REFERENCE_LINK, false);

	return std::count(q.begin(), q.end(), hname);
}


//==================================================================


TargetSet::TargetSet()
{

}

TargetSet::~TargetSet()
{

}

void TargetSet::emplace(Handle& h)
{
	if (_targets_map.count(h) == 1)
		return;

	_targets_map.emplace(h, &_history_space, h);
}

uint TargetSet::size()
{
	return _targets_map.size();
}

/**
 * Select a Target from the set using some fitness criteria.
 *
 * @return a reference to the selected Target
 */
Target& TargetSet::select()
{
	// XXX TODO do proper target selection here using some fitness function
	auto& p = rand_element(_targets_map);

	Target& t = _targets_map[p.first];
	t.increment_selection_count();

	return t;
}
