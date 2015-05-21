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

#include "Target.h"

using namespace opencog;

Target::Target(const Handle& h) : _htarget(h)
{
}

Target::~Target()
{
}


TargetsSet::TargetsSet()
{
}

TargetsSet::~TargetsSet()
{
}

void TargetsSet::insert(Target& t)
{
	// check if a target with same handle already exist, and if so, do nothing
	if (_targets_map.count(t.get_handle()) == 1)
		return;

	_targets_map[t.get_handle()] = t;
}

void TargetsSet::emplace(Handle& h)
{
	if (_targets_map.count(h) == 1)
		return;

	_targets_map.emplace(h, h);
}

uint TargetsSet::size()
{
	return _targets_map.size();
}

Target& TargetsSet::select()
{
	auto& p = rand_element(_targets_map);
	return _targets_map[p.first];
}
