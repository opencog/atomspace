/*
 * opencog/atomspace/ValuationTable.cc
 *
 * Copyright (C) 2017 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
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

#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/Atom.h>

#include "ValuationTable.h"

using namespace opencog;

ValuationTable::ValuationTable()
{
}

ValuationTable::~ValuationTable()
{
}

/// Associate a value with a particular (key,atom) pair
/// The atom, key and value are wrapped up in a single valuation.
void ValuationTable::addValuation(const ValuationPtr& vp)
{
	const Handle& key = vp->key();
	const Handle& atom = vp->atom();

	std::lock_guard<std::mutex> lck(_mtx);

	// Make a record of all the keys being used for this atom.
	auto ikeys = _keyset.find(atom);
	if (ikeys == _keyset.end())
	{
		HandleSet keys;
		keys.insert(key);
		_keyset.insert(make_pair(atom, keys));
	}
	else
	{
		ikeys->second.insert(key);
	}

	// Record the actual valuation
	_vindex[std::make_pair(key, atom)] = vp;
}

/// Associate a value with a particular (key,atom) pair
void ValuationTable::addValuation(const Handle& key,
                                  const Handle& atom,
                                  const ProtoAtomPtr& val)
{
	ValuationPtr vp(createValuation(key, atom, val));
	addValuation(vp);
}

ValuationPtr ValuationTable::getValuation(const Handle& key, const Handle& atom)
{
	std::lock_guard<std::mutex> lck(_mtx);

	auto vpiter = _vindex.find(std::make_pair(key, atom));
	if (vpiter == _vindex.end())
		throw RuntimeException(TRACE_INFO,
			"There is no value for key %s on atom %s",
			key->to_string().c_str(), atom->to_string().c_str());
	return vpiter->second;
}

ProtoAtomPtr ValuationTable::getValue(const Handle& key, const Handle& atom)
{
	return getValuation(key, atom)->value();
}

/// Obtain all of the keys in use for a given atom.
//
// XXX FIXME this implementation is ... poor. Its probably just enough
// to store all keys in use, instead of all keys for a given atom.
// i.e. this uses too much RAM, and we could get away with just
// searching the _vindex, instead, which would be slower but more memory
// efficient.  The point is that the only user of this function is going
// to be the peristence framework, and so we should optimize for that.
HandleSet ValuationTable::getKeys(const Handle& atom)
{
	std::lock_guard<std::mutex> lck(_mtx);

	auto ikeys = _keyset.find(atom);
	if (ikeys == _keyset.end())
		return HandleSet();

	return ikeys->second;
}
