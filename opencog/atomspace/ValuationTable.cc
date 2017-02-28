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
void ValuationTable::addValuation(ValuationPtr& vp)
{
	const Handle& key = vp->key();
	const Handle& atom = vp->atom();

	// Make a record of all the keys being used for this atom.
	auto ikeys = _keyset.find(atom);
	if (ikeys == _keyset.end())
	{
		std::set<Handle> keys;
		keys.insert(key);
		_keyset.insert(make_pair(atom, keys));
	}
	else
	{
		ikeys->second.insert(key);
	}

	// Record the actual valuation
	_vindex.insert(std::make_pair(
		std::make_pair(key, atom), vp));
}

/// Associate a value with a particular (key,atom) pair
void ValuationTable::addValuation(const Handle& key,
                                  const Handle& atom,
                                  ProtoAtomPtr& val)
{
	ValuationPtr vp(createValuation(key, atom, val));
	addValuation(vp);
}

ValuationPtr ValuationTable::getValuation(const Handle& key, const Handle& atom)
{
	auto vpiter = _vindex.find(std::make_pair(key, atom));
	if (vpiter == _vindex.end())
		throw RuntimeException(TRACE_INFO, "there is now value for this key");
	return vpiter->second;
}

ProtoAtomPtr ValuationTable::getValue(const Handle& key, const Handle& atom)
{
	return getValuation(key, atom)->value();
}

std::set<Handle> ValuationTable::getKeys(const Handle& atom)
{
	auto ikeys = _keyset.find(atom);
	if (ikeys == _keyset.end())
		return std::set<Handle>();

	return ikeys->second;
}
