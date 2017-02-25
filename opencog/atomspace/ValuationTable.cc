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

void ValuationTable::addValuation(ValuationPtr& vp)
{
	_vindex.insert(std::make_pair(
		std::make_pair(vp->key(), vp->atom()), vp));
}

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
