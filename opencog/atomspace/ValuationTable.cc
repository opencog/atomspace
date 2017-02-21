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
	return nullptr;
}

ProtoAtomPtr ValuationTable::getValue(const Handle& key, const Handle& atom)
{
	return nullptr;
}
