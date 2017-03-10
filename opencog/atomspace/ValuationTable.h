/*
 * opencog/atomspace/ValuationTable.h
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

#ifndef _OPENCOG_VALUTATION_TABLE_H
#define _OPENCOG_VALUTATION_TABLE_H

#include <map>
#include <mutex>
#include <set>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Valuation.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This class provides a mechanism to store valuations for atoms.
 * XXX FIXME:  An alternative sould be to store values directly
 * with each atom. That would make access to values faster, but
 * some number of bytes in each atom, to hold the needed map.
 * Not clear which is preferale, at this point.  RAM is cheap,
 * CPU cycles are harder to get. Hmmmm.
 */
class ValuationTable
{
private:

	// Single, global mutex for locking the indexes.
	mutable std::mutex _mtx;

	std::map<std::pair<Handle, Handle>, ValuationPtr> _vindex;
	std::map<Handle, std::set<Handle>> _keyset;

	/**
	 * Override and declare copy constructor and equals operator as
	 * private.  This is to prevent large object copying by mistake.
	 */
	ValuationTable& operator=(const ValuationTable&);
	ValuationTable(const ValuationTable&);

public:

	ValuationTable();
	~ValuationTable();

   void addValuation(const ValuationPtr&);
   void addValuation(const Handle&, const Handle&, const ProtoAtomPtr&);
	ValuationPtr getValuation(const Handle&, const Handle&);
	ProtoAtomPtr getValue(const Handle&, const Handle&);

	std::set<Handle> getKeys(const Handle&);
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_VALUTATION_TABLE_H
