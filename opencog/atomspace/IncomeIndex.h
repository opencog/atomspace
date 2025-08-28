/*
 * opencog/atomspace/IncomeIndex.h
 *
 * Copyright (C) 2008,2025 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_INCOMEINDEX_H
#define _OPENCOG_INCOMEINDEX_H

#include <mutex>
#include <set>
#include <vector>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
// The InSet is the IncomingSet map, plus a lock on that set.
struct InSet : std::map<Handle, InSetMap>
{
	mutable std::shared_mutex _mtx;
};

#define INCOME_INDEX_SHARED_LOCK(s) std::shared_lock<std::shared_mutex> lck(s._mtx);
#define INCOME_INDEX_UNIQUE_LOCK(s) std::unique_lock<std::shared_mutex> lck(s._mtx);

/**
 * Implements a pool holding Incoming sets for those Atoms that have
 * incoming sets. The goal of the pool is to avoid lock contention;
 * this is possible, because each Handle has a deterministic hash,
 * which can be used to identify the pool.
 */
class IncomeIndex
{
	private:
		mutable std::vector<InSet> _idx;

		static constexpr int POOL_SIZE = 32;
		InSet& get_inset(const Handle& h) const
		{
			return _idx[h->get_hash() % POOL_SIZE];
		}
	public:
		IncomeIndex(void) {}

		void removeInset(const Handle& h)
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_UNIQUE_LOCK(s);
			s.erase(h);
		}

		bool haveInset(const Handle& h) const
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_SHARED_LOCK(s);
			const auto inset = s.find(h);
			return s.end() != inset;
		}

		// Get the inset, or create a new one.
		InSetMap& getInset(const Handle& h)
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_UNIQUE_LOCK(s);
			InSetMap iset;

			auto iter = s.find(h);
			if (s.end() == iter)
			{
				s.insert({h, InSetMap()});
				iter = s.find(h);
			}
			return iter->second;
		}

		// Swap the handle to which the incoming set belonws.
		void swapInset(const Handle& oldh, const Handle& newh)
		{
			// Both the old and the new will have exactly the same hash.
			InSet& s(get_inset(oldh));
			INCOME_INDEX_UNIQUE_LOCK(s);
			InSetMap iset;

			auto iter = s.find(oldh);
			if (s.end() == iter)
			{
				s.insert({newh, InSetMap()});
				return;
			}
			s.erase(oldh);
			s.insert({newh, iter->second});
		}

		// How many entries are there, anyway?
		size_t size(void) const
		{
			size_t cnt = 0;
			for (const InSet& s : _idx)
			{
				INCOME_INDEX_SHARED_LOCK(s);
				cnt += s.size();
			}
			return cnt;
		}

		void clear(void);
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_INCOMEINDEX_H
