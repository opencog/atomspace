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
 * Implements a pool hholding Incoming sets for those Atoms that have
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

		void insertAtom(const Handle& h)
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_UNIQUE_LOCK(s);
#define SANITY_CHECK
#ifdef SANITY_CHECK
			auto iter = s.find(h);
			OC_ASSERT(s.end() == iter, "Double insertion of Incming Set!");
#endif
			s.insert({h,InSetMap()});
		}

		void removeAtom(const Handle& h)
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_UNIQUE_LOCK(s);
			s.erase(h);
		}

		const InSetMap& findInset(const Handle& h) const
		{
			InSet& s(get_inset(h));
			INCOME_INDEX_SHARED_LOCK(s);
			const auto inset = s.find(h);

			static const InSetMap empty_map;
			if (s.end() == inset) return empty_map;
			return inset->second;
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
