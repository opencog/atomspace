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
 * Implements a pool holding incoming sets for those Atoms that have
 * incoming sets. This pool is not currently used; it does not help
 * either with performance or with RAM usage!
 *
 * The original idea motivating this pool was the observation that
 * not all Atoms have incoming sets. Typically, only half of them do,
 * and so we should be able to save space by moving the InSetMap out
 * of the Atom, and into the pool, here. But this idea fails miserably,
 * and here's why.
 *
 * sizeof(InSetMap) = 48
 * sizeof(Handle) = 16
 * sizeof(std::pair<Handle, InSetMap>) = 64 = 16 + 48
 * sizeof std::map<Handle, InSetMap> entry approx 96 bytes total.
 *
 * The savings of reducing each Atom by 48 bytes is offset by requiring
 * approx 96 bytes to store the incoming sets for those Atoms that do
 * have them. So even if half of all Atoms don't have an incoming set,
 * there's no savings, because twice as much RAM is neded for the other
 * half.
 *
 * Measured on a typical dataset, the sfia dataset, average RAM per Atom
 * is 8 bytes **more**, using the IncomeIndex, here, than keeping the
 * incoming set inline with the Atom itself. That, plus using the index
 * here hurts performance by about 20%. In short, its a disaster. The
 * code here is left for reference, but it is not compiled.
 *
 * A pool is used to avoid lock contention. This is possible, because
 * each Handle has a deterministic hash, which can be used to identify
 * the pool.
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
		IncomeIndex(void);

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
				return;

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
