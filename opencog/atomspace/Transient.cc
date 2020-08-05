/*
 * Transient.cc
 *
 * Copyright (C) 2008,2009,2014,2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  February 2008
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

#include <atomic>
#include <opencog/util/Logger.h>

#include <opencog/atomspace/AtomSpace.h>
#include "Transient.h"

using namespace opencog;

/* ======================================================== */
/// Cache for temp (transient) atomspaces.  The evaluation of
/// expressions during pattern matching and during other operations
/// requires having a temporary atomspace, treated as a scratch space,
/// to hold temporary results. These are then discarded, after the
/// match is confirmed or denied. The issue is that creating an
/// atomspace is CPU-intensive, so its cheaper to just have a cache
/// of empty atomspaces, hanging around, and ready to go. The code
/// in this section implements this.

// XXX TODO This should be changed to use an use-counting AtomSpacePtr
// so that the transients are automatically released back to the pool.
// This will avoid mem leakage due to bad exception handling and other
// programming bugs.

const bool TRANSIENT_SPACE = true;
const int MAX_CACHED_TRANSIENTS = 32;

// Allocated storage for the transient atomspace cache static variables.
static std::mutex s_transient_cache_mutex;
static std::vector<AtomSpace*> s_transient_cache;

static std::atomic_int num_issued = 0;

AtomSpace* opencog::grab_transient_atomspace(AtomSpace* parent)
{
	AtomSpace* transient_atomspace = nullptr;

	// See if the cache has one...
	if (s_transient_cache.size() > 0)
	{
		// Grab the mutex lock.
		std::unique_lock<std::mutex> cache_lock(s_transient_cache_mutex);

		// Check to make sure the cache still has one now that we have
		// the mutex.
		if (s_transient_cache.size() > 0)
		{
			// Pop the latest transient atomspace off the cache stack.
			transient_atomspace = s_transient_cache.back();
			s_transient_cache.pop_back();

			// Ready it for the new parent atomspace.
			transient_atomspace->ready_transient(parent);

			num_issued ++;
		}
	}

	// If we didn't get one from the cache, then create a new one.
	if (!transient_atomspace)
	{
		transient_atomspace = new AtomSpace(parent, TRANSIENT_SPACE);
		num_issued ++;
	}

	if (MAX_CACHED_TRANSIENTS < num_issued.load())
		throw FatalErrorException(TRACE_INFO, "Transient space memleak!");

	return transient_atomspace;
}

void opencog::release_transient_atomspace(AtomSpace* atomspace)
{
	bool atomspace_cached = false;

	// If the cache is not full...
	if (s_transient_cache.size() < MAX_CACHED_TRANSIENTS)
	{
		// Grab the mutex lock.
		std::unique_lock<std::mutex> cache_lock(s_transient_cache_mutex);

		// Check it again since we only now have the mutex locked.
		if (s_transient_cache.size() < MAX_CACHED_TRANSIENTS)
		{
			// Clear this transient atomspace.
			atomspace->clear_transient();

			// Place this transient into the cache.
			s_transient_cache.push_back(atomspace);

			// The atomspace has been cached.
			atomspace_cached = true;

			num_issued--;
		}
	}

	// If we didn't cache the atomspace, then delete it.
	if (!atomspace_cached)
		delete atomspace;
}

/* ===================== END OF FILE ===================== */
