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
#include <mutex>
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
/// atomspace (was) CPU-intensive (it's pretty small, now), so its
/// cheaper to just have a cache of empty atomspaces, hanging around,
/// and ready to go. The code in this section implements this.
///
/// XXX FIXME. Performance has not been recently measured; there
/// have been a lot of redesigns since when this utility was created.
/// It is not at all clear that the code here takes less CPU/RAM than
/// simply creating new AtomSpaces on the fly. For now, we keep this
/// code, as a historical precedent. But performance measurements should
/// be done, and if there is no savings, this code should be trashed.

const bool TRANSIENT_SPACE = true;
const int MAX_CACHED_TRANSIENTS = 1024;

// Allocated storage for the transient atomspace cache static variables.
static std::mutex s_transient_cache_mutex;
static std::vector<AtomSpacePtr> s_transient_cache;
static std::atomic_int num_issued = 0;

AtomSpace* opencog::grab_transient_atomspace(AtomSpace* parent)
{
	AtomSpacePtr tranny;

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
			tranny = s_transient_cache.back();
			s_transient_cache.pop_back();

			// Ready it for the new parent atomspace.
			tranny->ready_transient(parent);
			num_issued ++;
		}
	}

	// If we didn't get one from the cache, then create a new one.
	if (!tranny)
	{
		tranny = createAtomSpace(parent, TRANSIENT_SPACE);
		num_issued ++;
	}

	if (MAX_CACHED_TRANSIENTS < num_issued.load())
		logger().warn("Possible transient space memleak!");

	return tranny.get();
}

void opencog::release_transient_atomspace(AtomSpace* atomspace)
{
	num_issued--;

	// Grab the mutex lock.
	std::unique_lock<std::mutex> cache_lock(s_transient_cache_mutex);

	// Don't bother, if there are alreeady plenty.
	if (s_transient_cache.size() < MAX_CACHED_TRANSIENTS)
	{
		// Clear this transient atomspace.
		atomspace->clear_transient();

		// Place this transient into the cache.
		AtomSpacePtr tranny(AtomSpaceCast(atomspace));
		s_transient_cache.push_back(tranny);
	}
}

/* ===================== END OF FILE ===================== */
