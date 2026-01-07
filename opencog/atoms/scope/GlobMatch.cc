/*
 * opencog/atoms/flow/GlobMatch.cc
 *
 * Copyright (C) 2025 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <stack>
#include <map>
#include <opencog/atoms/value/LinkValue.h>
#include "GlobMatch.h"

using namespace opencog;

namespace opencog
{

// Internal data structures for backtracking state

template<typename GroundSeq>
struct GlobStackEntry
{
	Handle glob;          // The glob being matched
	size_t pattern_idx;   // Position in pattern where glob appears
	size_t ground_idx;    // Position in ground where glob match started

	GlobStackEntry(const Handle& g, size_t p, size_t gr)
		: glob(g), pattern_idx(p), ground_idx(gr) {}
};

template<typename GroundSeq>
using GlobStack = std::stack<GlobStackEntry<GroundSeq>>;

// Map from glob to the size we last tried for it (for backtracking)
using GlobSizeMap = std::map<Handle, size_t>;

// ================================================================

template<typename GroundSeq>
bool glob_match(
	const HandleSeq& pattern,
	const GroundSeq& ground,
	ValueMap& bindings,
	const Variables* variables,
	GlobValidateCallback<GroundSeq> validate,
	GlobMakeValueCallback<GroundSeq> make_value,
	size_t pattern_start,
	size_t pattern_size)
{
	// Calculate actual pattern bounds
	size_t pattern_end = (pattern_size == SIZE_MAX)
		? pattern.size()
		: pattern_start + pattern_size;

	// Matching state
	size_t ip = pattern_start;  // Pattern index
	size_t jg = 0;              // Ground index

	// Backtracking state
	GlobStack<GroundSeq> glob_stack;
	GlobSizeMap glob_sizes;     // Track sizes we've tried for each glob
	bool backtracking = false;
	bool exhausted = false;

	// Lambdas for common operations

	// Backtrack to previous glob position
	auto backtrack = [&](bool is_glob) -> void
	{
		backtracking = true;

		// If we just failed on a glob, remove its binding and size record
		if (is_glob && !glob_stack.empty())
		{
			Handle glob = glob_stack.top().glob;
			bindings.erase(glob);
			glob_sizes.erase(glob);
			glob_stack.pop();
		}

		// If no more globs to backtrack to, we're done
		if (glob_stack.empty())
		{
			exhausted = true;
			return;
		}

		// Restore position to the previous glob
		GlobStackEntry<GroundSeq>& prev = glob_stack.top();
		ip = prev.pattern_idx;
		jg = prev.ground_idx;

		// Remove that glob's binding so we can try a smaller size
		bindings.erase(prev.glob);
	};

	// Main matching loop
	while (ip < pattern_end)
	{
		if (exhausted)
			return false;

		const Handle& pattern_elem = pattern[ip];

		// Check if this pattern element is a glob
		bool is_glob = variables->is_globby(pattern_elem);

		if (!is_glob)
		{
			// ---- NON-GLOB MATCHING ----

			// Out of ground elements?
			if (jg >= ground.size())
			{
				backtrack(false);
				continue;
			}

			// Would matching leave unmatched ground at the end?
			// (We're at the last pattern element but have >1 ground left)
			if (ip + 1 == pattern_end && jg + 1 < ground.size())
			{
				backtrack(false);
				continue;
			}

			// Try to match this pattern element
			if (!validate(pattern_elem, ground[jg], bindings))
			{
				backtrack(false);
				continue;
			}

			// Success - advance both indices
			ip++;
			jg++;
		}
		else
		{
			// ---- GLOB MATCHING ----

			Handle glob = pattern_elem;

			// Check if we already bound this glob earlier in the pattern
			auto existing_binding = bindings.find(glob);
			if (existing_binding != bindings.end() && !backtracking)
			{
				// This glob was matched before - verify consistency
				// The binding should be a List or LinkValue containing the matched elements

				ValuePtr prev_value = existing_binding->second;
				size_t prev_size = 0;

				// Extract the size of the previous match
				if (prev_value->is_atom())
				{
					Handle h = HandleCast(prev_value);
					if (h->is_link())
						prev_size = h->get_arity();
				}
				else if (prev_value->is_type(LINK_VALUE))
				{
					prev_size = LinkValueCast(prev_value)->value().size();
				}

				// Check if we have enough ground elements
				if (jg + prev_size > ground.size())
				{
					backtrack(false);
					continue;
				}

				// Verify each element matches
				bool mismatch = false;
				if (prev_value->is_atom())
				{
					Handle h = HandleCast(prev_value);
					if (h->is_link())
					{
						const HandleSeq& prev_seq = h->getOutgoingSet();
						for (size_t i = 0; i < prev_size; i++)
						{
							if (ground[jg + i] != prev_seq[i])
							{
								mismatch = true;
								break;
							}
						}
					}
				}
				else if (prev_value->is_type(LINK_VALUE))
				{
					const ValueSeq& prev_seq = LinkValueCast(prev_value)->value();
					for (size_t i = 0; i < prev_size; i++)
					{
						if (ground[jg + i] != prev_seq[i])
						{
							mismatch = true;
							break;
						}
					}
				}

				if (mismatch)
				{
					backtrack(false);
					continue;
				}

				// Consistent - advance ground index and move on
				jg += prev_size;
				ip++;
				continue;
			}

			// This is a new glob match (or we're retrying after backtrack)

			// Get the interval constraints
			GlobInterval interval = variables->get_interval(glob);
			size_t lower_bound = interval.first;
			size_t upper_bound = interval.second;

			// Calculate maximum size we can actually match
			size_t ground_remaining = ground.size() - jg;
			size_t max_size = std::min(upper_bound, ground_remaining);

			// If backtracking, try a smaller size than last time
			auto prev_size_it = glob_sizes.find(glob);
			if (prev_size_it != glob_sizes.end())
			{
				// Try next smaller size
				if (prev_size_it->second > 0)
					max_size = std::min(max_size, prev_size_it->second - 1);
				else
				{
					// Can't go smaller than 0
					backtrack(true);
					continue;
				}
			}

			// Push to stack if not already backtracking from this glob
			if (!backtracking)
			{
				glob_stack.push(GlobStackEntry<GroundSeq>(glob, ip, jg));
			}
			else
			{
				// Reset backtracking flag for next iteration
				backtracking = false;
			}

			// Try sizes from max_size down to lower_bound
			bool found_match = false;
			for (size_t size = max_size; size >= lower_bound && size <= max_size; size--)
			{
				// Extract the subsequence to match
				GroundSeq matched_seq;
				for (size_t i = 0; i < size; i++)
				{
					matched_seq.push_back(ground[jg + i]);
				}

				// Type validation is handled by FilterLink::extract() via _recursive_glob
				// when each element is validated during the matching process

				// Check if this would leave the pattern/ground unbalanced
				// Note: We could do a sophisticated check here counting minimum requirements,
				// but the Variables object may not return correct intervals for TypedVariable globs,
				// so we rely on backtracking instead. Only check the simple case where this is
				// the last pattern element.
				size_t ground_after = ground.size() - (jg + size);

				// Special case: if this is the last pattern element,
				// we must consume exactly all remaining ground
				if (ip + 1 == pattern_end && ground_after != 0)
					continue;  // Try next smaller size

				// This size works! Record the match
				ValuePtr glob_value = make_value(matched_seq);
				bindings[glob] = glob_value;
				glob_sizes[glob] = size;
				jg += size;
				found_match = true;
				break;
			}

			if (!found_match)
			{
				// No valid size found - backtrack
				backtrack(true);
				continue;
			}

			// Success - advance pattern index
			ip++;
		}
	}

	// Check if we consumed all ground elements
	return (jg == ground.size());
}

// Explicit template instantiations for HandleSeq and ValueSeq
template bool glob_match<HandleSeq>(
	const HandleSeq&, const HandleSeq&, ValueMap&, const Variables*,
	GlobValidateCallback<HandleSeq>, GlobMakeValueCallback<HandleSeq>,
	size_t, size_t);

template bool glob_match<ValueSeq>(
	const HandleSeq&, const ValueSeq&, ValueMap&, const Variables*,
	GlobValidateCallback<ValueSeq>, GlobMakeValueCallback<ValueSeq>,
	size_t, size_t);

} // namespace opencog
