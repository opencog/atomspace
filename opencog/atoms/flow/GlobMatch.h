/*
 * opencog/atoms/flow/GlobMatch.h
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

#ifndef _OPENCOG_GLOB_MATCH_H
#define _OPENCOG_GLOB_MATCH_H

#include <functional>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/scope/Variables.h>

namespace opencog
{

/**
 * Callback types for domain-specific operations.
 *
 * GlobMatch uses these callbacks to delegate operations that require
 * domain knowledge (validation, result construction, etc.)
 */

/// Callback to validate if a pattern element matches a ground element.
/// Used for non-glob pattern elements only. Glob element validation
/// is handled internally using Variables::is_type().
///
/// @param pattern_elem  Pattern element to match
/// @param ground_elem   Ground element to match against
/// @param bindings      Accumulating variable bindings (may be updated)
/// @return true if match is valid, false otherwise
template<typename GroundSeq>
using GlobValidateCallback = std::function<
	bool(const Handle& pattern_elem,
	     const typename GroundSeq::value_type& ground_elem,
	     ValueMap& bindings)
>;

/// Callback to create a Value from a matched sequence.
/// Used to construct the glob binding value (e.g., LinkValue or List).
///
/// @param matched_seq  Sequence of ground elements matched by a glob
/// @return ValuePtr wrapping the matched sequence
template<typename GroundSeq>
using GlobMakeValueCallback = std::function<
	ValuePtr(const GroundSeq& matched_seq)
>;

/**
 * Perform glob matching with backtracking.
 *
 * Matches a pattern sequence (which may contain glob nodes) against
 * a ground sequence, using backtracking to find valid glob bindings.
 *
 * The algorithm tries to match each glob from its maximum allowed size
 * down to its minimum, backtracking when later constraints fail. This
 * ensures that all valid matchings are explored.
 *
 * @param pattern       Pattern sequence (may contain globs and variables)
 * @param ground        Ground sequence to match against
 * @param bindings      Output map of variables/globs to their bindings
 * @param variables     Variables object providing type constraints and intervals
 * @param validate      Callback to validate non-glob pattern elements
 * @param make_value    Callback to create value from matched sequence
 * @param pattern_start Index in pattern to start matching from
 * @param pattern_size  Number of pattern elements to match
 *
 * @return true if pattern matches ground with valid bindings, false otherwise
 *
 * Example usage in FilterLink:
 *   auto validate = [&](const Handle& p, const ValuePtr& g, ValueMap& b) {
 *       return this->extract(p, g, b, scratch, silent, quotation);
 *   };
 *   auto make_value = [](const ValueSeq& seq) {
 *       return createLinkValue(seq);
 *   };
 *   bool matched = glob_match(pattern, ground, bindings, _mvars,
 *                             validate, make_value, offset, size);
 */
template<typename GroundSeq>
bool glob_match(
	const HandleSeq& pattern,
	const GroundSeq& ground,
	ValueMap& bindings,
	const Variables* variables,
	GlobValidateCallback<GroundSeq> validate,
	GlobMakeValueCallback<GroundSeq> make_value,
	size_t pattern_start = 0,
	size_t pattern_size = SIZE_MAX
);

} // namespace opencog

#endif // _OPENCOG_GLOB_MATCH_H
