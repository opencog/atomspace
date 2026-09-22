/*
 * opencog/atoms/base/FreeHypermultiset.h
 *
 * Copyright (C) 2026 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_FREE_HYPERMULTISET_H
#define _OPENCOG_FREE_HYPERMULTISET_H

#include <cstddef>
#include <vector>

#include <boost/multiprecision/cpp_int.hpp>

namespace opencog
{
/**
 * Count finite free hypermultisets by their number of non-skin membranes.
 *
 * Free hypermultisets are equivalent to unlabeled, unordered rooted trees.
 * Thus, the count for n membranes is the number of rooted trees with n + 1
 * nodes (OEIS A000081).
 */
using FreeHypermultisetCount = boost::multiprecision::cpp_int;

/**
 * Return the number of free hypermultisets with exactly `membranes`
 * non-skin membranes.
 */
FreeHypermultisetCount free_hypermultiset_count(std::size_t membranes);

/**
 * Return counts indexed by their number of non-skin membranes from zero
 * through `max_membranes`, inclusive.
 */
std::vector<FreeHypermultisetCount>
free_hypermultiset_counts(std::size_t max_membranes);

} // namespace opencog

#endif // _OPENCOG_FREE_HYPERMULTISET_H
