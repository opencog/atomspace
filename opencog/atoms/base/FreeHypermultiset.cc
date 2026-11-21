/*
 * opencog/atoms/base/FreeHypermultiset.cc
 *
 * Copyright (C) 2026 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/base/FreeHypermultiset.h>

#include <limits>
#include <stdexcept>

namespace opencog
{
namespace
{
FreeHypermultisetCount checked_add(FreeHypermultisetCount lhs,
                                   FreeHypermultisetCount rhs)
{
	const FreeHypermultisetCount max =
		std::numeric_limits<FreeHypermultisetCount>::max();
	if (rhs > max - lhs)
		throw std::overflow_error("Free hypermultiset count is too large");
	return lhs + rhs;
}

FreeHypermultisetCount checked_multiply(FreeHypermultisetCount lhs,
                                        FreeHypermultisetCount rhs)
{
	const FreeHypermultisetCount max =
		std::numeric_limits<FreeHypermultisetCount>::max();
	if (lhs != 0 && rhs > max / lhs)
		throw std::overflow_error("Free hypermultiset count is too large");
	return lhs * rhs;
}
}

std::vector<FreeHypermultisetCount>
free_hypermultiset_counts(std::size_t max_membranes)
{
	if (max_membranes > std::numeric_limits<std::size_t>::max() - 2)
		throw std::length_error("Free hypermultiset membrane bound is too large");

	// Let T(n) count rooted trees with n nodes. Otter's recurrence is
	// n*T(n) = sum(k=1..n-1, T(n-k)*sum(d|k, d*T(d))).
	// M(m) = T(m+1) is the free-hypermultiset count requested here.
	const std::size_t max_nodes = max_membranes + 1;
	std::vector<FreeHypermultisetCount> trees(max_nodes + 1);
	std::vector<FreeHypermultisetCount> divisor_sums(max_nodes + 1);
	trees[1] = 1;
	for (std::size_t multiple = 1; multiple <= max_nodes; ++multiple)
		divisor_sums[multiple] = 1;

	for (std::size_t nodes = 2; nodes <= max_nodes; ++nodes)
	{
		FreeHypermultisetCount sum = 0;
		for (std::size_t part = 1; part < nodes; ++part)
			sum = checked_add(sum, checked_multiply(
				trees[nodes - part], divisor_sums[part]));
		trees[nodes] = sum / (nodes - 1);

		for (std::size_t multiple = nodes; multiple <= max_nodes;)
		{
			if (static_cast<std::uintmax_t>(nodes) >
			    std::numeric_limits<FreeHypermultisetCount>::max())
				throw std::overflow_error(
					"Free hypermultiset count is too large");
			const FreeHypermultisetCount node_count =
				static_cast<FreeHypermultisetCount>(nodes);
			divisor_sums[multiple] = checked_add(divisor_sums[multiple],
				checked_multiply(node_count, trees[nodes]));
			if (multiple > max_nodes - nodes)
				break;
			multiple += nodes;
		}
	}

	std::vector<FreeHypermultisetCount> counts;
	counts.reserve(max_membranes + 1);
	for (std::size_t membranes = 0; membranes <= max_membranes; ++membranes)
		counts.emplace_back(trees[membranes + 1]);
	return counts;
}

FreeHypermultisetCount free_hypermultiset_count(std::size_t membranes)
{
	return free_hypermultiset_counts(membranes).back();
}

} // namespace opencog
