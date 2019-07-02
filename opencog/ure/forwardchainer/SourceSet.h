/*
 * SourceSet.h
 *
 * Copyright (C) 2018 SingularityNET Foundation
 *
 * Author: Nil Geisweiller
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

#ifndef _OPENCOG_SOURCESET_H_
#define _OPENCOG_SOURCESET_H_

#include <vector>
#include <mutex>

#include <boost/operators.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>

#include "../Rule.h"
#include "../UREConfig.h"

namespace opencog
{

/**
 * Each source is associated to
 *
 * 1. The source body and its variable declaration
 *
 * 2. a complexity (reflecting the probability that expanding it will
 *    fulfill the objective),
 *
 * 3. the set of rules that have expanded it so far,
 *
 * 4. a flag call indicating if the source expansions have been exhausted.
 */
// TODO: this class has thing in common with AndBIT, maybe their
// common things could be placed in a parent class.
class Source : public boost::totally_ordered<Source>
{
public:
	Source(const Handle& body,
	       const Handle& vardecl=Handle::UNDEFINED,
	       double complexity=0.0);

	/**
	 * Comparison operators. For operator< compare by complexity, or by
	 * handle value if they are of the same size.
	 */
	bool operator==(const Source& other) const;
	bool operator<(const Source& other) const;

	/**
	 * Set exhausted flag back to false, and erase tried rules
	 */
	void reset_exhausted();

	/**
	 * Check if the given rule is has been tried already
	 */
	bool is_exhausted(const Rule& rule) const;

	/**
	 * Return the complexity of new source expanded from this source by
	 * a rule with probability of success prob.
	 */
	double expand_complexity(double prob) const;

	std::string to_string(const std::string& indent=empty_string) const;

	// Body of the source
	Handle body;

	// Variable declaration, if any, associated to body
	Handle vardecl;

	// Sum of the complexities of the steps involved in producing it.
	double complexity;

	// True iff all rules that could expand the source have been tried
	bool exhausted;

	// Rules so far applied to that source
	RuleSet rules;

private:
	// NEXT TODO: subdivide in smaller and shared mutexes
	mutable std::mutex _whole_mutex;
};

/**
 * Population of sources to forwardly expand.
 */
// TODO: this class has things in common with BIT, maybe their common
// things could be placed in a parent class.
class SourceSet
{
public:
	SourceSet(const UREConfig& config,
	          const Handle& init_source,
	          const Handle& init_vardecl);

	/**
	 * Return a sequence of weights (probability estimate up to a
	 * normalizing factor) of picking the corresponding source.
	 */
	std::vector<double> get_weights() const;

	/**
	 * When new inference rules come in or we get to retry exhausted
	 * sources, then reset exhausted flags.
	 */
	void reset_exhausted();

	/**
	 * Insert produced sources from src into the population, by
	 * applying rule with a given probability of success prob (useful
	 * for calculating complexity).
	 */
	void insert(const HandleSet& products, const Source& src, double prob);

	size_t size() const;

	bool empty() const;

	std::string to_string(const std::string& indent=empty_string) const;

	// Collection of sources. We use a sorted vector instead of a set
	// because the source being expanded is modified (it keeps track of
	// its expansion rules). Alternatively we could use a set and
	// define Sources::rules as mutable.
	typedef boost::ptr_vector<Source> Sources;
	Sources sources;

	// True iff all sources have been tried
	bool exhausted;

private:
	/**
	 * Calculate the weight (probability estimate up to a normalizing
	 * factor) of expanding src.
	 */
	double get_weight(const Source& src) const;
	double complexity_factor(const Source& src) const;

	const UREConfig& _config;
};

std::string oc_to_string(const Source& source,
                         const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet::Sources& sources,
                         const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet& sources,
                         const std::string& indent=empty_string);

} // ~namespace opencog

#endif /* _OPENCOG_SOURCESET_H_ */
