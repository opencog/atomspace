/*
 * SourceSet.cc
 *
 * Copyright (C) 2018 SingularityNET Foundation
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

#include "SourceSet.h"

#include <boost/range/algorithm/find.hpp>
#include <boost/range/algorithm/lower_bound.hpp>

#include <opencog/util/numeric.h>

namespace opencog {

Source::Source(const Handle& bdy, const Handle& vdcl, double cpx)
	: body(bdy), vardecl(vdcl), complexity(cpx), exhausted(false)
{
}

bool Source::operator==(const Source& other) const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	return body == other.body && vardecl == other.vardecl;
}

bool Source::operator<(const Source& other) const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	// Sort by complexity to so that simpler sources come first. Then
	// by content. Makes it easier to prune by complexity. It should
	// also make sampling a bit faster. And finally the user probabably
	// want that.
	return (complexity < other.complexity)
		or (complexity == other.complexity
		    and (content_based_handle_less()(body, other.body)
		         or (body == other.body
		             and content_based_handle_less()(vardecl, other.vardecl))));
}

void Source::reset_exhausted()
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	exhausted = false;
	rules.clear();
}

bool Source::is_exhausted(const Rule& pos_rule) const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	for (const Rule& rule : rules)
		if (pos_rule.is_alpha_equivalent(rule))
			return true;
	return false;
}

double Source::expand_complexity(double prob) const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	return complexity - log2(prob);
}

std::string Source::to_string(const std::string& indent) const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	std::stringstream ss;
	ss << indent << "body:" << std::endl
	   << oc_to_string(body, indent + oc_to_string_indent) << std::endl
	   << indent << "vardecl:" << std::endl
	   << oc_to_string(vardecl, indent + oc_to_string_indent) << std::endl
	   << indent << "complexity: " << complexity << std::endl
	   << indent << "exhausted: " <<  exhausted << std::endl
	   << indent << "rules:" << std::endl
	   << rules.to_short_string(indent + oc_to_string_indent);
	return ss.str();
}

SourceSet::SourceSet(const UREConfig& config,
                     const Handle& init_source,
                     const Handle& init_vardecl)
	: exhausted(false), _config(config)
{
	if (init_source) {
		// Accept set of initial sources wrapped in a SetLink
		HandleSeq init_sources = init_source->get_type() == SET_LINK ?
			init_source->getOutgoingSet() : HandleSeq{init_source};
		if (init_sources.empty()) {
			exhausted = true;
		} else {
			for (const Handle& src : init_sources)
				sources.push_back(new Source(src, init_vardecl));
		}
	} else {
		exhausted = true;
	}
}

std::vector<double> SourceSet::get_weights() const
{
	std::vector<double> results;
	for (const Source& src : sources)
		results.push_back(get_weight(src));
	return results;
}

void SourceSet::reset_exhausted()
{
	if (sources.empty()) {
		exhausted = true;
		return;
	}

	for (Source& src : sources)
		src.reset_exhausted();
	exhausted = false;
}

void SourceSet::insert(const HandleSet& products, const Source& src, double prob)
{
	const static Handle empty_variable_list = Handle(createVariableList(HandleSeq()));

	// Calculate the complexity of the new sources
	double new_cpx = src.expand_complexity(prob);

	// Insert all new sources
	for (const Handle& product :  products) {
		Source* new_src = new Source(product, empty_variable_list, new_cpx);

		// Make sure it isn't already in the sources
		if (boost::find(sources, *new_src) != sources.end()) {
			LAZY_URE_LOG_DEBUG << "The following source is already in the population: "
			                   << new_src->body->id_to_string();
			delete new_src;
			continue;
		}

		// Otherwise, insert it while preserving the order
		auto ptr_less = [](const Source& ls, const Source* rs) {
			                return ls < *rs; };
		sources.insert(boost::lower_bound(sources, new_src, ptr_less), new_src);
	}
}

size_t SourceSet::size() const
{
	return sources.size();
}

bool SourceSet::empty() const
{
	return sources.empty();
}

std::string SourceSet::to_string(const std::string& indent) const
{
	return oc_to_string(sources, indent);
}

double SourceSet::get_weight(const Source& src) const
{
	// TODO: we could take into account some sort of fitness. For
	// instance if the fitness is maximize confidence, then we could
	// factor in the confidence of the source, as the higher the
	// confidence of the source, the higher the confidence of the
	// conclusion.
	return src.exhausted ? 0.0 : complexity_factor(src);
}

double SourceSet::complexity_factor(const Source& src) const
{
	return exp(-_config.get_complexity_penalty() * src.complexity);
}

std::string oc_to_string(const Source& src, const std::string& indent)
{
	return src.to_string(indent);
}

std::string oc_to_string(const SourceSet::Sources& sources, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << sources.size() << std::endl;
	size_t i = 0;
	for (const Source& src : sources) {
		ss << indent << "Source[" << i << "]:" << std::endl
		   << src.to_string(indent + oc_to_string_indent);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const SourceSet& sources, const std::string& indent)
{
	return sources.to_string(indent);
}

} // ~namespace opencog
