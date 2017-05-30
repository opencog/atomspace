/*
 * ForwardChainer.h
 *
 * Copyright (C) 2014,2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#ifndef FORWARDCHAINERX_H_
#define FORWARDCHAINERX_H_

#include <opencog/rule-engine/URECommons.h>
#include <opencog/rule-engine/UREConfigReader.h>

#include "FCStat.h"

class ForwardChainerUTest;

namespace opencog
{

enum class source_selection_mode
{
	TV_FITNESS, STI, UNIFORM
};

class Rule;

class ForwardChainer
{
private:
	friend class ::ForwardChainerUTest;

	AtomSpace& _as;

	// The focus set is copied into this atomspace; during chaining,
	// the pattern matcher is applied only to this atomspace.  This
	// is the primary mechanism by which chaining is restricted to
	// the focus set.  This is effective, but not very efficient;
	// perhaps there is some better mechanism?
	AtomSpace _focus_set_as;

	URECommons _rec;            // utility class
	Handle _rbs;                // rule-based system
	UREConfigReader _configReader;

	int _iteration;
	source_selection_mode _ts_mode;
	bool _search_in_af;
	bool _search_focus_set;
	Handle _init_source;
	Handle _init_vardecl;
	Handle _cur_source;

	// We maintain both selected and unselected sources, to speed up
	// choose_source()
	UnorderedHandleSet _selected_sources;
	UnorderedHandleSet _unselected_sources;

	FCStat _fcstat;

	void init(const Handle& source,
	          const Handle& vardecl,
	          const HandleSeq& focus_set);

	void apply_all_rules();

	template<typename HandleContainer>
	void update_potential_sources(const HandleContainer& input)
		{
			UnorderedHandleSet input_minus_selected;
			for (const Handle& h : input)
				if (_selected_sources.find(h) == _selected_sources.end())
					input_minus_selected.insert(h);
			_potential_sources.insert(input_minus_selected.begin(),
			                          input_minus_selected.end());
			_unselected_sources.insert(input_minus_selected.begin(),
			                           input_minus_selected.end());
		}

	void validate(const Handle& source);

	void expand_meta_rules();

protected:
	RuleSet _rules; /* loaded rules */
	UnorderedHandleSet _potential_sources;
	HandleSeq _focus_set;

	/**
	 * choose next source from the source list
	 *
	 * @return  A handle to the chosen source from source list
	 */
	Handle select_source();

	/**
	 * Choose an applicable rules from the rule base by selecting
	 * rules whose premise structurally matches with the source.
	 *
	 * If no rule can be chosen return nullptr.
	 *
	 * @return  A rule that in which @param source could ground.
	 */
	Rule select_rule(const Handle& source);

	/**
	 * Apply rule.
	 */
	UnorderedHandleSet apply_rule(const Rule& rule);

public:
	/**
	 * Ctor. rbs is a Handle pointing to rule-based system.
	 */
	ForwardChainer(AtomSpace& as, const Handle& rbs, const Handle& source,
	               const Handle& vardecl=Handle::UNDEFINED,
	               const HandleSeq& focus_set=HandleSeq(),
	               source_selection_mode sm=source_selection_mode::UNIFORM);
	~ForwardChainer();

	/**
	 * URE configuration accessors
	 */
	UREConfigReader& get_config();
	const UREConfigReader& get_config() const;

	/**
	 * Perform forward chaining inference till the termination
	 * criteria have been met.
	 */
	void do_chain();

	/**
	 * Perform a single forward chaining inference step.
	 */
	void do_step();

	/**
	 * @return true if the termination criteria have been met.
	 */
	bool termination();

	/**
	 * @return all results in their order of inference.
	 */
	UnorderedHandleSet get_chaining_result();
};

} // ~namespace opencog

#endif /* FORWARDCHAINERX_H_ */
