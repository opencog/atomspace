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

enum class source_selection_mode {
	TV_FITNESS, STI, UNIFORM
};

class FCMemory;
class Rule;

class ForwardChainer {
private:
    friend class ::ForwardChainerUTest;

    AtomSpace& _as;
    //This restricts PM to look only in the focus set
    AtomSpace _focus_set_as;

    URECommons _rec;            // utility class
    Handle _rbs;                // rule-based system
    UREConfigReader _configReader;

    int _iteration;
    int _max_iteration;
    source_selection_mode _ts_mode;
    bool _search_in_af;
    bool _search_focus_set;
    Handle _cur_source;

	// We maintain both a selected and unselected sources to speed up
	// choose_source
    UnorderedHandleSet _selected_sources;
    UnorderedHandleSet _unselected_sources;

    FCStat _fcstat;

    void init(const Handle& hsource, const HandleSeq& focus_set);

    void apply_all_rules();
	
    Handle gen_sub_varlist(const Handle& parent, const Handle& parent_varlist);
    bool is_constant_clause(const Handle& hvarlist, const Handle& hclause) const;
    Handle remove_constant_clauses(const Handle& hvarlist,
                                   const Handle& himplicand);
    HandleSeq substitute_rule_part(AtomSpace& as, const Handle& hrule,
                                   const OrderedHandleSet& vars,
                                   const HandleMapSeq&
                                   var_groundings);
    bool unify(const Handle& source, const Handle& pattern, const Rule* rule);
    UnorderedHandleSet derive_rules(const Handle& source, const Handle& pattern,
                                    const Rule* rule);
	template<typename HandleContainer>
    void update_potential_sources(const HandleContainer& input) {
		UnorderedHandleSet input_minus_selected;
		for (const Handle& h : input)
			if (_selected_sources.find(h) == _selected_sources.end())
				input_minus_selected.insert(h);
		_potential_sources.insert(input_minus_selected.begin(),
		                          input_minus_selected.end());
        _unselected_sources.insert(input_minus_selected.begin(),
                                   input_minus_selected.end());
    }
    bool is_valid_implicant(const Handle& h);
    void validate(const Handle& hsource, const HandleSeq& hfocus_set);

protected:
    vector<Rule*> _rules; /*<loaded rules*/
    UnorderedHandleSet _potential_sources;
    HandleSeq _focus_set;

    /**
     * choose next source from the source list
     *
     * @return  A handle to the chosen source from source list
     */
    virtual Handle select_source();

	/**
     * Choose an applicable rules from the rule base by selecting
     * rules whose premise structurally matches with the source.
     *
     * If no rule can be chosen return nullptr.
     *
     * @return  A rule that in which @param source could ground.
     */
    virtual Rule* select_rule(const Handle& hsource);

	/**
	 * Apply rule on the current source. Creating derived rules if
	 * necessary.
	 */
    virtual UnorderedHandleSet apply_rule(const Rule* rule);

	/**
     * Apply rule handle (BindLink).
     *
     * @return  A set of handles created as a result of applying current
     *          choosen rule.
     */
    virtual HandleSeq apply_rule(const Handle& rhandle);

    UnorderedHandleSet derive_rules(const Handle& source, const Rule* rule);

public:
    /**
     * Ctor. rbs is a Handle pointing to rule-based system.
     */
    ForwardChainer(AtomSpace& as, const Handle& rbs, const Handle& hsource,
                   const HandleSeq& focus_set = HandleSeq(),
                   source_selection_mode sm = source_selection_mode::UNIFORM);
    virtual ~ForwardChainer();

    /**
     * Perform a single forward chaining inference step.
     */
    void do_step();

    /**
     * Perform forward chaining inference till the termination
     * criteria have been met.
     */
    void do_chain();

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
