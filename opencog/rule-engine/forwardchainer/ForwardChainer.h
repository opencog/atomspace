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

enum source_selection_mode {
    TV_FITNESS_BASED, STI_BASED
};

class FCMemory;
class Logger;
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
    Logger * _log;

    int _iteration = 0;
    source_selection_mode _ts_mode;
    bool _search_in_af;
    bool _search_focus_Set;
    const Rule* _cur_rule;
    Handle _cur_source;
    HandleSeq _selected_sources;

    FCStat _fcstat;

    void init(Handle hsource, HandleSeq focus_set);
    void setLogger(Logger* log);
    Logger* getLogger(void);

    void apply_all_rules(bool search_focus_set = false);

    UnorderedHandleSet get_subatoms(const Rule *rule);
    Handle gen_sub_varlist(const Handle& parent, const Handle& parent_varlist);
    HandleSeq substitute_rule_part(AtomSpace& as, Handle hrule,
                                   const std::set<Handle>& vars,
                                   const std::vector<std::map<Handle, Handle>>&
                                   var_groundings);
    bool unify(Handle source, Handle target, const Rule* rule);
    bool subatom_unify(Handle source, const Rule* rule);
    HandleSeq derive_rules(Handle source, Handle target, const Rule* rule);
    void update_potential_sources(HandleSeq input);

    bool is_valid_implicant(const Handle& h);
    void validate(Handle hsource, HandleSeq hfocus_set);

protected:
    vector<Rule*> _rules; /*<loaded rules*/
    HandleSeq _potential_sources;
    HandleSeq _focus_set;

    /**
     * Choose an applicable rules from the rule base by selecting
     * rules whose premise structurally matches with the source.
     *
     * @return  A rule that in which @param source could ground.
     */
    virtual Rule* choose_rule(Handle hsource, bool subatom_match);

    /**
     * choose next source from the source list
     *
     * @return  A handle to the chosen source from source list
     */
    virtual Handle choose_next_source(void);

    /**
     * Apply chosen rule. the default will wrap a custom PM callback class.
     * i.e invokes _pattern_matcher.
     *
     * @return  A set of handles created as a result of applying current
     *          choosen rule.
     */
    virtual HandleSeq apply_rule(Handle rhandle, bool search_focus_set_only =
            false);

    HandleSeq derive_rules(Handle source, const Rule* rule, bool subatomic = false);

public:
    /**
     * Ctor. rbs is a Handle pointing to rule-based system.
     */
    ForwardChainer(AtomSpace& as, Handle rbs, Handle hsource,
                   HandleSeq focus_set);
    virtual ~ForwardChainer();

    void do_chain(void);
    void do_step(void);
    HandleSeq get_chaining_result(void);
};

} // ~namespace opencog

#endif /* FORWARDCHAINERX_H_ */
