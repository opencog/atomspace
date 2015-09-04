/*
 * ForwardChainer.h
 *
 * Copyright (C) 2014,2015 Misgana Bayetta
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

#include "FCMemory.h"

class ForwardChainerUTest;
class ForwardChainerCallBackUTest;

namespace opencog
{

enum source_selection_mode {
    TV_FITNESS_BASED, STI_BASED
};

class URECommons;
class UREConfigReader;
class FCMemory;
class Logger;
class Rule;

class ForwardChainer {
private:
    friend class ::ForwardChainerUTest;
    friend class ::ForwardChainerCallBackUTest;

    AtomSpace& _as;
    URECommons _rec;            // utility class
	Handle _rbs;                // rule-based system
	UREConfigReader _configReader;
    FCMemory _fcmem;            // stores history
    Logger * _log;
    int _iteration = 0;
    source_selection_mode _ts_mode;
    std::map<Rule*, float> rule_weight;

    void init();
    void add_to_source_list(Handle h);

    void do_pm();
    void do_pm(const Handle& hsource, const UnorderedHandleSet& var_nodes);
    UnorderedHandleSet do_step(bool search_focus_set = false);
    bool is_valid_implicant(const Handle& h);
    UnorderedHandleSet get_subatoms(Rule *rule);
    Handle gen_sub_varlist(const Handle& parent, const Handle& parent_varlist);
    HandleSeq substitute_rule_part(
            AtomSpace& as, Handle hrule, const std::set<Handle>& vars,
            const std::vector<std::map<Handle, Handle>>& var_groundings);
    bool unify(Handle source, Handle target, Rule* rule);
    bool subatom_unify(Handle source, Rule* rule);
    HandleSeq derive_rules(Handle source, Handle target, Rule* rule);

protected:
    /**
     * Choose a set of applicable rules from the rule base by selecting
     * rules whose premise structurally matches with the source.
     * @fcmem an object holding the current source/target and other inform
     * ation of the forward chaining instance.
     * @return a set of applicable rules
     */
    virtual std::vector<Rule*> choose_rules(FCMemory& fcmem);
    /**
     * Choose additional premises for the rule.
     * @fcmem an object holding the current source/target and other inform
     * ation of the forward chaining instance.
     * @return a set of Handles chosen as a result of applying fitness
     * criteria with respect to the current source.
     */
    virtual HandleSeq choose_premises(FCMemory& fcmem);
    /**
     * choose next source from the source list
     * @return a handle to the chosen source from source list
     */
    virtual Handle choose_next_source(FCMemory& fcmem);
    /**
     * apply chosen rule. the default will wrap a custom PM callback class.
     * i.e invokes _pattern_matcher.
     * @return a set of handles created as a result of applying current choosen rule
     */
    virtual HandleSeq apply_rule(Handle rhandle, bool search_focus_set_only =
            false);

    HandleSeq derive_rules(Handle source, Rule* rule, bool subatomic = false);

public:
	/**
	 * Ctor. rbs is a Handle pointing to rule-based system.
	 */
    ForwardChainer(AtomSpace& as, Handle rbs);

    void do_chain(Handle hsource =
            Handle::UNDEFINED,HandleSeq focus_set = {},bool search_focus_set = false);

    HandleSeq get_chaining_result(void);

    void setLogger(Logger* log);
    Logger* getLogger(void);
};

} // ~namespace opencog

#endif /* FORWARDCHAINERX_H_ */
