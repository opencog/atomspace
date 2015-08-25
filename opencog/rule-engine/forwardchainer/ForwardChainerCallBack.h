/*
 * ForwardChainerCallBack.h
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

#ifndef FORWARDCHAINERCALLBACK_H_
#define FORWARDCHAINERCALLBACK_H_

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Handle.h>

namespace opencog {

enum source_selection_mode {
    TV_FITNESS_BASED, STI_BASED
};

class Rule;
class FCMemory;
class ForwardChainerCallBack
{
private:
    AtomSpace* as_;

public:
    ForwardChainerCallBack(AtomSpace* as) :
            as_(as)
    {
    }
    virtual ~ForwardChainerCallBack()
    {
    }
    /**
     * Choose a set of applicable rules from the rule base by selecting
     * rules whose premise structurally matches with the source.
     * @fcmem an object holding the current source/target and other inform
     * ation of the forward chaining instance.
     * @return a set of applicable rules
     */
    virtual std::vector<Rule*> choose_rules(FCMemory& fcmem) = 0;
    /**
     * Choose additional premises for the rule.
     * @fcmem an object holding the current source/target and other inform
     * ation of the forward chaining instance.
     * @return a set of Handles chosen as a result of applying fitness
     * criteria with respect to the current source.
     */
    virtual HandleSeq choose_premises(FCMemory& fcmem) = 0;
    /**
     * choose next source from the source list
     * @return a handle to the chosen source from source list
     */
    virtual Handle choose_next_source(FCMemory& fcmem) = 0;
    /**
     * apply chosen rule. the default will wrap a custom PM callback class.
     * i.e invokes _pattern_matcher.
     * @return a set of handles created as a result of applying current choosen rule
     */
    virtual HandleSeq apply_rule(FCMemory& fcmem) = 0;

    HandleSeq derive_rules(Handle source,Rule* rule,bool subatomic=false);
};


/**
 * Derives new rules by replacing variables that are unfiable in @param target
 *  with source.
 *
 * @param  source    A source atom that will be matched with the rule.
 * @param  rule      A rule object that contains @param target in its implicant.
 * @param  subatomic A flag that sets subatom unification.
 *
 * @return  A HandleSeq of derived rule handles.
 */
HandleSeq ForwardChainerCallBack::derive_rules(Handle source, Handle target,
                                               Rule* rule)
{
    //exceptions
    if (not is_valid_implicant(target))
        return {};

    HandleSeq derived_rules = { };

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(target);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(target, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    BindLinkPtr bl =
    createBindLink(HandleSeq { implicant_vardecl, hcpy, hcpy });

    VarGroundingPMCB gcb(&temp_pm_as);
    gcb.implicand = bl->get_implicand();

    bl->imply(gcb);

    auto del_by_value =
            [] (std::vector<std::map<Handle,Handle>>& vec_map,const Handle& h) {
                for (auto& map: vec_map)
                for(auto& it:map) {if (it.second == h) map.erase(it.first);}
            };

    //We don't want implicant_var list to be matched as
    //in the case of free vars in modus-ponens rules.
    del_by_value(gcb.term_groundings, implicant_vardecl);
    del_by_value(gcb.var_groundings, implicant_vardecl);

    FindAtoms fv(VARIABLE_NODE);
    for (const auto& termg_map : gcb.term_groundings) {
        for (const auto& it : termg_map) {
            if (it.second == sourcecpy) {

                fv.search_set(it.first);

                Handle rhandle = rule->get_handle();
                HandleSeq new_candidate_rules = substitute_rule_part(
                        temp_pm_as, temp_pm_as.add_atom(rhandle), fv.varset,
                        gcb.var_groundings);

                for (Handle nr : new_candidate_rules) {
                    if (find(derived_rules.begin(), derived_rules.end(), nr) == derived_rules.end()) {
                        //Adding back to _as avoids UUID clashes.
                        Handle h = _as.add_atom(nr);
                        //Avoid adding original rule to derived rule list
                        if (h != rhandle)
                            derived_rules.push_back(h);
                    }
                }
            }
        }
    }

    return derived_rules;
}

} // ~namespace opencog

#endif /* FORWARDCHAINERCALLBACK_H_ */
