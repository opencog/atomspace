/*
 * DefaultForwardChainerCB.cc
 *
 * Copyright (C) 2015 Misgana Bayetta
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

#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/atoms/bind/BindLink.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/query/PatternMatch.cc>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/rule-engine/URECommons.h>

#include "VarGroundingPMCB.h"
#include "DefaultForwardChainerCB.h"

using namespace opencog;

DefaultForwardChainerCB::DefaultForwardChainerCB(AtomSpace& as,
                                                 source_selection_mode ts_mode
                                                 /*=TV_FITNESS_BASED*/) :
        ForwardChainerCallBack(&as), _as(as), _fcpm(&as), _ts_mode(ts_mode)
{
}

/**
 * Choose rules based on implicant member of rule matching
 * the source. Whenever a matching rule is found, it derives
 * new rules by substituting bound variables of the rule with
 * the source. All derived rules are then kept in the global
 * variable @param rule_derivations.
 *
 * @param fcmem Forward chainer's working memory
 *
 * @return      A vector of chosen rules
 */
vector<Rule*> DefaultForwardChainerCB::choose_rules(FCMemory& fcmem)
{
    Handle source = fcmem.get_cur_source();
    if (source == Handle::UNDEFINED)
        throw InvalidParamException(TRACE_INFO, "Needs a valid source atom");

    rule_derivations.clear(); //clear previous mapping

    vector<Rule*> chosen_rules;
    auto rules = fcmem.get_rules();

    for (Rule* rule : rules) {
        HandleSeq derived_rules = unify(source,rule);

        //Chosen rule.
        if (not derived_rules.empty())
        {
            chosen_rules.push_back(rule);
            rule_derivations[rule->get_handle()] = derived_rules;
        }
    }

    return chosen_rules;
}

/**
 * Tries to unify the @param source with @parama target and derives
 * new rules using @param rule as a template.
 *
 * @param source  An atom that might bind to variables in @param rule.
 * @param target  An atom to be unified with @param source
 * @rule  rule    The rule object whose implicants are to be unified.
 *
 * @return        HandleSeq of possible derivation of rules by substituting
 *                variables in implicants members of @param rule with their
 *                mapped bindings.
 */
HandleSeq DefaultForwardChainerCB::unify(Handle source,Handle target,Rule* rule){
    //exceptions
    if(not is_valid_implicant(target)) return {};

    HandleSeq derived_rules={};

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
                   for(auto& it:map) { if (it.second == h) map.erase(it.first);}
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
/**
 *  Tries to unify sub atoms of implicant lists in @param rule with @param source.
 *
 *  @param source  An atom that might bind to variables in @param rule.
 *  @param rule    The rule object whose implicants are to be sub atom unified.
 *
 *  @return        HandleSeq of derived rules from sub atoms unification.
 */
HandleSeq DefaultForwardChainerCB::subatom_unify(Handle source,Rule* rule)
{
    HandleSeq derived_rules;
    HandleSeq impl_members = rule->get_implicant_seq();

    for (Handle h : impl_members) {
        UnorderedHandleSet hs = get_all_unique_atoms(h);
        hs.erase(h); //Already tried to unify this.

        HandleSeq result = unify(source, rule);
        derived_rules.insert(derived_rules.begin(), result.begin(),
                             result.end());

    }

        return derived_rules;
}

HandleSeq DefaultForwardChainerCB::choose_premises(FCMemory& fcmem)
{
    HandleSeq inputs;
    URECommons urec(_as);
    Handle hsource = fcmem.get_cur_source();

    // Get everything associated with the source handle.
    UnorderedHandleSet neighbors = get_distant_neighbors(hsource, 2);

    // Add all root links of atoms in @param neighbors.
    for (auto hn : neighbors) {
        if (hn->getType() != VARIABLE_NODE) {
            HandleSeq roots;
            urec.get_root_links(hn, roots);
            for (auto r : roots) {
                if (find(inputs.begin(), inputs.end(), r) == inputs.end() and r->getType()
                        != BIND_LINK)
                    inputs.push_back(r);
            }
        }
    }

    return inputs;
}

Handle DefaultForwardChainerCB::choose_next_source(FCMemory& fcmem)
{
    HandleSeq tlist = fcmem.get_potential_sources();
    map<Handle, float> tournament_elem;
    URECommons urec(_as);
    Handle hchosen = Handle::UNDEFINED;

    for (Handle t : tlist) {
        switch (_ts_mode) {
        case TV_FITNESS_BASED: {
            float fitness = urec.tv_fitness(t);
            tournament_elem[t] = fitness;
        }
            break;
        case STI_BASED:
            tournament_elem[t] = t->getSTI();
            break;
        default:
            throw RuntimeException(TRACE_INFO,
                                   "Unknown source selection mode.");
            break;
        }
    }

    //!Choose a new source that has never been chosen before.
    //!xxx FIXME since same handle might be chosen multiple times the following
    //!code doesn't guarantee all sources have been exhaustively looked.
    for (size_t i = 0; i < tournament_elem.size(); i++) {
        Handle hselected = urec.tournament_select(tournament_elem);
        if (fcmem.isin_selected_sources(hselected)) {
            continue;
        } else {
            hchosen = hselected;
            break;
        }
    }

    // Incase of when all sources are selected
    if (hchosen == Handle::UNDEFINED)
        return urec.tournament_select(tournament_elem);

    return hchosen;
}

HandleSeq DefaultForwardChainerCB::apply_rule(FCMemory& fcmem)
{
    _fcpm.set_fcmem(&fcmem);

    auto rule_handle = fcmem.get_cur_rule()->get_handle();
    BindLinkPtr bl(BindLinkCast(rule_handle));
    if (NULL == bl) {
        bl = createBindLink(*LinkCast(rule_handle));
    }
    _fcpm.implicand = bl->get_implicand();
    bl->imply(_fcpm);
    // bl->satisfy(*_fcpm);

    HandleSeq product = _fcpm.get_products();

    //! Make sure the inferences made are new.
    for (auto iter = product.begin(); iter != product.end();) {
        if (fcmem.isin_potential_sources(*iter))
            iter = product.erase(iter);
        else
            ++iter;
    }

    return product;
}

Handle DefaultForwardChainerCB::gen_sub_varlist(const Handle& parent,
                                                const Handle& parent_varlist)
{
    FindAtoms fv(VARIABLE_NODE);
    fv.search_set(parent);

    HandleSeq oset;
    if (LinkCast(parent_varlist))
        oset = LinkCast(parent_varlist)->getOutgoingSet();
    else
        oset.push_back(parent_varlist);

    HandleSeq final_oset;

    // for each var in varlist, check if it is used in parent
    for (const Handle& h : oset) {
        Type t = h->getType();

        if (VARIABLE_NODE == t && fv.varset.count(h) == 1)
            final_oset.push_back(h);
        else if (TYPED_VARIABLE_LINK == t
                and fv.varset.count(LinkCast(h)->getOutgoingSet()[0]) == 1)
            final_oset.push_back(h);
    }

    return Handle(createVariableList(final_oset));
}

/**
 * Derives new rules from @param hrule by replacing variables
 * with their groundings.
 *
 * @param as             An atomspace where the handles dwell.
 * @param hrule          A handle to BindLink instance
 * @param vars           The grounded var list in @param hrule
 * @param var_groundings The set of groundings to each var in @param vars
 *
 * @return A HandleSeq of all possible derived rules
 */
HandleSeq DefaultForwardChainerCB::substitute_rule_part(
        AtomSpace& as, Handle hrule,const std::set<Handle>& vars,
        const std::vector<std::map<Handle, Handle>>& var_groundings)
{
    std::vector<std::map<Handle, Handle>> filtered_vgmap_list;

    //Filter out variables not listed in vars from var-groundings
    for (const auto& varg_map : var_groundings) {
        std::map<Handle, Handle> filtered;

        for (const auto& iv : varg_map){
            if (find(vars.begin(), vars.end(), iv.first) != vars.end()){
                filtered[iv.first] = iv.second;
            }
        }

        filtered_vgmap_list.push_back(filtered);
    }

    HandleSeq derived_rules;
    BindLinkPtr blptr = BindLinkCast(hrule);
    Substitutor st(&as);

    for (auto& vgmap : filtered_vgmap_list) {
        Handle himplicand = st.substitute(blptr->get_implicand(), vgmap);
        //Create the BindLink/Rule by substituting vars with groundings
        if (contains_atomtype(himplicand, VARIABLE_NODE)) {
            Handle himplicant = st.substitute(blptr->get_body(), vgmap);

            //Assuming himplicant's set of variables are superset for himplicand's,
            //generate varlist from himplicant.
            Handle hvarlist = gen_sub_varlist(
                    himplicant, LinkCast(hrule)->getOutgoingSet()[0]);
            Handle hderived_rule = Handle(LinkCast(createBindLink(HandleSeq {
                    hvarlist, himplicant, himplicand })));
            derived_rules.push_back(hderived_rule);
        }
        else{
            //TODO Execute if executable and push to FC results
        }
    }

    return derived_rules;
}

/**
 * Checks whether an atom can be used to generate a bindLink or not.
 *
 * @param h  The atom handle to be validated.
 *
 * @return   A boolean result of the check.
 */
bool DefaultForwardChainerCB::is_valid_implicant(const Handle& h)
{
    if (h->getType() == ABSENT_LINK)
        return false;

    FindAtoms fv(VARIABLE_NODE);
    fv.search_set(h);

    if (fv.varset.empty())
       return false;

    return true;
}
