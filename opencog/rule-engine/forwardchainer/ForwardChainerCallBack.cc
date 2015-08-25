/*
 * ForwardChainerCallBack.cc
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

#include <opencog/atomutils/AtomUtils.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomutils/Substitutor.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Handle.h>
#include <opencog/atoms/bind/BindLink.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/rule-engine/Rule.h>

#include "ForwardChainerCallBack.h"

using namespace opencog;


HandleSeq ForwardChainerCallBack::apply_rule(Handle rhandle){

    DefaultImplicator impl(_as);

    BindLinkPtr bl(BindLinkCast(rhandle));
    if (NULL == bl) {
        bl = createBindLink(*LinkCast(rhandle));
    }
    impl.implicand = bl->get_implicand();
    bl->imply(impl);

    HandleSeq product = impl.get_result_list();

    return product;
}

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
                        Handle h = _as->add_atom(nr);
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
 * Derives new rules by replacing variables in @param rule that are
 * unfiable with source.
 *
 * @param  source    A source atom that will be matched with the rule.
 * @param  rule      A rule object
 * @param  subatomic A flag that sets subatom unification.
 *
 * @return  A HandleSeq of derived rule handles.
 */
HandleSeq ForwardChainerCallBack::derive_rules(Handle source, Rule* rule,bool subatomic/*=false*/)
{
    HandleSeq derived_rules = { };

    auto add_result = [&derived_rules] (HandleSeq result) {
        derived_rules.insert(derived_rules.end(), result.begin(),
                result.end());
    };

    if (subatomic) {
        for (Handle target : get_subatoms(rule))
            add_result(derive_rules(source, target, rule));

    } else {
        for (Handle target : rule->get_implicant_seq())
            add_result(derive_rules(source, target, rule));

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
bool ForwardChainerCallBack::is_valid_implicant(const Handle& h)
{
    if (h->getType() == ABSENT_LINK)
        return false;

    FindAtoms fv(VARIABLE_NODE);
    fv.search_set(h);

    if (fv.varset.empty())
       return false;

    return true;
}

/**
 * Gets all unique atoms of in the implicant list of @param r.
 *
 * @param r  A rule object
 *
 * @return   An unoderedHandleSet of of all unique atoms in the implicant.
 */
UnorderedHandleSet ForwardChainerCallBack::get_subatoms(Rule *rule)
{
    UnorderedHandleSet output_expanded;

    HandleSeq impl_members = rule->get_implicant_seq();
    for (Handle h : impl_members) {
        UnorderedHandleSet hs = get_all_unique_atoms(h);
        hs.erase(h); //Already tried to unify this.
        output_expanded.insert(hs.begin(), hs.end());
    }

    return output_expanded;
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
HandleSeq ForwardChainerCallBack::substitute_rule_part(
        AtomSpace& as, Handle hrule, const std::set<Handle>& vars,
        const std::vector<std::map<Handle, Handle>>& var_groundings)
{
    std::vector<std::map<Handle, Handle>> filtered_vgmap_list;

    //Filter out variables not listed in vars from var-groundings
    for (const auto& varg_map : var_groundings) {
        std::map<Handle, Handle> filtered;

        for (const auto& iv : varg_map) {
            if (find(vars.begin(), vars.end(), iv.first) != vars.end()) {
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
        } else {
            //TODO Execute if executable and push to FC results
        }
    }

    return derived_rules;
}

/**
 * Tries to unify the @param source with @parama target and derives
 * new rules using @param rule as a template.
 *
 * @param source  An atom that might bind to variables in @param rule.
 * @param target  An atom to be unified with @param source
 * @rule  rule    The rule object whose implicants are to be unified.
 *
 * @return        true on successful unification and false otherwise.
 */
bool ForwardChainerCallBack::unify(Handle source,Handle target,Rule* rule){
    //exceptions
    if (not is_valid_implicant(target))
        return false;

    HandleSeq derived_rules={};

    AtomSpace temp_pm_as;
    Handle hcpy = temp_pm_as.add_atom(target);
    Handle implicant_vardecl = temp_pm_as.add_atom(
            gen_sub_varlist(target, rule->get_vardecl()));
    Handle sourcecpy = temp_pm_as.add_atom(source);

    BindLinkPtr bl =
    createBindLink(HandleSeq { implicant_vardecl, hcpy, hcpy });

    DefaultImplicator impl(&temp_pm_as);
    impl.implicand = bl->get_implicand();

    bl->imply(impl);

    if(not impl.get_result_list().empty())
        return true;
    else
        return false;

}

/**
 *  Checks if sub atoms of implicant lists in @param rule are unifiable with
 *  @param source.
 *
 *  @param source  An atom that might bind to variables in @param rule.
 *  @param rule    The rule object whose implicants are to be sub atom unified.
 *
 *  @return        true if source is subatom unifiable and false otherwise.
 */
bool ForwardChainerCallBack::subatom_unify(Handle source,Rule* rule)
{
    UnorderedHandleSet output_expanded = get_subatoms(rule);

    for (Handle h : output_expanded) {
        if( unify(source,h, rule))
            return true;
    }

    return false;
}

Handle ForwardChainerCallBack::gen_sub_varlist(const Handle& parent,
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
