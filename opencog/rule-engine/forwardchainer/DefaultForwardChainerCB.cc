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
#include <opencog/guile/SchemeSmob.h>
#include <opencog/atoms/bind/BindLink.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/atomutils/FindUtils.h>

#include "DefaultForwardChainerCB.h"
#include "../URECommons.h"

using namespace opencog;

DefaultForwardChainerCB::DefaultForwardChainerCB(AtomSpace& as,
                                                 source_selection_mode ts_mode
                                                 /*=TV_FITNESS_BASED*/) :
        ForwardChainerCallBack(&as), _as(as), _fcpm(&as), _ts_mode(ts_mode)
{
}

/**
 * Choose rules based on premises of rule matching the source.
 * Uses temporary atomspace to limit the search space. It adds
 * each members of the implicant(members being identified by
 * checking if they are wrapped in Logical links or not) and
 * the source to a temporary atomspace and creates a bindLink
 * for each implicants to match against the source;If they match,
 * the rule in which the implicant was a member is considered as
 *  a valid match and is  added to the return list.
 *
 * @param fcmem forward chainer's working memory
 * @return a vector of chosen rules
 */
vector<Rule*> DefaultForwardChainerCB::choose_rules(FCMemory& fcmem)
{
    new_rules.clear(); //empty vector before starting matching
    Handle source = fcmem.get_cur_source();
    if (source == Handle::UNDEFINED)
        throw InvalidParamException(TRACE_INFO, "Needs a valid source atom");

    vector<Rule*> chosen_rules;
    auto rules = fcmem.get_rules();
    vector<Rule*> visited;
    for (Rule* rule : rules) {
        HandleSeq impl_members = rule->get_implicant_seq();
        bool match = false;
        for (Handle h : impl_members) {
            //exceptions
            if (h->getType() == ABSENT_LINK)
                continue;

            AtomSpace temp_pm_as;
            Handle hcpy = temp_pm_as.add_atom(h);
            Handle implicant_vardecl = temp_pm_as.add_atom(
                    gen_sub_varlist(h, rule->get_vardecl()));
            Handle sourcecpy = temp_pm_as.add_atom(source);

            HandleSeq hx { implicant_vardecl, hcpy, hcpy };
            BindLinkPtr bl = createBindLink(hx);
            DefaultImplicator imp(&temp_pm_as);
            imp.implicand = bl->get_implicand();
            PMCGroundings gcb(imp);
            bl->imply(gcb);

            FindAtoms fv(VARIABLE_NODE);

            for (const std::map<Handle, Handle> termg_map : gcb._term_groundings) {
                for (const auto& it : termg_map) {
                    if (it.second == sourcecpy) {
                        match = true;
                        fv.search_set(it.first);

                        //TODO where should we check? how to refactor this part?
                        HandleSeq new_rules = substitute_rule_part(
                                temp_pm_as,
                                temp_pm_as.add_atom(rule->get_handle()),
                                fv.varset, gcb._var_groundings);
                        //Push newly created valid rules
                        for (const auto& nr : new_rules)
                            if (find(new_rules.begin(), new_rules.end(),
                                     nr)
                                == new_rules.end())
                                new_rules.push_back(nr);
                    }
                }
            }
        }

        if (match) {
            auto it = find(chosen_rules.begin(), chosen_rules.end(), rule);
            if (it == chosen_rules.end()) {
                std::cout << "RULE-CHOSEN:\n" << rule->get_handle()->toShortString() << std::endl;
                chosen_rules.push_back(rule);
            }
        }
    }

    return chosen_rules;
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
 * Derives a new rule by replacing the grounded vars of @param hrule
 *
 * @param as             The Atomspace where all the atoms are dwelling
 * @param hrule          A handle to BindLink instance
 * @param vars           The grounded var list in @param hrule
 * @param var_groundings the set of groundings to each var in @param vars
 *
 * @return A HandleSeq of all possible derived rules
 */
HandleSeq DefaultForwardChainerCB::substitute_rule_part(
        AtomSpace& as, Handle hrule,const std::set<Handle>& vars,
        const std::vector<std::map<Handle, Handle>>& var_groundings)
{
    Substitutor st(&as);
    BindLinkPtr blptr = BindLinkCast(hrule);
    std::vector<std::map<Handle, Handle>> filtered_vgmap_list;

    //Get variable grounding map
    for (const std::map<Handle, Handle>& varg_map : var_groundings) {
        std::map<Handle, Handle> filtered_vgmap;
        for (const auto& iv : varg_map) {
            //add to our mapping list if this key/varNode is in our vars
            //and if it is a node to node mapping.
            if (find(vars.begin(), vars.end(), iv.first) != vars.end()
                and (NodeCast(iv.first) and NodeCast(iv.second))) {
                filtered_vgmap[iv.first] = iv.second;
            }
        }
        filtered_vgmap_list.push_back(filtered_vgmap);
    }

    //With all the above informations, create new bindLinks for each var-ground mapping
    HandleSeq derived_rules;
    for (auto& vgmap : filtered_vgmap_list) {
        //Construct new VariableList by removing grounded ones from the original
        Variables var_struct = blptr->get_variables();
        HandleSeq var_seq = var_struct.varseq;
        VariableTypeMap vtype_map = var_struct.typemap;

        for (const Handle& var : vars) {
            auto it = find(var_seq.begin(), var_seq.end(), var);
            if (it != var_seq.end()) {
                var_seq.erase(it);
                vtype_map.erase(*it);
            }
        }

        //Create the BindLink/Rule
        Handle hvarlist = create_varlist(as,var_seq, vtype_map);
        if(hvarlist == Handle::UNDEFINED) continue;

        //TODO if implicand is executable, execute it?
        Handle himplicant = st.substitute(blptr->get_body(), vgmap);
        Handle himplicand = st.substitute(blptr->get_implicand(), vgmap);
        Handle hderived_rule = Handle(LinkCast(createBindLink(HandleSeq{hvarlist,himplicant,himplicand})));
        derived_rules.push_back(hderived_rule);
    }

    return derived_rules;
}

Handle DefaultForwardChainerCB::create_varlist(AtomSpace& as,HandleSeq& varseq,
                                               VariableTypeMap& vtype_map)
{
    HandleSeq oset;
    for (auto& var : varseq) {

        if (vtype_map.count(var)) {
            HandleSeq tvarlist = { var };
            std::set<Type> types = vtype_map[var];
            for (auto& type : types)
                tvarlist.push_back(TypeNode(type).getHandle());

            oset.push_back(as.add_link(TYPED_VARIABLE_LINK, tvarlist));
        } else {
            oset.push_back(var);
        }
    }

    if (not oset.empty())
        return as.add_link(VARIABLE_LIST, oset);

    return Handle::UNDEFINED;
}
