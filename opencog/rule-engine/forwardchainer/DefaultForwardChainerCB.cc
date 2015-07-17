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
    Handle source = fcmem.get_cur_source();
    if (source == Handle::UNDEFINED)
        throw InvalidParamException(TRACE_INFO, "Needs a valid source atom");

    vector<Rule*> chosen_rules;
    auto rules = fcmem.get_rules();
    for (Rule* rule : rules) {
        Handle rimplicant = rule->get_implicant();
        HandleSeq impl_members = get_implicant_seq(rimplicant);
        bool match = false;

        for (Handle h : impl_members) {
            AtomSpace rule_atomspace;
            Handle hcpy = rule_atomspace.add_atom(h);
            Handle implicant_vardecl = rule_atomspace.add_atom(
                    gen_sub_varlist(h, rule->get_vardecl()));
            Handle sourcecpy = rule_atomspace.add_atom(source);

            HandleSeq hx { implicant_vardecl, hcpy, hcpy };
            BindLinkPtr bl = createBindLink(hx);
            DefaultImplicator imp(&rule_atomspace);
            imp.implicand = bl->get_implicand();
            bl->imply(imp);

            for (const auto& hi : imp.result_list) {
                if (hi == sourcecpy) {
                    match = true;
                    break;
                }
            }

            if (match)
                break;
        }

        if (match) {
            auto it = find(chosen_rules.begin(), chosen_rules.end(), rule);
            if (it == chosen_rules.end()){
                chosen_rules.push_back(rule);}
        }
    }

    return chosen_rules;
}

/**
 * Gets all top level links of certain types and subclasses that
 * contain @param hsource
 *
 * @param hsource handle whose top level links are to be searched
 * @param as the atomspace in which search is to be done
 * @param link_type the root link types to be searched
 * @param subclasses a flag that tells to look subclasses of @link_type
 */
HandleSeq DefaultForwardChainerCB::get_rootlinks(Handle hsource, AtomSpace* as,
                                                 Type link_type,
                                                 bool subclasses)
{
    URECommons urec(*as);
    HandleSeq chosen_roots;
    HandleSeq candidates_roots;
    urec.get_root_links(hsource, candidates_roots);

    for (Handle hr : candidates_roots) {
        bool notexist = find(chosen_roots.begin(), chosen_roots.end(), hr)
                == chosen_roots.end();
        auto type = as->get_type(hr);
        bool subtype = (subclasses and classserver().isA(type, link_type));
        if (((type == link_type) or subtype) and notexist) {
            // Make sure matches are actually part of the premise list
            // rather than the output of the bindLink
            Handle hpremise = BindLinkCast(hr)->get_body();
            if (urec.exists_in(hpremise, hsource)) {
                chosen_roots.push_back(hr);
            }
        }

    }

    return chosen_roots;
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

HandleSeq DefaultForwardChainerCB::get_implicant_seq(Handle implicant)
{
    Type t = implicant->getType();
    HandleSeq hs;

    if (t == AND_LINK or t == OR_LINK)
        hs = _as.get_outgoing(implicant);
    else
        hs.push_back(implicant);

    return hs;
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
