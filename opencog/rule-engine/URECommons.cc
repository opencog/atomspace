/*
 * URECommons.cc
 *
 * Copyright (C) 2014 Misgana Bayetta
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Oct 2014
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
#include "URECommons.h"

#include <opencog/util/macros.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

URECommons::URECommons(AtomSpace * as) :
        as_(as)
{
}

URECommons::~URECommons()
{
}

Handle URECommons::create_bindLink(Handle himplicant, bool vnode_is_typedv)
    throw (opencog::InvalidParamException)
{
    if (!LinkCast(himplicant)) {
        throw InvalidParamException(TRACE_INFO, "Input must be a link type ");
    } //xxx why?

    //if(vnode_is_typedv)
    himplicant = replace_nodes_with_varnode(himplicant);

    UnorderedHandleSet variable_nodes = get_outgoing_nodes(himplicant,
                                                           {VARIABLE_NODE});
    HandleSeq list_link_elem;

    // For searching ImplicationLinks with variables.
    if (vnode_is_typedv) {
        Handle h = as_->addNode(TYPE_NODE, "VariableNode");
        for (Handle hvn : variable_nodes) {
            Handle hi = as_->addLink(TYPED_VARIABLE_LINK, hvn, h);
            list_link_elem.push_back(hi);
        }
    } else
        list_link_elem.insert(list_link_elem.end(), variable_nodes.begin(),
                              variable_nodes.end());

    Handle var_listLink = as_->addLink(VARIABLE_LIST, list_link_elem);

    Handle implicationLink = as_->addLink(IMPLICATION_LINK, himplicant,
                                          himplicant);

    return as_->addLink(BIND_LINK, var_listLink, implicationLink);
}

bool URECommons::exists_in(Handle& hlink, Handle& h)
{
    if (hlink == h) {
        return true;
    } else {
        if (not LinkCast(hlink))
            throw InvalidParamException(TRACE_INFO,
                                        "Need a LINK type to look in");
        auto outg = as_->getOutgoing(hlink);
        if (find(outg.begin(), outg.end(), h) != outg.end())
            return true;
        else {
            for (Handle hi : outg) {
                if (LinkCast(hi) and exists_in(hi, h))
                    return true;
            }
        }
        return false;
    }
}

Handle URECommons::change_node_types(Handle& h,
                                     map<Handle, Handle>& replacement_map)
{
    Handle hcpy;
    if (LinkCast(h)) {
        HandleSeq hs_cpy;
        HandleSeq hs = as_->getOutgoing(h);
        for (Handle hi : hs) {
            if (NodeCast(hi)) {
                if (replacement_map.find(hi) != replacement_map.end())
                    hs_cpy.push_back(replacement_map[hi]);
                else
                    hs_cpy.push_back(hi);
            } else if (LinkCast(hi)) {
                hs_cpy.push_back(change_node_types(hi, replacement_map));
            }
        }
        hcpy = as_->addLink(as_->getType(h), hs_cpy);
        hcpy->setTruthValue(as_->getTV(h));
    } else if (NodeCast(h)) {
        if (replacement_map.find(h) != replacement_map.end())
            hcpy = replacement_map[h];
        else
            hcpy = h;
    }

    return hcpy;
}

void URECommons::get_root_links(Handle h, HandleSeq& parents)
{
    auto incoming = as_->getIncoming(h);
    if (incoming.empty())
        return;
    else {
        for (Handle hi : incoming) {
            auto i = as_->getIncoming(hi);
            if (i.empty()) {
                if (find(parents.begin(), parents.end(), hi) == parents.end())
                    parents.push_back(hi);
            } else {
                get_root_links(hi, parents);
            }
        }
    }
}

float URECommons::tv_fitness(Handle h)
{
    TruthValuePtr ptv = as_->getTV(h);
    confidence_t c = ptv->getConfidence();
    strength_t s = ptv->getMean();
    return (pow((1 - s), FITNESS_PARAM) * (pow(c, (2 - FITNESS_PARAM))));
}
