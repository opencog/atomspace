/*
 * InferenceSCM.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Sept 2014
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include <opencog/rule-engine/forwardchainer/ForwardChainer.h>
#include <opencog/rule-engine/backwardchainer/BackwardChainer.h>

#include "UREConfigReader.h"
#include "InferenceSCM.h"
using namespace opencog;

InferenceSCM::InferenceSCM() : ModuleWrap("opencog rule-engine") {}

/// This is called while (opencog rule-engine) is the current module.
/// Thus, all the definitions below happen in that module.
void InferenceSCM::init(void)
{
	define_scheme_primitive("cog-fc",
		&InferenceSCM::do_forward_chaining, this, "rule-engine");

	define_scheme_primitive("cog-bc",
		&InferenceSCM::do_backward_chaining, this, "rule-engine");

	define_scheme_primitive("ure-rbs-rules",
		&InferenceSCM::get_rulebase_rules, this, "rule-engine");
}

namespace opencog {

/**
 * A scheme cog-fc call back handler method which invokes the forward
 * chainer with the arguments passed to cog-fc.
 *
 * @param hsource      The source atom to start the forward chaining with.
 * @param rbs          A handle to the rule base ConceptNode.
 * @param hfoucs_set   A handle to a set link containing the set of focus sets.
 *                     if the set link is empty, FC will be invoked on the entire
 *                     atomspace.
 *
 * @return             A ListLink containing the result of FC inference.
 */
Handle InferenceSCM::do_forward_chaining(
                           Handle hsource,
                           Handle rbs,
                           Handle hfocus_set)
{
    AtomSpace *as = SchemeSmob::ss_get_env_as("cog-fc");
    HandleSeq focus_set = {};

    if (hfocus_set->getType() == SET_LINK)
        focus_set = LinkCast(hfocus_set)->getOutgoingSet();
    else
        throw RuntimeException(
                TRACE_INFO,
                "InferenceSCM::do_forward_chaining - focus set should be SET_LINK type!");

    ForwardChainer fc(*as, rbs, hsource, focus_set);
    fc.do_chain();
    HandleSeq result = fc.get_chaining_result();

    return as->add_link(LIST_LINK, result);
}

Handle InferenceSCM::do_backward_chaining(
                            Handle h,
                            Handle rbs,
                            Handle focus_link)
{
    if (Handle::UNDEFINED == rbs)
        throw RuntimeException(TRACE_INFO,
            "InferenceSCM::do_backward_chaining - invalid rulebase!");

    AtomSpace *as = SchemeSmob::ss_get_env_as("cog-bc");
    BackwardChainer bc(*as, rbs);
    bc.set_target(h, focus_link);

    logger().debug("[BackwardChainer] Before do_chain");

    bc.do_chain();

    logger().debug("[BackwardChainer] After do_chain");
    map<Handle, UnorderedHandleSet> soln = bc.get_chaining_result();

    HandleSeq soln_list_link;
    for (auto it = soln.begin(); it != soln.end(); ++it) {
        HandleSeq hs;
        hs.push_back(it->first);
        hs.insert(hs.end(), it->second.begin(), it->second.end());

        soln_list_link.push_back(as->add_link(LIST_LINK, hs));
    }

    return as->add_link(LIST_LINK, soln_list_link);
}

HandleSeq InferenceSCM::get_rulebase_rules(Handle rbs)
{
    if (Handle::UNDEFINED == rbs)
        throw RuntimeException(TRACE_INFO,
            "InferenceSCM::get_rulebase_rules - invalid rulebase!");

    AtomSpace *as = SchemeSmob::ss_get_env_as("ure-rbs-rules");
    UREConfigReader ure_config(*as, rbs);
    auto rules = ure_config.get_rules();
    HandleSeq hs;

    // Copy handles from a rule vector to a handle vector as there are no
    // scheme-primitive signature for rule vector.
    // TODO: create a rule-vector scheme-primitive signature, if Rule.h isn't
    // converted to a sub-class of PatternLink.
    for (auto i = rules.begin(); i != rules.end(); i++){
        hs.push_back((*i).get_alias());
    }

    return hs;
}

}

void opencog_ruleengine_init(void)
{
    static InferenceSCM inference;
    inference.module_init();
}
