/*
 * InferenceSCM.cc
 *
 * Copyright (C) 2014 Misgana Bayetta
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

#include "InferenceSCM.h"

#include <opencog/rule-engine/forwardchainer/ForwardChainer.h>
#include <opencog/rule-engine/backwardchainer/BackwardChainer.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*> InferenceSCM::_binders;

InferenceSCM::InferenceSCM() : ModuleWrap("opencog rule-engine") {}

/// This is called while (opencog rule-engine) is the current module.
/// Thus, all the definitions below happen in that module.
void InferenceSCM::init(void)
{
	_binders.push_back(new FunctionWrap(do_forward_chaining,
	                                    "cog-fc", "rule-engine"));
	_binders.push_back(new FunctionWrap(do_backward_chaining,
	                                    "cog-bc", "rule-engine"));
}

InferenceSCM::~InferenceSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
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
Handle do_forward_chaining(AtomSpace* as,
                           const Handle& hsource,
                           const Handle& rbs,
                           const Handle& hfocus_set)
{
#ifdef HAVE_GUILE
    ForwardChainer fc(*as, rbs);

    HandleSeq focus_set = {};

    if (hfocus_set->getType() == SET_LINK)
        focus_set = LinkCast(hfocus_set)->getOutgoingSet();
    else
        throw RuntimeException(
                TRACE_INFO,
                "InferenceSCM::do_forward_chaining - focus set should be SET_LINK type!");

    // TODO variable fulfillment
    fc.do_chain(hsource,focus_set);
    HandleSeq result = fc.get_chaining_result();

    return as->add_link(LIST_LINK, result);

#else
    return Handle::UNDEFINED;
#endif
}

Handle do_backward_chaining(AtomSpace* as,
                            const Handle& h,
                            const Handle& rbs,
                            const Handle& focus_link)
{
    if (Handle::UNDEFINED == rbs)
        throw RuntimeException(TRACE_INFO,
            "InferenceSCM::do_backward_chaining - invalid rulebase!");

#ifdef HAVE_GUILE
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
#else
    return Handle::UNDEFINED;
#endif
}

}

void opencog_ruleengine_init(void)
{
    static InferenceSCM inference;
    inference.module_init();
}
