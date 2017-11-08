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

#ifdef HAVE_GUILE

#include <opencog/guile/SchemeModule.h>

namespace opencog {

class InferenceSCM : public ModuleWrap
{
protected:
	virtual void init();

	/**
	 * The scheme (cog-mandatory-args-fc) function calls this, to
	 * perform forward-chaining.
	 *
	 * @param rbs          A node, holding the name of the rulebase.
	 * @param source       The source atom with which to start the chaining.
	 * @param vardecl      The variable declaration, if any, of the source.
	 * @param focus_set    A SetLink containing the atoms to which forward
	 *                     chaining will be applied.  If the set link is
	 *                     empty, chaining will be invoked on the entire
	 *                     atomspace.
	 *
	 * @return             A SetLink containing the results of FC inference.
	 */
	Handle do_forward_chaining(Handle rbs,
	                           Handle source,
	                           Handle vardecl,
	                           Handle focus_set);

	/**
	 * The scheme (cog-mandatory-args-bc) function calls this, to
	 * perform forward-chaining.
	 *
	 * @param rbs          A node, holding the name of the rulebase.
	 * @param target       The target atom with which to start the chaining from.
	 * @param vardecl      The variable declaration, if any, of the target.
	 * @param trace_as     AtomSpace where to record the back-inference traces
	 * @param control_as   AtomSpace where to find the inference control rules
	 * @param focus_set    A SetLink containing the atoms to which forward
	 *                     chaining will be applied.  If the set link is
	 *                     empty, chaining will be invoked on the entire
	 *                     atomspace.
	 *
	 * @return             A SetLink containing the results of FC inference.
	 */
	Handle do_backward_chaining(Handle rbs,
	                            Handle target,
	                            Handle vardecl,
	                            bool trace_enabled,
	                            AtomSpace* trace_as,
	                            bool control_enabled,
	                            AtomSpace* control_as,
	                            Handle focus_set);

	Handle get_rulebase_rules(Handle rbs);

public:
	InferenceSCM();
};

} /*end of namespace opencog*/

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include <opencog/rule-engine/forwardchainer/ForwardChainer.h>
#include <opencog/rule-engine/backwardchainer/BackwardChainer.h>

#include "UREConfig.h"
using namespace opencog;

InferenceSCM::InferenceSCM() : ModuleWrap("opencog rule-engine") {}

/// This is called while (opencog rule-engine) is the current module.
/// Thus, all the definitions below happen in that module.
void InferenceSCM::init(void)
{
	define_scheme_primitive("cog-mandatory-args-fc",
		&InferenceSCM::do_forward_chaining, this, "rule-engine");

	define_scheme_primitive("cog-mandatory-args-bc",
		&InferenceSCM::do_backward_chaining, this, "rule-engine");

	define_scheme_primitive("cog-rbs-rules",
		&InferenceSCM::get_rulebase_rules, this, "rule-engine");
}

Handle InferenceSCM::do_forward_chaining(Handle rbs,
                                         Handle source,
                                         Handle vardecl,
                                         Handle focus_set_h)
{
    AtomSpace *as = SchemeSmob::ss_get_env_as("cog-mandatory-args-fc");
    HandleSeq focus_set = {};

    // A ListLink means that the variable declaration is undefined
    if (vardecl->get_type() == LIST_LINK)
	    vardecl = Handle::UNDEFINED;

    if (focus_set_h->get_type() == SET_LINK)
	    focus_set = focus_set_h->getOutgoingSet();
    else
	    throw RuntimeException(
		    TRACE_INFO,
		    "InferenceSCM::do_forward_chaining - focus set should be SET_LINK type!");

    ForwardChainer fc(*as, rbs, source, vardecl, focus_set);
    fc.do_chain();
    UnorderedHandleSet result = fc.get_chaining_result();

    return as->add_link(SET_LINK, HandleSeq(result.begin(), result.end()));
}

Handle InferenceSCM::do_backward_chaining(Handle rbs,
                                          Handle target,
                                          Handle vardecl,
                                          bool trace_enabled,
                                          AtomSpace* trace_as,
                                          bool control_enabled,
                                          AtomSpace* control_as,
                                          Handle focus_link)
{
    // A ListLink means that the variable declaration is undefined
    if (vardecl->get_type() == LIST_LINK)
	    vardecl = Handle::UNDEFINED;

    if (not trace_enabled)
	    trace_as = nullptr;

    if (not control_enabled)
	    control_as = nullptr;

    AtomSpace *as = SchemeSmob::ss_get_env_as("cog-mandatory-args-bc");
    BackwardChainer bc(*as, rbs, target, vardecl, trace_as, control_as, focus_link);

    bc.do_chain();

    return bc.get_results();
}

// XXX FIXME -- this appears to be dead code, that no one uses.
// Can this be removed?
Handle InferenceSCM::get_rulebase_rules(Handle rbs)
{
    if (Handle::UNDEFINED == rbs)
        throw RuntimeException(TRACE_INFO,
            "InferenceSCM::get_rulebase_rules - invalid rulebase!");

    AtomSpace *as = SchemeSmob::ss_get_env_as("cog-rbs-rules");
    UREConfig ure_config(*as, rbs);
    auto rules = ure_config.get_rules();
    HandleSeq hs;

    // Copy handles from a rule vector to a handle vector
    for (auto i = rules.begin(); i != rules.end(); i++){
        hs.push_back((*i).get_alias());
    }

    return createLink(hs, SET_LINK);
}


extern "C" {
void opencog_ruleengine_init(void);
};

void opencog_ruleengine_init(void)
{
    static InferenceSCM inference;
    inference.module_init();
}

#endif // HAVE_GUILE
