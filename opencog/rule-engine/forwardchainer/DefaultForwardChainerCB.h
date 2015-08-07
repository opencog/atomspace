/*
 * DefaultForwardChainerCB.h
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

#ifndef DEFAULTFORWARDCHAINERCB_H_
#define DEFAULTFORWARDCHAINERCB_H_

#include <opencog/rule-engine/forwardchainer/ForwardChainerPMCB.h>
#include "ForwardChainerCallBack.h"

namespace opencog
{

class FCMemory;
class Rule;
class DefaultForwardChainerCB: public virtual ForwardChainerCallBack {
private:
    AtomSpace& _as;
    ForwardChainerPMCB _fcpm;
    bool subclasses = false;
    source_selection_mode _ts_mode;

    Handle gen_sub_varlist(const Handle& parent, const Handle& parent_varlist);
    HandleSeq substitute_rule_part(AtomSpace& as, Handle hrule,const std::set<Handle>& vars,const std::vector<std::map<Handle,Handle>>& var_groundings);


public:
    DefaultForwardChainerCB(AtomSpace& as, source_selection_mode ts_mode =
            TV_FITNESS_BASED);

    //callbacks
    virtual vector<Rule*> choose_rules(FCMemory& fcmem);
    virtual HandleSeq choose_premises(FCMemory& fcmem);
    virtual Handle choose_next_source(FCMemory& fcmem);
    virtual HandleSeq apply_rule(FCMemory& fcmem);

    HandleSeq new_rules; // new_rules formed substituting source to matching implicant
};

} // ~namespace opencog

#endif /* DEFAULTFORWARDCHAINERCB_H_ */
