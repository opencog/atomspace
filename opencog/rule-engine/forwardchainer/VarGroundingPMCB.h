/*
 * VarGroundingPMCB.h
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

#ifndef VARGROUNDINGPMCB_H_
#define VARGROUNDINGPMCB_H_

#include <opencog/query/DefaultImplicator.h>

namespace opencog
{
/**
 * It just holds all var_grounding and term_grounding maps
 * of the entire PM process.
 */
class VarGroundingPMCB: public virtual DefaultImplicator {
public:
    VarGroundingPMCB(AtomSpace * as) :
            Implicator(as), InitiateSearchCB(as),
            DefaultPatternMatchCB(as),DefaultImplicator(as)

    {
    }
    virtual ~VarGroundingPMCB(){}

    virtual bool grounding(const std::map<Handle, Handle> &var_soln,
                           const std::map<Handle, Handle> &term_soln)
    {

        var_groundings.push_back(var_soln);
        term_groundings.push_back(term_soln);
        // If we found as many as we want, then stop looking for more.
        if (result_list.size() < max_results)
            return false;

        return true;
    }

    std::vector<std::map<Handle, Handle>> var_groundings;
    std::vector<std::map<Handle, Handle>> term_groundings;
};

} /* namespace opencog */

#endif /* VARGROUNDINGPMCB_H_ */
