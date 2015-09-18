/*
 * FocusSetPMCB.h
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#ifndef _FOCUSSETPMCB_H_
#define _FOCUSSETPMCB_H_

#include <opencog/query/DefaultImplicator.h>

namespace opencog
{

/**
 * Used for pattern matching and instantiating on two
 * different atomspaces.
 *
 */
class FocusSetPMCB: public virtual DefaultImplicator {
private:
    Instantiator* _inst;
    AtomSpace * _inst_as;

public:
    FocusSetPMCB(AtomSpace* pm_as, AtomSpace* inst_as) :
            Implicator(pm_as), InitiateSearchCB(pm_as),
                    DefaultPatternMatchCB(pm_as), DefaultImplicator(pm_as),_inst_as(inst_as)
    {
        _inst = new Instantiator(inst_as);
    }

    virtual ~FocusSetPMCB()
    {
        delete _inst;
    }

    virtual bool grounding(const std::map<Handle, Handle> &var_soln,
                           const std::map<Handle, Handle> &term_soln)
    {
        std::cout << "RESULT" << std::endl;

        Handle h = _inst->instantiate(implicand, var_soln);

        _result_list.push_back(h);

        return false;
    }

};

} /* namespace opencog */

#endif /* _FOCUSSETPMCB_H_ */
