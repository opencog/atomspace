/*
 * BackwardChainerPMCB.h
 *
 * Copyright (C) 2014 Misgana Bayetta
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  October 2014
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

#ifndef _OPENCOG_BACKWARDCHAINERPMCB_H_
#define _OPENCOG_BACKWARDCHAINERPMCB_H_

#include <opencog/query/DefaultImplicator.h>

namespace opencog
{

class BackwardChainerPMCB :
	public InitiateSearchCB,
	public DefaultPatternMatchCB // : public virtual PLNImplicator
{
protected:
	AtomSpace* _as;
	VariableListPtr _int_vars;
	bool _enable_var_name_check;

	HandleMapSeq var_solns_;
	HandleMapSeq pred_solns_;

public:
	BackwardChainerPMCB(AtomSpace*, VariableListPtr, bool);
	virtual ~BackwardChainerPMCB();

	virtual void set_pattern(const Variables& vars,
	                         const Pattern& pat)
	{
		InitiateSearchCB::set_pattern(vars, pat);
		DefaultPatternMatchCB::set_pattern(vars, pat);
	}

	virtual bool grounding(const HandleMap &var_soln,
	                       const HandleMap &pred_soln);

    /**
     * Return list of matching results found by the pattern matcher
     * @return
     */
	const HandleMapSeq& get_var_list() const;
	const HandleMapSeq& get_pred_list() const;
};

} // ~namespace opencog

#endif /* _OPENCOG_BACKWARDCHAINERPMCB_H_ */
