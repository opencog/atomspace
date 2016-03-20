/*
 * opencog/atoms/execution/Eager.cc
 *
 * Copyright (C) 2009, 2013, 2015, 2016 Linas Vepstas
 * All Rights Reserved
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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Eager.h"
#include "Instantiator.h"

using namespace opencog;

/// Utility -- Perform eager execution of black-box arguments.
///
/// Perform eager execution of the arguments. We have to do this,
/// because the user-defined functions are black-boxes, and cannot be
/// trusted to do lazy execution correctly. Right now, this is the
/// policy. I guess we could add "scm-lazy:" and "py-lazy:" URI's
/// for user-defined functions smart enough to do lazy evaluation.
///
/// When executing, if the results are different, add the new
/// results to the atomspace. We need to do this, because scheme,
/// and python expects to find thier arguments in the atomspace.
/// This is arguably broken, as it pollutes the atomspace with
/// junk that is never cleaned up.  We punt for now, but something
/// should be done about this. XXX FIXME ... Well ... except that
/// all callers of this method are invited to create a temporary
/// scratch atomspace, and use that. This presumably avoids the
/// pollution concerns.
///
Handle opencog::eager_execute(AtomSpace* as, const Handle& cargs)
{
	Instantiator inst(as);
	Handle args(cargs);
	if (LIST_LINK == cargs->getType())
	{
		std::vector<Handle> new_oset;
		bool changed = false;
		for (const Handle& ho : cargs->getOutgoingSet())
		{
			Handle nh(inst.execute(ho));
			// nh might be NULL if ho was a DeleteLink
			if (nullptr == nh)
			{
				changed = true;
				continue;
			}

			// Unwrap the top-most DontExecLink's.  Lower ones are left
			// untouched.  We do this as a sop for issue opencog/atomspace#704
			// but maybe we should not?
			if (DONT_EXEC_LINK == nh->getType())
			{
				nh = nh->getOutgoingAtom(0);
			}
			new_oset.emplace_back(nh);

			if (nh != ho) changed = true;
		}
		if (changed)
			args = as->add_link(LIST_LINK, new_oset);
	}

	return args;
}
