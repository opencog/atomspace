/*
 * opencog/atoms/execution/Force.cc
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

#include "Force.h"
#include "Instantiator.h"

using namespace opencog;

/// Utility -- Force execution of black-box arguments.
///
/// Force execution of the arguments. We have to do this, because the
/// user-defined functions are black-boxes, and cannot be trusted to do
/// lazy execution correctly. Right now, forcing is the policy. Perhaps
/// we could add "scm-lazy:" and "py-lazy:" URI's for user-defined
/// functions smart enough to do lazy evaluation.
///
/// Note that although execution of the arguments is forced, the
/// execution itself happens in a lazy manner: thus, if the arguments
/// have nested terms that don't actually need to be executed, they
/// won't be. That is, the forcing is only one-level deep; its *not*
/// recursive.
///
/// When executing, if the results are different, the new results
/// are added to the atomspace. We need to do this, because scheme,
/// and python expects to find their arguments in the atomspace.
/// Users who do not want to pollute the atomspace should use a
/// temporary (scratch) atomspace.
///
Handle opencog::force_execute(AtomSpace* as, const Handle& cargs, bool silent)
{
	Instantiator inst(as);

	if (LIST_LINK != cargs->getType())
	{
		Handle args(inst.execute(cargs, silent));
		if (args != cargs)
			args = as->add_atom(args);
		return args;
	}

	Handle args(cargs);
	HandleSeq new_oset;
	bool changed = false;
	for (const Handle& ho : cargs->getOutgoingSet())
	{
		Handle nh(inst.execute(ho, silent));
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
	return args;
}
