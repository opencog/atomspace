/*
 * opencog/atoms/parallel/PureExecLink.cc
 *
 * Copyright (C) 2009, 2013-2015, 2020, 2024 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#include <opencog/atoms/parallel/PureExecLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Transient.h>

using namespace opencog;

/// PureExecLink
/// Perform execution in given AtomSpace, or a transient, if none given.
///
/// The general structure of this link is
///
///        PureExecLink
///            ExecutableAtom
///            AtomSpace (optional)
///
/// When this link is executed, the `ExecutableAtom` is executed in the
/// specified AtomSpace, so that any Atoms created during execution end
/// up there, instead of the current AtomSpace.

PureExecLink::PureExecLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	if (0 == _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting at least one argument!");
}

ValuePtr PureExecLink::execute(AtomSpace* as,
                               bool silent)
{
	if (not _outgoing[0]->is_executable()) return _outgoing[0];

	// Is there an AtomSpace? If so, use that.
	if (1 < _outgoing.size() and _outgoing[1]->get_type() == ATOM_SPACE)
	{
		AtomSpace* tas = AtomSpaceCast(_outgoing[1]).get();
		return _outgoing[0]->execute(tas, silent);
	}

	// No AtomSpace provided. Use a temporary.
	// XXX Note that this leaks, if the execute throws.
	// The transient code will catch the leak, and complain.
	// (There's no actual memleak; just a complaint about counting.)
	AtomSpace* tas = grab_transient_atomspace(as);
	ValuePtr evp(_outgoing[0]->execute(tas, silent));
	release_transient_atomspace(tas);
	return evp;
}

DEFINE_LINK_FACTORY(PureExecLink, PURE_EXEC_LINK)
