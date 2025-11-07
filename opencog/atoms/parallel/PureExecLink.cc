/*
 * opencog/atoms/parallel/PureExecLink.cc
 *
 * Copyright (C) 2009, 2013-2015, 2020, 2024, 2025 Linas Vepstas
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
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Transient.h>

using namespace opencog;

/// PureExecLink
/// Perform execution in given AtomSpace, or a transient, if none given.
///
/// The general structure of this link is
///
///        PureExecLink
///            AtomSpace (optional)
///            ExecutableAtom
///            AnotherExecutableAtom
///            AnotherAtomSpace (optional)
///            MoreExecutableAtom
///
/// When this link is executed, all of the various `ExecutableAtoms`
/// are executed in the sequential order, in the most recent AtomSpace
/// that preceeded them. Thus, if execution has side effects, such
/// as creating new Atoms, they end up there, and not the current
/// AtomSpace. That's what make's it "Pure" -- no side-effects in the
/// current AtomSpace.
///
/// If no AtomSpace is given, a temporary transient is used.
///
/// Execution results are placed in a LinkValue; the LinkValue is
/// returned.

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
	ValueSeq vseq;
	AtomSpace* ctxt = nullptr;
	for (const Handle& h : _outgoing)
	{
		if (h->is_type(ATOM_SPACE))
		{
			ctxt = AtomSpaceCast(h).get();
			continue;
		}
		if (not h->is_executable())
		{
			vseq.push_back(h);
			continue;
		}
		if (ctxt)
		{
			vseq.push_back(h->execute(ctxt, silent));
			continue;
		}

		// No AtomSpace provided. Use a temporary.
		Transient scratch(as);
		vseq.push_back(h->execute(scratch.tmp, silent));
	}

	return createLinkValue(std::move(vseq));
}

DEFINE_LINK_FACTORY(PureExecLink, PURE_EXEC_LINK)
