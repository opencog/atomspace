/*
 * AssignLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomutils/FindUtils.h>

#include "AssignLink.h"

using namespace opencog;

AssignLink::AssignLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(ASSIGN_LINK, oset, tv, av)
{
	init(oset);
}

AssignLink::AssignLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, oset, tv, av)
{
	if (not classserver().isA(t, ASSIGN_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an AssignLink, got %s", tname.c_str());
	}

	init(oset);
}

AssignLink::AssignLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, ASSIGN_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an AssignLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

// ---------------------------------------------------------------

void AssignLink::init(const HandleSeq& oset)
{
	// The first member of the handleset must be a TypeNode, and it must
	// name a valid atom type.

	if (0 == oset.size())
		throw InvalidParamException(TRACE_INFO,
			"AssignLinks must have members!");

	Type t = oset[0]->getType();
	if (TYPE_NODE != t)
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a AssignLink! First member must be a type node!");

	const std::string& name = NodeCast(oset[0])->getName();
	if (not classserver().isDefined(name))
		throw InvalidParamException(TRACE_INFO,
			"Invalid format for a AssignLink! Not a defined type!");

	// Cache the type and the oset
	_link_type = classserver().getType(name);

	for (size_t j=1; j < oset.size(); j++)
		_outset.push_back(oset[j]);

	_osetz = _outset.size();
}

// ---------------------------------------------------------------

Handle AssignLink::execute(AtomSpace * as) const
{
	// XXX This is probably wrong ... if the as is null, we should
	// probably use the atomspace that this link is in, right?
	// We need to make a decision here and in many other places...
	if (NULL == as)
		return Handle(createLink(_link_type, _outset));

	// First, remove everything resembling this pattern.
	IncomingSet iset = _outset[0]->getIncomingSet();
	for (const LinkPtr& lp : iset)
	{
		// Wrong type, can't delete that!
		if (_link_type != lp->getType()) continue;
		// Wrong Arity, can't delete that either!
		if (_osetz != lp->getArity()) continue;

		const HandleSeq& hs = lp->getOutgoingSet();
		if (hs[0] != _outset[0]) continue;

		bool match = true;
		for (size_t i=1; i < _osetz; i++)
		{
			// Contains a variable, or doesn't match -- don't delete.
			if (VARIABLE_NODE == hs[i]->getType())
			{
				match = false;
				break;
			}
		}
		if (not match) continue;

		as->removeAtom(Handle(lp));
	}

	return as->addAtom(createLink(_link_type, _outset));
}

// ============================================================

Handle InsertLink::execute(AtomSpace* as) const
{
	if (NULL == as)
		return Handle(createLink(_link_type, _outset));
	return as->addAtom(createLink(_link_type, _outset));
}

InsertLink::InsertLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: AssignLink(INSERT_LINK, oset, tv, av)
{}

InsertLink::InsertLink(Link &l)
	: AssignLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, INSERT_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a InsertLink, got %s", tname.c_str());
	}
}

// ============================================================


Handle RemoveLink::execute(AtomSpace* as) const
{
	// Are there *any* constants in the outgoing set?
	// narrowst will be -1 if they're all free variables.
	// narrowest helps make the next loop small...
	int narrowest = -1;
	size_t narsz = SIZE_MAX;
	for (size_t i=0; i < _osetz; i++)
	{
		if (VARIABLE_NODE == _outset[i]->getType()) continue;
		size_t isz = _outset[i]->getIncomingSetSize();
		if (isz < narsz)
		{
			narsz = isz;
			narrowest = i;
		}
	}

	// Delete matching constant (closed) links
	if (0 <= narrowest)
	{
		IncomingSet iset = _outset[narrowest]->getIncomingSet();
		for (const LinkPtr& lp : iset)
		{
			// Wrong type, can't delete that!
			if (_link_type != lp->getType()) continue;
			// Wrong Arity, can't delete that either!
			if (_osetz != lp->getArity()) continue;

			const HandleSeq& hs = lp->getOutgoingSet();
			bool match = true;
			for (size_t i=0; i < _osetz; i++)
			{
				if (VARIABLE_NODE == _outset[i]->getType()) continue;

				// Contains a variable, or doesn't match -- don't delete.
				if (VARIABLE_NODE == hs[i]->getType() or
				    _outset[i] != hs[i])
				{
					match = false;
					break;
				}
			}
			if (not match) continue;

			as->removeAtom(Handle(lp));
		}
		return Handle::UNDEFINED;
	}

	// If we are here, then the entire outset consisted of fre variables.
	// In this case, deleted everything that has the same arity, and does
	// not contain variables.
	HandleSeq seq;
	as->getHandlesByType(seq, _link_type);
	for (const Handle& h : seq)
	{
		LinkPtr lp = LinkCast(h);

		// Wrong Arity, cannot delete that.
		if (_osetz != lp->getArity()) continue;

		const HandleSeq& hs = lp->getOutgoingSet();
		bool match = true;
		for (size_t i=0; i < _osetz; i++)
		{
			// Contains a variable -- don't delete.
			if (VARIABLE_NODE == hs[i]->getType())
			{
				match = false;
				break;
			}
		}

		if (not match) continue;
		as->removeAtom(h);
	}
	return Handle::UNDEFINED;
}

RemoveLink::RemoveLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: AssignLink(REMOVE_LINK, oset, tv, av)
{}

RemoveLink::RemoveLink(Link &l)
	: AssignLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, REMOVE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a RemoveLink, got %s", tname.c_str());
	}
}

/* ===================== END OF FILE ===================== */
