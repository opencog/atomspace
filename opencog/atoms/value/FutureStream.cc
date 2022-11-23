/*
 * opencog/atoms/value/FutureStream.cc
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#include <stdlib.h>
#include <opencog/atoms/value/FutureStream.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// ==============================================================

FutureStream::FutureStream(const Handle& h) :
	LinkStreamValue(FUTURE_STREAM), _formula({h}), _as(h->getAtomSpace())
{
	init();
}

FutureStream::FutureStream(const HandleSeq&& oset) :
	LinkStreamValue(FUTURE_STREAM), _formula(std::move(oset))
{
	if (not (FUTURE_STREAM == _type)) return;

	if (0 == _formula.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting at least one atom!");

	_as = _formula[0]->getAtomSpace();

	init();
}

void FutureStream::init(void)
{
	if (not (FUTURE_STREAM == _type)) return;

	// Unwrap a ListLink, if it is present.
	if (1 == _formula.size() and LIST_LINK == _formula[0]->get_type())
		_formula = _formula[0]->getOutgoingSet();

	// Verify that we've got valid stuff.
	for (const Handle& h : _formula)
	{
		if (h->is_executable())
		{
			_value.emplace_back(h->execute(_as));
		}
		else if (h->is_evaluatable())
		{
			_value.emplace_back(ValueCast(h->evaluate(_as)));
		}
		else
		{
			throw SyntaxException(TRACE_INFO,
				"Expecting an executable/evaluatable atom, got %s",
				h->to_string().c_str());
		}
	}
}


// ==============================================================

void FutureStream::update() const
{
	std::vector<ValuePtr> newval;
	for (const Handle& h : _formula)
	{
		if (h->is_executable())
			newval.emplace_back(h->execute(_as));
		else if (h->is_evaluatable())
			newval.emplace_back(ValueCast(h->evaluate(_as)));
	}
	_value.swap(newval);
}

// ==============================================================

std::string FutureStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	for (const Handle& h : _formula)
		rv += "\n" + h->to_short_string(indent + "   ");
	rv += "\n" + indent + "   ; Current sample:\n";
	rv += LinkValue::to_string(indent + "   ; ");
	rv += "\n)";
	return rv;
}

// ==============================================================

bool FutureStream::operator==(const Value& other) const
{
	if (FUTURE_STREAM != other.get_type()) return false;

	const FutureStream* eso = (const FutureStream*) &other;
	return eso->_formula == _formula;
}

// ==============================================================

// Adds factor when library is loaded.
DEFINE_VALUE_FACTORY(FUTURE_STREAM, createFutureStream, const Handle&)
DEFINE_VALUE_FACTORY(FUTURE_STREAM, createFutureStream, const HandleSeq&&)
