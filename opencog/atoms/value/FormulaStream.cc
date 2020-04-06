/*
 * opencog/atoms/value/FormulaStream.cc
 *
 * Copyright (C) 2020 Linas Vepstas
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
#include <opencog/atoms/value/FormulaStream.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// ==============================================================

FormulaStream::FormulaStream(const Handle& h) :
	StreamValue(FORMULA_STREAM), _formula(h), _as(h->getAtomSpace())
{
	ValuePtr vp;
	if (h->is_executable())
	{
		vp = h->execute(_as);
	}
	else if (h->is_evaluatable())
	{
		vp = ValueCast(h->evaluate(_as));
	}
	else
	{
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable/evaluatable atom, got %s",
			h->to_string().c_str());
	}

	if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
		throw SyntaxException(TRACE_INFO,
			"Expecting formula to return a FloatValue, got %s",
			vp->to_string().c_str());

	_value = FloatValueCast(vp)->value();
}

// ==============================================================

void FormulaStream::update() const
{
	FloatValuePtr vp;
	if (_formula->is_evaluatable())
	{
		vp = _formula->evaluate(_as);
	}
	else if (_formula->is_executable())
	{
		vp = FloatValueCast(_formula->execute(_as));
	}

	_value = vp->value();
}

// ==============================================================

std::string FormulaStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n" + _formula->to_string(indent + "   ");
	rv += "\n" + indent + "   ; Current sample:\n";
	rv += indent + "   ; " + FloatValue::to_string("", FLOAT_VALUE);
	rv += "\n)";
	return rv;
}

// ==============================================================

bool FormulaStream::operator==(const Value& other) const
{
	if (FORMULA_STREAM != other.get_type()) return false;

	const FormulaStream* eso = (const FormulaStream*) &other;
	return eso->_formula == _formula;
}

// ==============================================================

// Adds factor when library is loaded.
DEFINE_VALUE_FACTORY(FORMULA_STREAM, createFormulaStream, const Handle&)
