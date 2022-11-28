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
	StreamValue(FORMULA_STREAM), _formula({h}), _as(h->getAtomSpace())
{
	init();
}

FormulaStream::FormulaStream(const HandleSeq&& oset) :
	StreamValue(FORMULA_STREAM), _formula(std::move(oset))
{
	if (0 == _formula.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting at least one atom!");

	_as = _formula[0]->getAtomSpace();

	init();
}

// Same as above, but Handles as a ValueSeq. The Sexper decoder
// will create these when it deserializes FormulaStreams.
FormulaStream::FormulaStream(const ValueSeq& voset) :
	StreamValue(FORMULA_STREAM)
{
	for (const ValuePtr& v : voset)
	{
		Handle h(HandleCast(v));
		if (h) _formula.emplace_back(h);
	}

	if (0 == _formula.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting at least one atom!");

	_as = _formula[0]->getAtomSpace();

	init();
}

void FormulaStream::init(void)
{
	// If the single argument is a ListLink, unwrap it.
	if (1 == _formula.size() and LIST_LINK == _formula[0]->get_type())
	{
		_formula = _formula[0]->getOutgoingSet();
	}

	if (1 == _formula.size())
	{
		const Handle& h = _formula[0];
		ValuePtr vp;
		if (h->is_executable())
			vp = h->execute(_as);
		else if (h->is_evaluatable())
			vp = ValueCast(h->evaluate(_as));
		else
			throw SyntaxException(TRACE_INFO,
				"Expecting an executable or evaluatable atom, got %s",
				h->to_string().c_str());

		if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
			throw SyntaxException(TRACE_INFO,
				"Expecting formula to return a FloatValue, got %s",
				vp->to_string().c_str());

		_value = FloatValueCast(vp)->value();
		return;
	}

	for (const Handle& h: _formula)
	{
		ValuePtr vp;
		if (h->is_executable())
			vp = h->execute(_as);
		else
			throw SyntaxException(TRACE_INFO,
				"Expecting an executable atom, got %s",
				h->to_string().c_str());

		if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
			throw SyntaxException(TRACE_INFO,
				"Expecting formula to return a FloatValue, got %s",
				vp->to_string().c_str());

		_value.emplace_back(FloatValueCast(vp)->value()[0]);
	}
}

// ==============================================================

// XXX FIXME The update here is not thread-safe...
void FormulaStream::update() const
{
	if (1 == _formula.size())
	{
		if (_formula[0]->is_evaluatable())
			_value = _formula[0]->evaluate(_as)->value();
		else if (_formula[0]->is_executable())
			_value = FloatValueCast(_formula[0]->execute(_as))->value();
		return;
	}

	// If there are multiple arguments, assume that each one returns a
	// single Float. Just concatenate all of them. Just execute to get it;
	// I cannot imagine why evaluating would be useful, here.
	std::vector<double> newval;
	for (const Handle& h :_formula)
		newval.push_back(FloatValueCast(h->execute(_as))->value()[0]);

	_value.swap(newval);
}

// ==============================================================

std::string FormulaStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	for (const Handle& h : _formula)
		rv += "\n" + h->to_short_string(indent + "   ");
	rv += "\n" + indent + "   ; Current sample:\n";
	rv += indent + "   ; " + FloatValue::to_string("", FLOAT_VALUE);
	rv += "\n)";
	return rv;
}

// ==============================================================

bool FormulaStream::operator==(const Value& other) const
{
	// If they are both FormulaStream's, then we're good.
	if (FORMULA_STREAM == other.get_type())
	{
		const FormulaStream* eso = (const FormulaStream*) &other;
		return eso->_formula == _formula;
	}

	if (not other.is_type(FLOAT_VALUE)) return false;

	// Value-compare
	return FloatValue::operator==(other);
}

// ==============================================================

// Adds factor when library is loaded.
DEFINE_VALUE_FACTORY(FORMULA_STREAM, createFormulaStream, const Handle&)
DEFINE_VALUE_FACTORY(FORMULA_STREAM, createFormulaStream, const HandleSeq&&)
DEFINE_VALUE_FACTORY(FORMULA_STREAM, createFormulaStream, const ValueSeq&)
