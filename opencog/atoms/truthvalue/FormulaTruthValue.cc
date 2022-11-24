/*
 * opencog/atoms/truthvalue/FormulaTruthValue.cc
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
 *
 */

#include <opencog/util/platform.h>
#include <opencog/util/exceptions.h>

#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/value/ValueFactory.h>
#include "FormulaTruthValue.h"

using namespace opencog;

FormulaTruthValue::FormulaTruthValue(const Handle& pred)
	: SimpleTruthValue(0, 0), _formula({pred}), _as(pred->getAtomSpace())
{
	init();
	update();
}

FormulaTruthValue::FormulaTruthValue(const Handle& stn, const Handle& cnf)
	: SimpleTruthValue(0, 0), _formula({stn, cnf}), _as(stn->getAtomSpace())
{
	init();
	update();
}

FormulaTruthValue::FormulaTruthValue(const HandleSeq&& seq)
	: SimpleTruthValue(0, 0), _formula(std::move(seq))
{
	if (0 == _formula.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting a formula!");

	_as = _formula[0]->getAtomSpace();
	init();
	update();
}

FormulaTruthValue::~FormulaTruthValue()
{}

void FormulaTruthValue::init(void)
{
	_type = FORMULA_TRUTH_VALUE;

	// If there is just one atom, then it is either a ListLink wrapping
	// a pair of formulas, or it is a predicate that is promising us
	// TV's in the future.
	if (1 == _formula.size() and LIST_LINK == _formula[0]->get_type())
		_formula = _formula[0]->getOutgoingSet();

	// We expect two formulas, they produce the strength and
	// the confidence, respectively.
	if (2 < _formula.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting no more than two formulas; got %d",
				_formula.size());

	if (1 == _formula.size())
	{
		if (not _formula[0]->is_type(EVALUATABLE_LINK) and
		    not _formula[0]->is_executable())
			throw SyntaxException(TRACE_INFO,
				"Expecting an evalutable or executable predicate, got %s",
				_formula[0]->to_string().c_str());
		return;
	}

	// If we are here, there are two.
	for (size_t i=0; i<2; i++)
	{
		if (not _formula[i]->is_executable())
			throw SyntaxException(TRACE_INFO,
				"Formula %d needs to be executable; got %s",
					i, _formula[i]->to_string().c_str());
	}
}

// XXX FIXME This update is not thread-safe.
void FormulaTruthValue::update(void) const
{
	if (1 == _formula.size())
	{
		const Handle& fut = _formula[0];
		if (fut->is_type(EVALUATABLE_LINK))
		{
			TruthValuePtr tvp = EvaluationLink::do_evaluate(_as, fut);
			_value = tvp->value();
			return;
		}

		if (fut->is_executable())
		{
			ValuePtr vp = fut->execute(_as);
			if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
				throw SyntaxException(TRACE_INFO,
					"Expecting FloatValue, got %s",
					vp->to_string().c_str());
			_value = FloatValueCast(vp)->value();
			return;
		}

		TruthValuePtr tvp = fut->getTruthValue();
		_value = tvp->value();
		return;
	}

	// If we are here, there are two atoms. We expect two formulas,
	// they produce the strength and the confidence, respectively.
	for (size_t i=0; i<2; i++)
	{
		ValuePtr vp = _formula[i]->execute(_as);
		if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
			throw SyntaxException(TRACE_INFO,
				"Expecting FloatValue, got %s",
					vp->to_string().c_str());
		_value[i] = FloatValueCast(vp)->value()[0];
	}
}

strength_t FormulaTruthValue::get_mean() const
{
	update();
	return _value[MEAN];
}

std::string FormulaTruthValue::to_string(const std::string& indent) const
{
	update();
	std::string rv = indent + "(FormulaTruthValue\n";
	for (const Handle& fo: _formula)
		rv += fo->to_short_string(indent + "   ") + "\n";
	rv += indent + "   ; Current sample:\n";
	rv += indent + "   ; " + SimpleTruthValue::to_string() + "\n)";
	return rv;
}

DEFINE_VALUE_FACTORY(FORMULA_TRUTH_VALUE,
	createFormulaTruthValue, const Handle&)
DEFINE_VALUE_FACTORY(FORMULA_TRUTH_VALUE,
	createFormulaTruthValue, const Handle&, const Handle&)
DEFINE_VALUE_FACTORY(FORMULA_TRUTH_VALUE,
	createFormulaTruthValue, const HandleSeq&&)
