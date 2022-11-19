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

FormulaTruthValue::FormulaTruthValue(const Handle& h)
	: SimpleTruthValue(0, 0), _formula({h}), _as(h->getAtomSpace())
{
	_type = FORMULA_TRUTH_VALUE;
	update();
}

FormulaTruthValue::FormulaTruthValue(const Handle& stn, const Handle& cnf)
	: SimpleTruthValue(0, 0), _formula({stn, cnf}), _as(stn->getAtomSpace())
{
	_type = FORMULA_TRUTH_VALUE;
	update();
}

FormulaTruthValue::FormulaTruthValue(const HandleSeq&& seq)
	: SimpleTruthValue(0, 0), _formula(seq), _as(seq[0]->getAtomSpace())
{
	_type = FORMULA_TRUTH_VALUE;
	update();
}

FormulaTruthValue::~FormulaTruthValue()
{}

void FormulaTruthValue::update(void) const
{
	// If there are two formulas, they produce the strength and
	// the confidence, respectively.  We ignore more than two formulas.
	if (1 < _formula.size())
	{
		for (size_t i=0; i<2; i++)
		{
			const Handle& fo = _formula[i];
			if (not fo->is_executable())
				throw SyntaxException(TRACE_INFO,
					"Formula needs to be executable; got %s",
						fo->to_string().c_str());

			ValuePtr vp = fo->execute(_as);
			if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
				throw SyntaxException(TRACE_INFO,
					"Expecting FloatValue, got %s",
						vp->to_string().c_str());
			_value[i] = FloatValueCast(vp)->value()[0];
		}
		return;
	}

	// If there is just one formula, then we expect it to produce
	// two numbers, the strength and the confidence.
	const Handle& fo = _formula[0];
	if (fo->get_type() == PREDICATE_FORMULA_LINK)
	{
		TruthValuePtr tvp = EvaluationLink::do_evaluate(_as, fo);
		_value = tvp->value();
	}
	else if (fo->is_evaluatable())
	{
		TruthValuePtr tvp = fo->evaluate(_as);
		_value = tvp->value();
	}
	else if (fo->is_executable())
	{
		ValuePtr vp = fo->execute(_as);
		if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
			throw SyntaxException(TRACE_INFO,
				"Expecting FloatValue, got %s",
					vp->to_string().c_str());
		_value = FloatValueCast(vp)->value();
	}
	else
	{
		TruthValuePtr tvp = fo->getTruthValue();
		_value = tvp->value();
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

bool FormulaTruthValue::operator==(const Value& rhs) const
{
	if (FORMULA_TRUTH_VALUE != rhs.get_type()) return false;

	const FormulaTruthValue *ftv = dynamic_cast<const FormulaTruthValue *>(&rhs);
	return ftv->_formula == _formula;
}

DEFINE_VALUE_FACTORY(FORMULA_TRUTH_VALUE,
	createFormulaTruthValue, const Handle&)
