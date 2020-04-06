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

#include <opencog/atoms/value/ValueFactory.h>
#include "FormulaTruthValue.h"

using namespace opencog;

FormulaTruthValue::FormulaTruthValue(const Handle& h)
	: SimpleTruthValue(0, 0), _formula(h), _as(h->getAtomSpace())
{
	update();
}

FormulaTruthValue::~FormulaTruthValue()
{}

void FormulaTruthValue::update(void) const
{
	if (_formula->is_evaluatable())
	{
		TruthValuePtr tvp = _formula->evaluate(_as);
		_value = tvp->value();
	}
	else if (_formula->is_executable())
	{
		ValuePtr vp = _formula->execute(_as);
		if (not nameserver().isA(vp->get_type(), FLOAT_VALUE))
			throw SyntaxException(TRACE_INFO,
				"Expecting FloatValue, got %s",
					vp->to_string().c_str());
		_value = FloatValueCast(vp)->value();
	}
	else
	{
		TruthValuePtr tvp = _formula->getTruthValue();
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
	rv += _formula->to_string(indent + "   ") + "\n";
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
