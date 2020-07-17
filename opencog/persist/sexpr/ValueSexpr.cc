/*
 * ValueSexpr.cc
 * Encode/decode S-Expressions for Atomese Values.
 *
 * Copyright (c) 2019 Linas Vepstas <linas@linas.org>
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/base/Valuation.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

#include "Sexpr.h"

using namespace opencog;

/* ================================================================== */

/**
 * Return a Value correspnding to the input string.
 * It is assumed the input string is encoded as a scheme string.
 * For example, `(FloatValue 1 2 3 4)`
 *
 * XXX FIXME This needs to be fuzzed; it is very likely to crash
 * and/or contain bugs if it is given strings of unexpected formats.
 */
ValuePtr Sexpr::decode_value(std::string& stv, size_t& pos)
{
	size_t totlen = stv.size();

#define LV "(LinkValue"
	if (0 == stv.compare(pos, sizeof(LV)-1, LV))
	{
		size_t vos = pos + sizeof(LV)-1;
		std::vector<ValuePtr> vv;
		vos = stv.find('(', vos);
		size_t epos = vos;
		size_t done = vos + 1;
		while (vos != std::string::npos and vos < done)
		{
			// Find the next balanced paren, and restart there.
			// This is not very efficient, but it works.
			epos = vos;
			int pcnt = 1;
			while (0 < pcnt and epos < totlen)
			{
				char c = stv[++epos];
				if ('(' == c) pcnt ++;
				else if (')' == c) pcnt--;
			}
			if (epos >= totlen)
				throw SyntaxException(TRACE_INFO,
					"Malformed LinkValue: %s", stv.substr(pos).c_str());

			vv.push_back(decode_value(stv, vos));
			done = stv.find(')', epos+1);
			vos = stv.find('(', epos+1);
		}
		if (std::string::npos == done)
			throw SyntaxException(TRACE_INFO,
				"Malformed LinkValue: %s", stv.substr(pos).c_str());
		pos = done + 1;
		return createLinkValue(vv);
	}

#define FV "(FloatValue"
	if (0 == stv.compare(pos, sizeof(FV)-1, FV))
	{
		size_t vos = pos + sizeof(FV)-1;
		std::vector<double> fv;
		while (vos < totlen and stv[vos] != ')')
		{
			size_t epos;
			fv.push_back(stod(stv.substr(vos), &epos));
			vos += epos;
		}
		pos = vos + 1;
		return createFloatValue(fv);
	}

#define TVL "(SimpleTruthValue "
#define TVS "(stv "
	size_t vos = std::string::npos;
	if (0 == stv.compare(pos, sizeof(TVL)-1, TVL))
		vos = pos + sizeof(TVL) - 1;
	else
	if (0 == stv.compare(pos, sizeof(TVS)-1, TVS))
		vos = pos + sizeof(TVS) - 1;

	if (std::string::npos != vos)
	{
		size_t elen;
		double strength = stod(stv.substr(vos), &elen);
		vos += elen;
		double confidence = stod(stv.substr(vos), &elen);
		vos += elen;
		vos = stv.find(')', vos);
		if (std::string::npos == vos)
			throw SyntaxException(TRACE_INFO,
				"Malformed SimpleTruthValue: %s", stv.substr(pos).c_str());
		pos = vos + 1;
		return ValueCast(createSimpleTruthValue(strength, confidence));
	}

#define CTV "(CountTruthValue "
	if (0 == stv.compare(pos, sizeof(CTV)-1, CTV))
	{
		size_t vos = pos + sizeof(CTV) - 1;
		size_t elen;
		double strength = stod(stv.substr(vos), &elen);
		vos += elen;
		double confidence = stod(stv.substr(vos), &elen);
		vos += elen;
		double count = stod(stv.substr(vos), &elen);
		vos += elen;
		vos = stv.find(')', vos);
		if (std::string::npos == vos)
			throw SyntaxException(TRACE_INFO,
				"Malformed CountTruthValue: %s", stv.substr(pos).c_str());
		pos = vos + 1;
		return ValueCast(createCountTruthValue(strength, confidence, count));
	}

	// XXX FIXME this mishandles escaped quotes
#define SV "(StringValue"
	if (0 == stv.compare(pos, sizeof(SV)-1, SV))
	{
		size_t vos = pos + sizeof(SV) - 1;
		std::vector<std::string> sv;
		size_t epos = stv.find(')', vos+1);
		if (std::string::npos == epos)
			throw SyntaxException(TRACE_INFO,
				"Malformed StringValue: %s", stv.substr(pos).c_str());
		while (vos < epos)
		{
			vos = stv.find('\"', vos);
			if (std::string::npos == vos) break;
			size_t evos = stv.find('\"', vos+1);
			sv.push_back(stv.substr(vos+1, evos-vos-1));
			vos = evos+1;
		}
		pos = epos + 1;
		return createStringValue(sv);
	}

	throw SyntaxException(TRACE_INFO, "Unknown Value %s",
		stv.substr(pos).c_str());
}

/* ================================================================== */

/**
 * Decode a Valuation association list.
 * This list has the format
 * ((KEY . VALUE)(KEY2 . VALUE2)...)
 * Store the results as values on the atom.
 */
void Sexpr::decode_alist(Handle& atom, std::string& alist)
{
	// Skip over opening paren
	size_t pos = 1;
	size_t totlen = alist.size();
	pos = alist.find('(', pos);
	while (std::string::npos != pos and pos < totlen)
	{
		++pos;  // over first paren of pair
		Handle key(decode_atom(alist, pos));
		pos = alist.find(" . ", pos);
		pos += 3;
		ValuePtr val(decode_value(alist, pos));
		atom->setValue(key, val);
		pos = alist.find('(', pos);
	}
}

/* ================================================================== */

/// Convert value (or Atom) into a string.
std::string Sexpr::encode_value(const ValuePtr& v)
{
	if (nameserver().isA(v->get_type(), FLOAT_VALUE))
	{
		// The FloatValue to_string() print prints out a high-precision
		// form of the value, as compared to SimpleTruthValue, which
		// only prints 6 digits and breaks the unit tests.
		FloatValuePtr fv(FloatValueCast(v));
		return fv->FloatValue::to_string();
	}
	return v->to_short_string();
}

/* ================================================================== */

/// Get all of the values on an Atom and print them as an
/// association list.
std::string Sexpr::encode_atom_values(const Handle& h)
{
	std::stringstream rv;

	rv << "(list ";
	for (const Handle& k: h->getKeys())
	{
		ValuePtr p = h->getValue(k);
		rv << "(cons " << k->to_short_string() << encode_value(p) + ")";
	}
	rv << ")";
	return rv.str();
}

/* ============================= END OF FILE ================= */
