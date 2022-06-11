/*
 * EncodeJson.cc
 * Encode Atoms and Values in a JSON format.
 *
 * Copyright (c) 2019, 2021 Linas Vepstas <linas@linas.org>
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
#include <iomanip>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Json.h"

using namespace opencog;

/* ================================================================== */
// Atom printers that do NOT print associated Values.

static std::string prt_node(const Handle& h, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "{\n" << indent << "  \"type\": \""
		<< nameserver().getTypeName(h->get_type())
		<< "\",\n" << indent << "  \"name\": ";
	ss << std::quoted(h->get_name());
	ss << "\n" << indent << "}";

	return ss.str();
}

static std::string prt_atom(const Handle&, const std::string& = "");

static std::string prt_link(const Handle& h, const std::string& indent)
{
	std::string idt = indent + "  ";
	std::string txt = indent + "{\n" + idt + "\"type\": \""
		+ nameserver().getTypeName(h->get_type()) + "\",\n"
		+ idt + "\"outgoing\": [\n";

	bool first = true;
	for (const Handle& ho : h->getOutgoingSet())
	{
		if (not first) { txt += ",\n"; } else { first = false; }
		txt += prt_atom(ho, idt + "  ");
	}
	txt += "]}";
	return txt;
}

static std::string prt_atom(const Handle& h, const std::string& indent)
{
	if (h->is_node()) return prt_node(h, indent);
	return prt_link(h, indent);
}

/// Convert the Atom into a string. It does NOT print any of the
/// associated values; use `dump_atom()` to get those.
std::string Json::encode_atom(const Handle& h, const std::string& indent)
{
	return prt_atom(h, indent);
}

/* ================================================================== */
// Same as above, for for printing values.

/// Convert value (or Atom) into a string.
std::string Json::encode_value(const ValuePtr& v, const std::string& indent)
{
	// Empty values are used to erase keys from atoms.
	if (nullptr == v) return "false";

	if (v->is_atom())
		return prt_atom(HandleCast(v));

	std::string idt = indent + "  ";
	std::string txt = indent + "{\n" + idt + "\"type\": \""
		+ nameserver().getTypeName(v->get_type()) + "\",\n"
		+ idt + "\"value\": [";

	Type typ = v->get_type();
	if (nameserver().isA(typ, FLOAT_VALUE))
	{
		// The FloatValue to_string() method prints out a high-precision
		// form of the value, as compared to SimpleTruthValue, which
		// only prints 6 digits and breaks the unit tests.
		FloatValuePtr fv(FloatValueCast(v));
		const std::vector<double>& fl = fv->value();
		bool first = true;
		for (double d : fl)
		{
			if (not first) txt += ", ";
			txt += std::to_string(d);
			first = false;
		}
	}

	else
	if (nameserver().isA(typ, STRING_VALUE))
	{
		StringValuePtr sv(StringValueCast(v));
		const std::vector<std::string>& sl = sv->value();
		bool first = true;
		for (const std::string& s : sl)
		{
			if (not first) txt += ", ";

			// Escape quotes
			std::stringstream ss;
			ss << std::quoted(s);
			txt += ss.str();
			first = false;
		}
	}

	else
	if (nameserver().isA(typ, LINK_VALUE))
	{
		LinkValuePtr lv(LinkValueCast(v));
		const std::vector<ValuePtr>& ll = lv->value();
		bool first = true;
		for (const ValuePtr& vp : ll)
		{
			if (not first) txt += ", ";
			txt += encode_value(vp);
			first = false;
		}
	}

	else
	{
		txt += "Error: don't know how to print this";
	}

	txt += "]}";
	return txt;
}

/* ================================================================== */

/// Get all of the values on an Atom and print them as an
/// association list.
std::string Json::encode_atom_values(const Handle& h)
{
	if (nullptr == h) return "[]\n";
	std::stringstream rv;

	rv << "[\n";
	bool first = true;
	for (const Handle& key: h->getKeys())
	{
		if (not first) { rv << ",\n"; } else { first = false; }
		rv << "  {\n";
		rv << "    \"key\": " << encode_atom(key, "    ") << ",\n";
		rv << "    \"value\": ";
		ValuePtr p = h->getValue(key);
		rv << encode_value(p, "    ") << "}";
	}
	rv << "]\n";
	return rv.str();
}

std::string Json::encode_type_list(const std::vector<Type>& tlist)
{
	std::string rv = "[";
	bool first = true;
	for (const Type& t: tlist)
	{
		if (not first) { rv += ", "; } else { first = false; }
		rv += "\"" + nameserver().getTypeName(t) + "\"";
	}
	rv += "]\n";
	return rv;
}

/* ============================= END OF FILE ================= */
