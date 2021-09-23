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

/// Convert value (or Atom) into a string.
std::string Json::encode_value(const ValuePtr& v, const std::string& indent)
{
	// Empty values are used to erase keys from atoms.
	if (nullptr == v) return "false";

	if (nameserver().isA(v->get_type(), FLOAT_VALUE))
	{
		// The FloatValue to_string() method prints out a high-precision
		// form of the value, as compared to SimpleTruthValue, which
		// only prints 6 digits and breaks the unit tests.
		FloatValuePtr fv(FloatValueCast(v));
		return fv->FloatValue::to_string();
	}

	if (not v->is_atom())
		return v->to_short_string();
	return prt_atom(HandleCast(v));
}

/* ================================================================== */

/// Get all of the values on an Atom and print them as an
/// association list.
std::string Json::encode_atom_values(const Handle& h)
{
	std::stringstream rv;

	rv << "(alist ";
	for (const Handle& k: h->getKeys())
	{
		ValuePtr p = h->getValue(k);
		rv << "(cons " << prt_atom(k) << encode_value(p) << ")";
	}
	rv << ")";
	return rv.str();
}

/* ================================================================== */
// Atom printers that encode ALL associated Values.

static std::string dump_node(const Handle& h)
{
	std::stringstream ss;
	ss << "(" << nameserver().getTypeName(h->get_type())
		<< " " << std::quoted(h->get_name());

	if (h->haveValues())
		ss << " " << Json::encode_atom_values(h);

	ss << ")";

	return ss.str();
}

static std::string dump_link(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho);

	if (h->haveValues())
	{
		txt += " ";
		txt += Json::encode_atom_values(h);
	}

	txt += ")";
	return txt;
}

/// Print the Atom, and all of the values attached to it.
/// Similar to `encode_atom()`, except that it also prints the values.
/// Values on going Atoms in a Link are NOT dumped!
/// This is in order to avoid duplication.
std::string Json::dump_atom(const Handle& h)
{
	if (h->is_node()) return dump_node(h);
	return dump_link(h);
}

/* ================================================================== */
// Atom printers that encode only one associated Value.

static std::string dump_vnode(const Handle& h, const Handle& key)
{
	std::stringstream ss;
	ss << "(" << nameserver().getTypeName(h->get_type())
		<< " " << std::quoted(h->get_name());

	ValuePtr p = h->getValue(key);
	if (nullptr != p)
		ss << " (alist (cons " << prt_atom(key) << Json::encode_value(p) << ")))";

	return ss.str();
}

static std::string dump_vlink(const Handle& h, const Handle& key)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho);

	ValuePtr p = h->getValue(key);
	if (nullptr != p)
		txt += " (alist (cons " + prt_atom(key) + Json::encode_value(p) + ")))";

	return txt;
}

/// Print the Atom, and just one of the values attached to it.
std::string Json::dump_vatom(const Handle& h, const Handle& key)
{
	if (h->is_node()) return dump_vnode(h, key);
	return dump_vlink(h, key);
}

/* ============================= END OF FILE ================= */
