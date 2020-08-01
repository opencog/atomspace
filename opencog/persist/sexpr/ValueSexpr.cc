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
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Sexpr.h"

using namespace opencog;

/* ================================================================== */
/**
 * Look for a type name, either of the form "ConceptNode" (wwith quotes)
 * or 'ConceptNode (symbol) starting at location `pos` in `tna`.
 * Return the type and update `pos` to point after the typename.
 */
Type Sexpr::decode_type(const std::string& tna, size_t& pos)
{
	// Advance past whitespace.
	pos = tna.find_first_not_of(" \n\t", pos);
	if (std::string::npos == pos)
		throw SyntaxException(TRACE_INFO, "Bad Type >>%s<<",
			tna.substr(pos).c_str());

	// Advance to next whitespace.
	size_t nos = tna.find_first_of(") \n\t", pos);
	if (std::string::npos == nos)
		nos = tna.size();

	size_t sos = nos;
	if ('\'' == tna[pos]) pos++;
	if ('"' == tna[pos]) { pos++; sos--; }

	Type t = nameserver().getType(tna.substr(pos, sos-pos));
	if (NOTYPE == t)
		throw SyntaxException(TRACE_INFO, "Unknown Type >>%s<<",
			tna.substr(pos, sos-pos).c_str());

	pos = nos;
	return t;
}

/* ================================================================== */

/**
 * Return a Value corresponding to the input string.
 * It is assumed the input string is encoded as a scheme string.
 * For example, `(FloatValue 1 2 3 4)` or more complex things:
 * `(LinkValue (Concept "a") (FloatValue 1 2 3))`
 *
 * The `pos` should point at the open-paren of the value-string.
 * Upon return, `pos` is updated to point at the matching closing paren.
 *
 * It is currently assumed that there is no whitespace between the
 * open-paren, and the string encoding the value.
 *
 * XXX FIXME This needs to be fuzzed; it is very likely to crash
 * and/or contain bugs if it is given strings of unexpected formats.
 */
ValuePtr Sexpr::decode_value(const std::string& stv, size_t& pos)
{
	size_t totlen = stv.size();

	// Skip past whitespace
	pos = stv.find_first_not_of(" \n\t", pos);

	// Special-case: Both #f and '() are used to denote "no value".
	// This is commonly used to erase keys from atoms. So handle this
	// first.
	if (0 == stv.compare(pos, 2, "#f"))
	{
		pos += 2;
		return nullptr;
	}
	if (0 == stv.compare(pos, 3, "'()"))
	{
		pos += 3;
		return nullptr;
	}

	// What kind of value is it?
	// Increment pos by one to point just after the open-paren.
	size_t vos = stv.find_first_of(" \n\t", ++pos);
	if (std::string::npos == vos)
		throw SyntaxException(TRACE_INFO, "Badly formatted Value %s",
			stv.substr(pos).c_str());

	Type vtype = nameserver().getType(stv.substr(pos, vos-pos));
	if (NOTYPE == vtype)
	{
		if (0 == stv.compare(pos, 3, "stv"))
			vtype = SIMPLE_TRUTH_VALUE;
		else
		if (0 == stv.compare(pos, 3, "ctv"))
			vtype = COUNT_TRUTH_VALUE;
		else
		throw SyntaxException(TRACE_INFO, "Unknown Value >>%s<<",
			stv.substr(pos, vos-pos).c_str());
	}

	if (nameserver().isA(vtype, ATOM))
	{
		// Decrement pos by one to point at the open-paren.
		return decode_atom(stv, --pos);
	}

	if (nameserver().isA(vtype, LINK_VALUE))
	{
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
		return valueserver().create(vtype, vv);
	}

	if (nameserver().isA(vtype, FLOAT_VALUE))
	{
		std::vector<double> fv;
		while (vos < totlen and stv[vos] != ')')
		{
			size_t epos;
			fv.push_back(stod(stv.substr(vos), &epos));
			vos += epos;
		}
		pos = vos + 1;

		return valueserver().create(vtype, fv);
	}

	// XXX FIXME this mishandles escaped quotes
	if (nameserver().isA(vtype, STRING_VALUE))
	{
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
		return valueserver().create(vtype, sv);
	}

	throw SyntaxException(TRACE_INFO, "Unsupported decode of Value %s",
		stv.substr(pos, vos-pos).c_str());
}

/* ================================================================== */

/**
 * Decode a Valuation association list.
 * This list has the format
 * ((KEY . VALUE)(KEY2 . VALUE2)...)
 * Store the results as values on the atom.
 */
void Sexpr::decode_alist(Handle& atom, const std::string& alist, size_t& pos)
{
	AtomSpace* as = atom->getAtomSpace();

	pos = alist.find_first_not_of(" \n\t", pos);
	if (std::string::npos == pos) return;
	if ('(' != alist[pos])
		throw SyntaxException(TRACE_INFO, "Badly formed alist: %s",
			alist.substr(pos).c_str());

	// Skip over opening paren
	pos++;
	size_t totlen = alist.size();
	pos = alist.find('(', pos);
	while (std::string::npos != pos and pos < totlen)
	{
		++pos;  // over first paren of pair
		Handle key(decode_atom(alist, pos));

		pos = alist.find(" . ", pos);
		pos += 3;
		ValuePtr val(decode_value(alist, pos));

		// Make sure all atoms have found a nice home.
		if (as)
		{
			Handle hkey = as->add_atom(key);
			if (hkey) key = hkey; // might be null, if `as` is read-only
			val = add_atoms(as, val);
		}
		atom->setValue(key, val);
		pos = alist.find('(', pos);
	}
}

/* ================================================================== */

/**
 * Decode a Valuation association list.
 * This list has the format
 * (list (cons KEY VALUE)(cons KEY2 VALUE2)...)
 * Store the results as values on the atom.
 */
void Sexpr::decode_slist(Handle& atom, const std::string& alist, size_t& pos)
{
	AtomSpace* as = atom->getAtomSpace();

	pos = alist.find_first_not_of(" \n\t", pos);
	if (std::string::npos == pos) return;
	if (alist.compare(pos, 5, "(list"))
		throw SyntaxException(TRACE_INFO, "Badly formed alist: %s",
			alist.substr(pos).c_str());

	size_t totlen = alist.size();
	pos = alist.find("(cons ", pos);
	while (std::string::npos != pos and pos < totlen)
	{
		pos += 5;
		pos = alist.find_first_not_of(" \n\t", pos);
		Handle key(decode_atom(alist, pos));
		pos++;
		pos = alist.find_first_not_of(" \n\t", pos);
		ValuePtr val(decode_value(alist, pos));
		if (as)
		{
			// Make sure all atoms have found a nice home.
			Handle hkey = as->add_atom(key);
			if (hkey) key = hkey; // might be null, if `as` is read-only
			val = add_atoms(as, val);
		}
		atom->setValue(key, val);
		pos = alist.find("(cons ", pos);
	}
}

/* ================================================================== */

static std::string prt_node(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type())
		+ " \"" + h->get_name() + "\")";
	return txt;
}

static std::string prt_atom(const Handle&);

static std::string prt_link(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho);
	txt += ")";
	return txt;
}

static std::string prt_atom(const Handle& h)
{
	if (h->is_node()) return prt_node(h);
	return prt_link(h);
}

std::string Sexpr::encode_atom(const Handle& h)
{
	return prt_atom(h);
}

/// Convert value (or Atom) into a string.
std::string Sexpr::encode_value(const ValuePtr& v)
{
	// Empty values are used to erase keys from atoms.
	if (nullptr == v) return " #f";

	if (nameserver().isA(v->get_type(), FLOAT_VALUE))
	{
		// The FloatValue to_string() print prints out a high-precision
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
std::string Sexpr::encode_atom_values(const Handle& h)
{
	std::stringstream rv;

	rv << "(list ";
	for (const Handle& k: h->getKeys())
	{
		ValuePtr p = h->getValue(k);
		rv << "(cons " << prt_atom(k) << encode_value(p) + ")";
	}
	rv << ")";
	return rv.str();
}

/* ================================================================== */

/// Make sure that any Atoms appearing buried in the value have found
/// a nice home to live in.
ValuePtr Sexpr::add_atoms(AtomSpace* as, const ValuePtr& vptr)
{
	Type t = vptr->get_type();
	if (nameserver().isA(t, ATOM))
	{
		Handle h = as->add_atom(HandleCast(vptr));
		if (h) return h; // Might be null if `as` is read-only.
		return vptr;
	}

	if (nameserver().isA(t, LINK_VALUE))
	{
		std::vector<ValuePtr> vvec;
		for (const ValuePtr& v : LinkValueCast(vptr)->value())
			vvec.push_back(add_atoms(as, v));

		return valueserver().create(t, vvec);
	}
	return vptr;
}

/* ============================= END OF FILE ================= */
