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
#include <iomanip>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Sexpr.h"

using namespace opencog;

/* ================================================================== */
/**
 * Look for a type name, either of the form "ConceptNode" (with quotes)
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

	// LinkValues are vectors of Values.
	// ListValues are possibly other types (such as FloatValues)
	// but contain Atoms in thier configuration data, and thus
	// need to be desrialized as if they were LinkValues (even
	// though they are not.) FormulaStream is an example.
	if (nameserver().isA(vtype, LINK_VALUE) or
	    nameserver().isA(vtype, LIST_VALUE))
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

				// Advance past escaped quotes.
				if ('"' == c)
				{
					++epos;
					// Search for ending quote, advancing past escaped quotes.
					while (epos < totlen and stv[epos] != '"')
					{
						if (stv[epos] == '\\') epos++;
						epos++;
					}
					continue;
				}
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

	// Unescape escaped quotes
	if (nameserver().isA(vtype, STRING_VALUE))
	{
		std::vector<std::string> sv;
		size_t p = vos+1;

		// p is pointing to the opening quote.
		while ('"' == stv[p])
		{
			size_t e = p + 1;

			// Search for ending quote, advancing past escaped quotes.
			while (e < totlen and stv[e] != '"')
			{
				if (stv[e] == '\\') e++;
				e++;
			}
			e++; // Now e points just past the close-quote.
			if (totlen <= e)
				throw SyntaxException(TRACE_INFO,
					"Malformed StringValue: %s", stv.substr(vos).c_str());

			// Unescape quotes in the string.
			std::stringstream ss;
			std::string val;
			ss << stv.substr(p, e-p);
			ss >> quoted(val);
			sv.push_back(val);

			// Skip past whitespace
			p = stv.find_first_not_of(" \n\t", e);
		}
		if (')' != stv[p])
			throw SyntaxException(TRACE_INFO,
				"Missing closing paren at %zu (after %zu strings) in StringValue: %s",
				p, sv.size(), stv.substr(vos).c_str());

		pos = ++p;
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
void Sexpr::decode_alist(const Handle& atom,
                         const std::string& alist, size_t& pos)
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
			as->set_value(atom, key, val);
		}
		else
			atom->setValue(key, val);
		pos = alist.find('(', pos);
	}
}

/* ================================================================== */

/**
 * Decode a Valuation association list.
 * This list has the format
 *    (alist (cons KEY VALUE)(cons KEY2 VALUE2)...)
 * Store the results as values on the atom.
 */
void Sexpr::decode_slist(const Handle& atom,
                         const std::string& alist, size_t& pos)
{
	AtomSpace* as = atom->getAtomSpace();

	pos = alist.find_first_not_of(" \n\t", pos);
	if (std::string::npos == pos) return;
	if (alist.compare(pos, 6, "(alist"))
		throw SyntaxException(TRACE_INFO, "Badly formed alist: %s",
			alist.substr(pos).c_str());

	size_t totlen = alist.size();
	size_t nxt = alist.find("(cons ", pos);
	while (std::string::npos != nxt and nxt < totlen)
	{
		nxt += 5;
		nxt = alist.find_first_not_of(" \n\t", nxt);
		Handle key(decode_atom(alist, nxt));
		nxt++;
		nxt = alist.find_first_not_of(" \n\t", nxt);
		ValuePtr val(decode_value(alist, nxt));
		pos = nxt + 1;   // move past closing paren of (cons ...)
		if (as)
		{
			// Make sure all atoms have found a nice home.
			Handle hkey = as->add_atom(key);
			if (hkey) key = hkey; // might be null, if `as` is read-only
			val = add_atoms(as, val);
			as->set_value(atom, key, val);
		}
		else
			atom->setValue(key, val);
		nxt = alist.find("(cons ", nxt);
	}

	// Move past closing parent of (alist ...)
	pos = alist.find(")", pos);
	pos++;
}

/* ================================================================== */
// Atom printers that do NOT print associated Values.

static std::string prt_node(const Handle& h, bool multispace)
{
	std::stringstream ss;
	ss << "(" << nameserver().getTypeName(h->get_type())
		<< " " << std::quoted(h->get_name());

	if (multispace and h->getAtomSpace())
		ss << " (AtomSpace \"" << h->getAtomSpace()->get_name() << "\")";

	ss << ")";

	return ss.str();
}

static std::string prt_atom(const Handle&, bool);

static std::string prt_link(const Handle& h, bool multispace)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho, multispace);

	if (multispace and h->getAtomSpace())
		txt += " (AtomSpace \"" + h->getAtomSpace()->get_name() + "\")";

	txt += ")";
	return txt;
}

static std::string prt_atom(const Handle& h, bool multispace)
{
	if (h->is_node()) return prt_node(h, multispace);
	return prt_link(h, multispace);
}

/// Convert the Atom into a string. It does NOT print any of the
/// associated values; use `dump_atom()` to get those.
std::string Sexpr::encode_atom(const Handle& h, bool multispace)
{
	return prt_atom(h, multispace);
}

/// Convert value (or Atom) into a string.
std::string Sexpr::encode_value(const ValuePtr& v)
{
	// Empty values are used to erase keys from atoms.
	if (nullptr == v) return " #f";

	// The FloatValue to_string() method prints out a high-precision
	// form of the value, as compared to SimpleTruthValue, which
	// only prints 6 digits and breaks the unit tests.
	// Only TruthValues have this issue.
	if (v->is_type(TRUTH_VALUE))
	{
		FloatValuePtr fv(FloatValueCast(v));
		return fv->FloatValue::to_string();
	}

	if (not v->is_atom())
		return v->to_short_string();
	return prt_atom(HandleCast(v), false);
}

/* ================================================================== */

/// Get all of the values on an Atom and print them as an
/// association list.
std::string Sexpr::encode_atom_values(const Handle& h)
{
	std::stringstream rv;

	rv << "(alist ";
	for (const Handle& k: h->getKeys())
	{
		ValuePtr p = h->getValue(k);
		rv << "(cons " << prt_atom(k, false) << encode_value(p) << ")";
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
		ss << " " << Sexpr::encode_atom_values(h);

	ss << ")";

	return ss.str();
}

static std::string dump_link(const Handle& h)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho, false);

	if (h->haveValues())
	{
		txt += " ";
		txt += Sexpr::encode_atom_values(h);
	}

	txt += ")";
	return txt;
}

/// Print the Atom, and all of the values attached to it.
/// Similar to `encode_atom()`, except that it also prints the values.
/// Values on going Atoms in a Link are NOT dumped!
/// This is in order to avoid duplication.
std::string Sexpr::dump_atom(const Handle& h)
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
		ss << " (alist (cons " << prt_atom(key, false) << Sexpr::encode_value(p) << ")))";

	return ss.str();
}

static std::string dump_vlink(const Handle& h, const Handle& key)
{
	std::string txt = "(" + nameserver().getTypeName(h->get_type()) + " ";
	for (const Handle& ho : h->getOutgoingSet())
		txt += prt_atom(ho, false);

	ValuePtr p = h->getValue(key);
	if (nullptr != p)
		txt += " (alist (cons " + prt_atom(key, false) + Sexpr::encode_value(p) + ")))";

	return txt;
}

/// Print the Atom, and just one of the values attached to it.
std::string Sexpr::dump_vatom(const Handle& h, const Handle& key)
{
	if (h->is_node()) return dump_vnode(h, key);
	return dump_vlink(h, key);
}

/* ================================================================== */

/// Make sure that any Atoms appearing buried in the value have found
/// a nice home to live in.
ValuePtr Sexpr::add_atoms(AtomSpace* as, const ValuePtr& vptr)
{
	return as->add_atoms(vptr);
}

/* ============================= END OF FILE ================= */
