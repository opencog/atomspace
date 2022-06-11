/*
 * DecodeJson.cc
 * Decode JSON describing Atoms and Values.
 *
 * Copyright (c) 2019, 2022 Linas Vepstas <linas@linas.org>
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
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Json.h"

using namespace opencog;

// This does NOT use any external JSON decoder libraries because those
// libs don't really make anything simpler, and also we don't need most
// of the features that they offer, and also I don't want more
// dependencies in the AtomSpace.

/* ================================================================== */
/**
 * Look for a type name of the form "ConceptNode" (with quotes)
 * starting at location `pos` in `tna`.
 * Return the type and update `pos` to point after the typename.
 */
Type Json::decode_type(const std::string& tna, size_t& pos)
{
	// Advance past whitespace.
	pos = tna.find_first_not_of(" \n\t", pos);
	if (std::string::npos == pos)
		throw SyntaxException(TRACE_INFO, "Bad Type >>%s<<",
			tna.substr(pos).c_str());

	// Advance to next whitespace.
	size_t nos = tna.find_first_of(",) \n\t", pos);
	if (std::string::npos == nos)
		nos = tna.size();

	size_t sos = nos;
	if ('"' == tna[pos]) { pos++; sos--; }

	Type t = nameserver().getType(tna.substr(pos, sos-pos));
	if (NOTYPE == t)
		throw SyntaxException(TRACE_INFO, "Unknown Type >>%s<<",
			tna.substr(pos, sos-pos).c_str());

	pos = nos;
	return t;
}

/* ================================================================== */

/// Extracts Node name-string. Given the string `s`, this updates
/// the `l` and `r` values such that `l` points at the first
/// non-whitespace character of the name, and `r` points at the last.
/// The string is considered to start *after* the first quote, and ends
/// just before the last quote. In this case, escaped quotes \" are
/// ignored (are considered to be part of the string).
///
/// This returns the unescaped node name.
///
std::string Json::get_node_name(const std::string& s,
                                size_t& l, size_t& r)
{
	// Advance past whitespace.
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;

	l++;
	size_t p = l;
	for (; p < r and (s[p] != '"' or ((0 < p) and (s[p - 1] == '\\'))); p++);
	r = p;

	// We use std::quoted() to unescape embedded quotes.
	// Unescaping works ONLY if the leading character is a quote!
	// So readjust left and right to pick those up.
	if ('"' == s[l-1]) l--; // grab leading quote, for std::quoted().
	if ('"' == s[r]) r++;   // step past trailing quote.
	std::stringstream ss;
	std::string name;
	ss << s.substr(l, r-l);
	ss >> std::quoted(name);
	return name;
}

/* ================================================================== */

/// Convert an Atomese JSON expression into a C++ Atom.
/// For example: `{ "type": "Concept", "name": "foo" }`
/// will return the corresponding atom.
///
/// The string to decode is `s`, beginning at location `l` and using `r`
/// as a hint for the end of the expression.
///
Handle Json::decode_atom(const std::string& s,
                         size_t& l, size_t& r)
{
	l = s.find("{", l);
	if (std::string::npos == l) return Handle::UNDEFINED;

	size_t tpos = s.find("\"type\":", l);
	if (std::string::npos == tpos) return Handle::UNDEFINED;
	tpos += 7;  // skip past "type":

	Type t = NOTYPE;
	try {
		t = Json::decode_type(s, tpos);
	}
	catch(...) {
		return Handle::UNDEFINED;
	}

	if (nameserver().isA(t, NODE))
	{
		size_t apos = s.find("\"name\":", l);
		if (std::string::npos == apos) return Handle::UNDEFINED;
		apos += 7;  // skip past "name":

		apos = s.find_first_not_of(" \n\t", apos);
		std::string name = Json::get_node_name(s, apos, r);

		r = s.find_first_of(",}", r); // Move past the closing paren
		r++;
		return createNode(t, std::move(name));
	}

	if (nameserver().isA(t, LINK))
	{
		size_t opos = s.find("\"outgoing\":", l);
		if (std::string::npos == opos) return Handle::UNDEFINED;
		opos += 11;  // skip past "outgoing":

		l = s.find("{", opos);
		size_t epos = r;

		HandleSeq hs;

		while (std::string::npos != r)
		{
			Handle ho = Json::decode_atom(s, l, r);
			if (nullptr == ho) return Handle::UNDEFINED;
			hs.push_back(ho);

			// Look for the comma
			l = s.find(",", r);
			if (std::string::npos == l) break;
			l ++;
			r = epos;
		}

		return createLink(std::move(hs), t);
	}

	return Handle::UNDEFINED;
}

/* ================================================================== */

/// Convert an Atomese JSON expression into a C++ Value.
/// For example: `{ "type": "FloatValue", "value": [1, 2, 3] }`
/// will return the corresponding ValuePtr.
///
/// The string to decode is `s`, beginning at location `l` and using `r`
/// as a hint for the end of the expression.
///
ValuePtr Json::decode_value(const std::string& s,
                            size_t& lo, size_t& ro)
{
	size_t l = lo;
	l = s.find("{", l);
	if (std::string::npos == l) return nullptr;

	size_t tpos = s.find("\"type\":", l);
	if (std::string::npos == tpos) return nullptr;
	tpos += 7;  // skip past "type":

	Type t = NOTYPE;
	try {
		t = Json::decode_type(s, tpos);
	}
	catch(...) {
		return nullptr;
	}

	if (nameserver().isA(t, ATOM))
	{
		return decode_atom(s, lo, ro);
	}

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		l = tpos;
		size_t opos = s.find("\"value\":", l);
		if (std::string::npos == opos) return nullptr;
		opos += 8;  // skip past "value":

		l = s.find("[", opos);

		size_t r = ro;
		std::vector<double> vd;
		while (std::string::npos != l)
		{
			l++;
			r = s.find_first_of(",]", l);
			if (std::string::npos == r) break;
			std::stringstream ss;
			ss << s.substr(l, r-l);
			double d;
			ss >> d;
			vd.push_back(d);

			if (']' == s[r]) break;
			l = r;
		}

		r = s.find("}", r);
		ro = r;
		return createFloatValue(std::move(vd));
	}

	if (nameserver().isA(t, STRING_VALUE))
	{
		l = tpos;
		size_t opos = s.find("\"value\":", l);
		if (std::string::npos == opos) return nullptr;
		opos += 8;  // skip past "value":

		l = s.find("[", opos);

		size_t r = ro;
		std::vector<std::string> vs;
		while (std::string::npos != l)
		{
			l++;
			std::stringstream ss;
			ss << s.substr(l);
			std::string uq;
			ss >> std::quoted(uq);
			vs.push_back(uq);

			// Step past the closing quote.
			r = l + uq.size() + 2;

			// Get to the next elt
			r = s.find_first_of(",]", r);
			if (std::string::npos == r) break;
			if (']' == s[r]) break;
			l = r;
		}

		r = s.find("}", r);
		ro = r;
		return createStringValue(std::move(vs));
	}

	if (nameserver().isA(t, LINK_VALUE))
	{
		l = tpos;
		size_t opos = s.find("\"value\":", l);
		if (std::string::npos == opos) return nullptr;
		opos += 8;  // skip past "value":

		l = s.find("[", opos);

		size_t r = ro;
		std::vector<ValuePtr> vv;
		while (std::string::npos != l)
		{
			l++;
			ValuePtr vp = decode_value(s, l, r);
			vv.push_back(vp);

			// Get to the next elt
			r = s.find_first_of(",]", r);
			if (std::string::npos == r) break;
			if (']' == s[r]) break;
			l = r;
		}

		r = s.find("}", r);
		ro = r;
		return createLinkValue(std::move(vs));
	}

	return nullptr;
}

/* ============================= END OF FILE ================= */
