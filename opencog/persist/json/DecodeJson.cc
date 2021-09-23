/*
 * DecodeJson.cc
 * Decode JSON decribing Atms and Values.
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
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Json.h"

using namespace opencog;

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
/// The string to decode is `s`, begining at location `l` and using `r`
/// as a hint for the end of the expression.
///
/// XXX FIXME. This is a quick hack. It will be confused be embedded
/// quotes n stuff. This does NOT use any external JSON decoder
/// libraries because those libs don't really make anything simpler,
/// and also we don't need most of the features that they offer, and
/// also I don't want more dependencies in the AtomSpace.
///
Handle Json::decode_atom(const std::string& s,
                         size_t l, size_t r)
{
printf("duuude entry=%ld %ld %s\n", l, r, s.substr(l, r-l).c_str());
	size_t tpos = s.find("\"type\":", l);
	if (std::string::npos == tpos) return Handle::UNDEFINED;

	Type t = NOTYPE;
	try {
		t = Json::decode_type(s, tpos);
	}
	catch(...) {
		return Handle::UNDEFINED;
	}

	if (nameserver().isA(t, NODE))
	{
		size_t npos = s.find("\"name\":", l);

		npos = s.find_first_not_of(" \n\t", npos);
		std::string name = Json::get_node_name(s, npos, r);
		return createNode(t, std::move(name));
	}
	return Handle::UNDEFINED;
}

/* ============================= END OF FILE ================= */
