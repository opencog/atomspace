/*
 * AtomSexpr.cc
 * Decode Atomese written in s-expression format.
 *
 * Copyright (C) 2020 Alexey Potapov, Anatoly Belikov
 *
 * Authors: Alexey Potapov
 *          Anatoly Belikov
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
#include <stdexcept>
#include <string>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Sexpr.h"

using namespace opencog;

/// Extract s-expression. Given a string `s`, update the `l` and `r`
/// values so that `l` points at the next open-parenthesis (left paren)
/// and `r` points at the matching close-paren.  Returns parenthesis
/// count. If zero, the parens match. If non-zero, then `r` points
/// at the first non-valid character in the string (e.g. comment char).
int Sexpr::get_next_expr(const std::string& s, size_t& l, size_t& r,
                         size_t line_cnt)
{
	// Advance past whitespace.
	// Would be more efficient to say l = s.find_first_not_of(" \t\n", l);
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;
	if (l == r) return 0;

	// Ignore comment lines.
	if (s[l] == ';') { l = r; return 1; }

	if (s[l] != '(')
		throw SyntaxException(TRACE_INFO,
			"Syntax error at line %lu Unexpected text: >>%s<<",
			line_cnt, s.substr(l).c_str());

	size_t p = l;
	int count = 1;
	bool quoted = false;
	do {
		p++;

		// Skip over any escapes
		if (s[p] == '\\') { p ++; continue; }

		if (s[p] == '"') quoted = !quoted;
		else if (quoted) continue;
		else if (s[p] == '(') count++;
		else if (s[p] == ')') count--;
		else if (s[p] == ';') break;      // comments!
	} while (p < r and count > 0);

	r = p;
	return count;
}

static NameServer& namer = nameserver();

/// Extracts link or node type. Given the string `s`, this updates
/// the `l` and `r` values such that `l` points at the first
/// non-whitespace character of the name, and `r` points at the last.
static Type get_typename(const std::string& s, size_t& l, size_t& r,
                         size_t line_cnt)
{
	// Advance past whitespace.
	l = s.find_first_not_of(" \t\n", l);

	if (s[l] != '(')
		throw SyntaxException(TRACE_INFO,
			"Error at line %lu unexpected content: >>%s<< in %s",
			line_cnt, s.substr(l, r-l+1).c_str(), s.c_str());

	// Advance until whitespace.
	l++;
	r = s.find_first_of("( \t\n", l);

	const std::string stype = s.substr(l, r-l);
	Type atype = namer.getType(stype);
	if (atype == opencog::NOTYPE)
		throw SyntaxException(TRACE_INFO,
			"Error at line %lu unknown Atom type: %s",
			line_cnt, stype.c_str());

	return atype;
}

/// Extracts Node name-string. Given the string `s`, this updates
/// the `l` and `r` values such that `l` points at the first
/// non-whitespace character of the name, and `r` points at the last.
/// The string is considered to start *after* the first quote, and ends
/// just before the last quote. In this case, escaped quotes \" are
/// ignored (are considered to be part of the string).
///
/// If the node is a Type node, then `l` points at the first
/// non-whitespace character of the type name and `r` points to the next
/// opening parenthesis.
///
/// This function was originally written to allow in-place extraction
/// of the node name. Unfortunately, node names containing escaped
/// quotes need to be unescaped, which prevents in-place extraction.
/// So, instead, this returns a copy of the name string.
std::string Sexpr::get_node_name(const std::string& s,
                                 size_t& l, size_t& r,
                                 Type atype, size_t line_cnt)
{
	// Advance past whitespace.
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;

	bool typeNode = namer.isA(atype, TYPE_NODE);

	// Scheme strings start and end with double-quote.
	// Scheme symbols start with single-quote.
	bool scm_symbol = false;
	if (typeNode and s[l] == '\'')
		scm_symbol = true;
	else if (not typeNode and s[l] != '"')
		throw SyntaxException(TRACE_INFO,
			"Syntax error at line %zu Unexpected content: >>%s<< in %s",
			line_cnt, s.substr(l, r-l+1).c_str(), s.c_str());

	l++;
	size_t p = l;
	if (scm_symbol)
		for (; p < r and (s[p] != '(' or s[p] != ' ' or s[p] != '\t' or s[p] != '\n'); p++);
	else
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

/// Extract SimpleTruthValue and return that, else throw an error.
static TruthValuePtr get_stv(const std::string& s,
                             size_t l, size_t r, size_t line_cnt)
{
	if (s.compare(l, 5, "(stv "))
		throw SyntaxException(TRACE_INFO,
			"Syntax error at line %zu Unexpected markup: >>%s<< in expr %s",
			line_cnt, s.substr(l, r-l+1).c_str(), s.c_str());

	return createSimpleTruthValue(
				NumberNode::to_vector(s.substr(l+4, r-l-4)));
}

/// Convert an Atomese S-expression into a C++ Atom.
/// For example: `(Concept "foobar")`  or
/// `(Evaluation (Predicate "blort") (List (Concept "foo") (Concept "bar")))`
/// will return the corresponding atoms.
///
/// The string to decode is `s`, beginning at location `l` and using `r`
/// as a hint for the end of the expression. The `line_count` is an
/// optional argument for printing file line-numbers, in case of error.
///
Handle Sexpr::decode_atom(const std::string& s,
                          size_t l, size_t r, size_t line_cnt,
                          std::unordered_map<std::string, Handle>& ascache)
{
	TruthValuePtr stv;
	size_t l1 = l, r1 = r;
	Type atype = get_typename(s, l1, r1, line_cnt);

	l = r1;
	if (namer.isLink(atype))
	{
		AtomSpace* as = nullptr;
		HandleSeq outgoing;
		do {
			l1 = l;
			r1 = r;
			get_next_expr(s, l1, r1, line_cnt);
			if (l1 == r1) break;

			// Atom names never start with lower-case.
			// We allow (stv nn nn) to occur in the middle of a long
			// sexpr, because apparently some users (agi-bio) do that.
			if (islower(s[l1+1]))
			{
				if ('s' == s[l1+1])  // 0 == s.compare(l1, 5, "(stv ")
					stv = get_stv(s, l1, r1, line_cnt);
				else
					break;
			}
			else
			{
				if (0 == s.compare(l1, 11, "(AtomSpace "))
				{
					Handle hasp(decode_frame(Handle::UNDEFINED, s, l1, ascache));
					as = (AtomSpace*) hasp.get();
				}
				else
					outgoing.push_back(decode_atom(s, l1, r1, line_cnt, ascache));
			}

			l = r1 + 1;
		} while (l < r);

		Handle h(createLink(std::move(outgoing), atype));
		if (as) h = as->add_atom(h);

		if (stv)
		{
			if (as)
				as->set_truthvalue(h, stv);
			else
				h->setTruthValue(stv);
		}

		// alist's occur at the end of the sexpr.
		if (l1 != r1 and l < r)
			decode_slist(h, s, l1);

		return h;
	}
	else
	if (namer.isNode(atype))
	{
		l1 = l;
		r1 = r;
		const std::string name = get_node_name(s, l1, r1, atype, line_cnt);

		Handle h(createNode(atype, std::move(name)));

		// There might be an stv in the content. Handle it.
		size_t l2 = r1;
		size_t r2 = r;
		get_next_expr(s, l2, r2, line_cnt);
		if (l2 < r2)
		{
			AtomSpace* as = nullptr;
			if (0 == s.compare(l2, 11, "(AtomSpace "))
			{
				Handle hasp(decode_frame(Handle::UNDEFINED, s, l2, ascache));
				as = (AtomSpace*) hasp.get();
				h = as->add_atom(h);
			}
			if (l2 < r2)
			{
				if (0 == s.compare(l2, 7, "(alist "))
					decode_slist(h, s, l2);
				else if (as)
					as->set_truthvalue(h, get_stv(s, l2, r2, line_cnt));
				else
					h->setTruthValue(get_stv(s, l2, r2, line_cnt));
			}
		}

		return h;
	}
	throw SyntaxException(TRACE_INFO,
		"Syntax error at line %zu Got a Value, not supported: %s",
		line_cnt, s.c_str());
}
