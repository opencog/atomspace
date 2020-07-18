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

#include <stdexcept>
#include <string>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

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
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;
	if (l == r) return 0;

	// Ignore comment lines.
	if (s[l] == ';') { r = l; return 1; }

	if (s[l] != '(')
		throw std::runtime_error(
			"Syntax error at line " + std::to_string(line_cnt) +
			" Unexpected text: >>" + s.substr(l) + "<<");

	size_t p = l;
	int count = 1;
	bool quoted = false;
	do {
		p++;
		if (s[p] == '"')
		{
			if (0 < p and s[p - 1] != '\\')
				quoted = !quoted;
		}
		else if (quoted) continue;
		else if (s[p] == '(') count++;
		else if (s[p] == ')') count--;
		else if (s[p] == ';') break;      // comments!
	} while (p < r and count > 0);

	r = p;
	return count;
}

/// Extracts link or node type. Given the string `s`, this updates
/// the `l` and `r` values such that `l` points at the first
/// non-whitespace character of the name, and `r` points at the last.
static void get_typename(const std::string& s, size_t& l, size_t& r,
                         size_t line_cnt)
{
	// Advance past whitespace.
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;

	if (s[l] != '(')
		throw std::runtime_error(
			"Syntax error at line " + std::to_string(line_cnt) +
			" Unexpected content: >>" + s.substr(l, r-l+1) + "<< in " + s);

	// Advance until whitespace. Might be fast to use strtok?
	l++;
	size_t p = l;
	for (; l < r and s[p] != '(' and s[p] != ' ' and s[p] != '\t' and s[p] != '\n'; p++);
	r = p;
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
static void get_node_name(const std::string& s, size_t& l, size_t& r,
                          size_t line_cnt, bool typeNode = false)
{
	// Advance past whitespace.
	while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;

	// Scheme strings start and end with double-quote.
	// Scheme symbols start with single-quote.
	if (typeNode and s[l] != '\'')
		throw std::runtime_error(
				"Syntax error at line " + std::to_string(line_cnt) +
				" Unexpected type name: >>" + s.substr(l, r-l+1) + "<< in " + s);
	else if (not typeNode and s[l] != '"')
		throw std::runtime_error(
			"Syntax error at line " + std::to_string(line_cnt) +
			" Unexpected content: >>" + s.substr(l, r-l+1) + "<< in " + s);

	l++;
	size_t p = l;
	if (typeNode)
		for (; p < r and (s[p] != '(' or s[p] != ' ' or s[p] != '\t' or s[p] != '\n'); p++);
	else
		for (; p < r and (s[p] != '"' or ((0 < p) and (s[p - 1] == '\\'))); p++);
	r = p;
}

/// Extract SimpleTruthValue and return that, else throw an error.
static TruthValuePtr get_stv(const std::string& s,
                             size_t l, size_t r, size_t line_cnt)
{
	if (s.compare(l, 5, "(stv "))
		throw std::runtime_error(
				"Syntax error at line " + std::to_string(line_cnt) +
				" Unsupported markup: " + s.substr(l, r-l+1) +
				" in expr: " + s);

	return createSimpleTruthValue(
				NumberNode::to_vector(s.substr(l+4, r-l-4)));
}

static NameServer& namer = nameserver();

/// Convert an Atomese S-expression into a C++ Atom.
/// For example: `(Concept "foobar")`  or
/// `(Evaluation (Predicate "blort") (List (Concept "foo") (Concept "bar")))`
/// will return the corresponding atoms.
///
/// The string to decode is `s`, begining at location `l` and using `r`
/// as a hint for the end of the expression. The `line_count` is an
/// optional argument for printing file line-numbers, in case of error.
///
Handle Sexpr::decode_atom(const std::string& s,
                          size_t l, size_t r, size_t line_cnt)
{
	size_t l1 = l, r1 = r;
	get_typename(s, l1, r1, line_cnt);
	const std::string stype = s.substr(l1, r1-l1);

	opencog::Type atype = namer.getType(stype);
	if (atype == opencog::NOTYPE)
		throw std::runtime_error(
			"Syntax error at line " + std::to_string(line_cnt) +
			" Unknown Atom type: " + stype);

	l = r1;
	if (namer.isLink(atype))
	{
		TruthValuePtr tvp;
		HandleSeq outgoing;
		do {
			l1 = l;
			r1 = r;
			get_next_expr(s, l1, r1, line_cnt);
			if (l1 == r1) break;

			// Atom names never start with lower-case.
			if ('s' == s[l1+1])
				tvp = get_stv(s, l1, r1, line_cnt);
			else
				outgoing.push_back(decode_atom(s, l1, r1, line_cnt));

			l = r1 + 1;
		} while (l < r);

		Handle h(createLink(std::move(outgoing), atype));
		if (tvp) h->setTruthValue(tvp);
		return h;
	}
	else
	if (namer.isNode(atype))
	{
		l1 = l;
		r1 = r;
		size_t l2;
		if (namer.isA(atype, TYPE_NODE)) {
			get_node_name(s, l1, r1, line_cnt, true);
			l2 = r1;
		} else {
			get_node_name(s, l1, r1, line_cnt);
			l2 = r1 + 1;   // step past trailing quote.
		}

		const std::string name = s.substr(l1, r1-l1);
		Handle h(createNode(atype, std::move(name)));

		// There might be an stv in the content. Handle it.
		size_t r2 = r;
		get_next_expr(s, l2, r2, line_cnt);
		if (l2 < r2)
			h->setTruthValue(get_stv(s, l2, r2, line_cnt));

		return h;
	}
	throw std::runtime_error(
		"Syntax error at line " + std::to_string(line_cnt) +
		"Got a Value, not supported: " + s);
}
