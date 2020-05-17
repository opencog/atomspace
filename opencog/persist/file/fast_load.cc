/*
 * fast_load.cc
 * fast load of Atomese in s-expression format.
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

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

#include "fast_load.h"

using namespace opencog;

// Extract s-expression. Given a string `s`, update the `l` and `r`
// values so that `l` points at the next open-parenthsis (left paren)
// and `r` points at the matching close-paren.  Returns parenthesis
// count. If zero, the parens match. If non-zero, then `r` points
// at the first non-valid character in the string (e.g. comment char).
static int get_next_expr(const std::string& s, size_t& l, size_t& r)
{
    // Advance past whitespace.
    while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;
    if (l == r) return 0;

    // Ignore comment lines.
    if (s[l] == ';') { r = l; return 1; }

    if (s[l] != '(')
        throw std::runtime_error("Unexpected text: >>" + s + "<<");

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

// Tokenizer - extracts link or node type or name. Given the string `s`,
// this updates the `l` and `r` values such that `l` points at the first
// non-whitespace character of the name, and `r` points at the last.
// The string is considered to start *after* the first quote, and ends
// just before the last quote. In this case, escaped quotes \" are
// ignored (are considered to be part of the string).
static void get_next_token(const std::string& s, size_t& l, size_t& r)
{
    // Advance past whitespace.
    while (l < r and (s[l] == ' ' or s[l] == '\t' or s[l] == '\n')) l++;

    // We are parsing string
    if (s[l] == '"')
    {
        l++;
        size_t p = l;
        for (; p < r and (s[p] != '"' or ((0 < p) and (s[p - 1] == '\\'))); p++);
        r = p-1;
        return;
    }

    if (s[l] == '(')
    {
        // Atom type name. Advance until whitespace.
        // Faster to use strtok!?
        l++;
        size_t p = l;
        for (; l < r and s[p] != '(' and s[p] != ' ' and s[p] != '\t' and s[p] != '\n'; p++);
        r = p - 1;
        return;
    }
    throw std::runtime_error(
        "Wasn't a token: >>" + s.substr(l, r-l+1) + "<< in " + s);
}

static NameServer& namer = nameserver();

// Parse the string `s`, returning a Handle that corresponds to that
// string.
static Handle recursive_parse(const std::string& s,
                              size_t l, size_t r, size_t line_cnt)
{
    size_t l1 = l, r1 = r;
    get_next_token(s, l1, r1);
    const std::string stype = s.substr(l1, r1-l1+1);

    opencog::Type atype = namer.getType(stype);
    if (atype == opencog::NOTYPE)
        throw std::runtime_error(
            "Syntax error at line " + std::to_string(line_cnt) +
            " Unknown Atom type: " + stype);

    l = r1 + 1;
    if (namer.isLink(atype))
    {
        r--; // get rid of trailing paren
        HandleSeq outgoing;
        do {
            l1 = l;
            r1 = r;
            get_next_expr(s, l1, r1);

            if (l1 == r1)
                throw std::runtime_error(
                    "Syntax error at line " + std::to_string(line_cnt) +
                    " Expecting an Atom");

            outgoing.push_back(recursive_parse(s, l1, r1, line_cnt));

            l = r1 + 1;
        } while (l < r);

        return createLink(std::move(outgoing), atype);
    }
    else
    if (namer.isNode(atype))
    {
        l1 = l;
        r1 = r;
        get_next_token(s, l1, r1);
        if (l1 > r1)
            throw std::runtime_error(
                "Syntax error at line " + std::to_string(line_cnt) +
                " Can't find Atom name");

        size_t l2 = r1 + 1;
        size_t r2 = r;
        get_next_token(s, l2, r2);
        if (l2 < r2)
            throw std::runtime_error(
                "Syntax error at line " + std::to_string(line_cnt) +
                " Unexpected content: " + s.substr(l2, r2-l2) +
                " in expr: " + s);

        const std::string name = s.substr(l1, r1-l1+1);
        return createNode(atype, std::move(name));
    }
    throw std::runtime_error(
        "Syntax error at line " + std::to_string(line_cnt) +
        "Got a Value, not supported: " + s);
}

/// load_file -- load the given file into the given AtomSpace.
void opencog::load_file(std::string fname, AtomSpace& as)
{
    std::ifstream f(fname);
    if (not f.is_open())
       throw std::runtime_error("Cannot find file >>" + fname + "<<");

    size_t expr_cnt = 0;
    size_t line_cnt = 0;

    std::string expr;
    while (!f.eof())
    {
        std::string line;
        std::getline(f, line);
        line_cnt++;
        expr += line;
        while (true)
        {
            size_t l = 0;
            size_t r = expr.length();

            // Zippy the Pinhead says: Are we having fun yet?
            int pcount = get_next_expr(expr, l, r);

            // Trim away comments at end of line
            if (0 < pcount)
            {
                expr = expr.substr(l, r-l);
                break;
            }

            // Nothing to do.
            if (l == r) break;

            expr_cnt++;
            r++;
            as.add_atom(recursive_parse(expr, l, r, line_cnt));
            expr = expr.substr(r);
        }
    }
    f.close();
}
