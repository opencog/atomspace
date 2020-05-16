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
// and `r` points at the matching close-paren.
static void get_next_expr(const std::string& s, uint& l, uint& r)
{
    uint l1 = l;
    while(s[l1] != '(' && l1 < r) {
        if(s[l1] != ' ' && s[l1] != '\t' && s[l1] != '\n') {
            throw std::runtime_error("Invalid syntax #1 in " + s.substr(l,r-l+1) + " at |" + s[l1] + "|");
        }
        l1++;
    }
    if(l1 >= r) {
        l = l1;
        return;
    }
    l = l1 + 1;
    int count = 1;
    bool par = false;
    do {
        l1++;
        if(s[l1] == '"')
            if (0 < l1 and s[l1 - 1] != '\\'){
                par = !par;
            }
        if(par) continue;
        if(s[l1] == '(') count++;
        if(s[l1] == ')') count--;
    } while(l1 <= r && count > 0);
    if(count != 0) {
        throw std::runtime_error("Invalid syntax #2 in " + s);
    }
    r = l1 - 1;
}

// Tokenizer - extracts link or node type or name. Given the string `s`,
// this updates the `l` and `r` values such that `l` points at the first
// non-whitespace character of the name, and `r` points at the last.
// The string is considered to start *after* the first quote, and ends
// just before the last quote. In this case, escaped quotes \" are
// ignored (are considered to be part of the string).
static std::string get_next_token(const std::string& s, uint& l, uint& r)
{
    std::string token;
    for(; l < r && (s[l] == ' ' || s[l] == '\t' || s[l] == '\n'); l++);

    if(s[l] == '"') {  // we are parsing string
        l++;
        uint l1 = l;
        for(; l1 < r && (s[l1] != '"' or ((0 < l1) and (s[l1 - 1] == '\\'))); l1++)
        {
            token.push_back(s[l1]);
        }
        r = l1-1;
    } else {  // Node type or something
        uint l1 = l;
        for(; l < r && s[l1] != '(' && s[l1] != ' ' && s[l1] != '\t' && s[l1] != '\n'; l1++)
            token.push_back(s[l1]);
        r = l1 - 1;
    }
    return token;
}

// Parse the string `s`, returning a Handle that corresponds to that
// string. The line_cnt is the current location in the file, for
// files that have bugs in them.
static Handle recursive_parse(const std::string& s, int line_cnt)
{
    NameServer & nameserver = opencog::nameserver();

    uint l = 0, r = s.length() - 1;

    uint l1 = l, r1 = r;
    const std::string stype = get_next_token(s, l1, r1);

    l = r1 + 1;
    opencog::Type atype = nameserver.getType(stype);
    if (atype == opencog::NOTYPE) {
       throw std::runtime_error(
           "Syntax error at line " + std::to_string(line_cnt) +
           " Unknown link type: " + stype);
    }
    if (nameserver.isLink(atype)){
        HandleSeq outgoing;
        do {
            l1 = l;
            r1 = r;
            get_next_expr(s, l1, r1);

            if(l1 < r1) {
                std::string expr = s.substr(l1, r1-l1+1);

                // FIXME -- support not only stv (SimpleTruthValues)
                // but also other Values.
                if(expr.find("stv") == std::string::npos) {
                    outgoing.push_back(recursive_parse(expr, line_cnt));
                } else {
                    //std::cout << "Need to parse " + expr << std::endl;
                }
            }
            l = r1 + 2;
        } while(l < r);
        return createLink(std::move(outgoing), atype);
    } else {
        l1 = l;
        r1 = r;
        std::string token = get_next_token(s, l1, r1);

        if(l1 >= r1) {
            throw std::runtime_error(
                "Syntax error at line " + std::to_string(line_cnt) +
                " Bad expr: " + s.substr(l, r-l+1));
        }
        l1 = r1 + 1;
        r1 = r;
        get_next_token(s, l1, r1);
        if(l1 < r1) {
            token = s.substr(l, r1 - l1 + 1);
            throw std::runtime_error(
                "Unexpexted token at line " + std::to_string(line_cnt) +
                " Token: " + token + " in expr: " + s);
        }
        return createNode(atype, std::move(token));
    }
    throw std::runtime_error(
       "Syntax error at line " + std::to_string(line_cnt) +
       " Strange type: " + stype + " in " + s);
}

/// load_file -- load the given file into the given AtomSpace.
void opencog::load_file(std::string fname, AtomSpace& as)
{
    std::ifstream f(fname);
    if (not f.is_open())
       throw std::runtime_error("Cannot find file >>" + fname + "<<");

    int expr_cnt = 0;
    int line_cnt = 0;
    while(!f.eof()) {
        std::string line, expr;
        uint count = 0, l = 0, shift = 0;
        int r = -1;
        bool par = false;
        char prev = ' ';
        do {
            std::getline(f, line);
            line_cnt++;
            line += " ";
            expr += line;
            for(uint i = 0; i < line.size(); i++) {
                if(line[i] == '"') {
                    if (prev != '\\'){
                        par = !par;
                    }
                }
                if(par) {
                    prev = line[i];
                    continue;
                }
                assert(not par);
                if(line[i] == '(') {
                    if(count == 0)
                        l = shift + i + 1;
                    count++;
                } else if(line[i] == ')') {
                    count--;
                    assert(0 <= count);
                    if(count == 0)
                        r = shift + i - 1;
                }
            }
            shift += line.size();
        } while(r == -1 && !f.eof());

        if(r != -1) {
            expr_cnt++;
            assert(0 <= r);
            as.add_atom(recursive_parse(expr.substr(l, r - l + 1), line_cnt));
        }
    }
    f.close();
}
