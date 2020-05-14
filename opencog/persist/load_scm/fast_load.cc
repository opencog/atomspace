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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include "fast_load.h"

using namespace opencog;

#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <stdexcept>

// Extract s-expression. Given a string `s`, update the `l` and `r`
// values so that `l` points at the next open-parenthsis (left paren)
// and `r` points at the matching close-paren.
void get_next_expr(const std::string& s, uint& l, uint& r)
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
void get_next_token(const std::string& s, uint& l, uint& r)
{
    for(; l < r && (s[l] == ' ' || s[l] == '\t' || s[l] == '\n'); l++);

    if(s[l] == '"') {  // we are parsing string
        l++;
        uint l1 = l;
        for(; l1 < r && (s[l1] != '"' or ((0 < l1) and (s[l1 - 1] == '\\'))); l1++);
        r = l1-1;
    } else {  // Node type or something
        uint l1 = l;
        for(; l < r && s[l1] != '(' && s[l1] != ' ' && s[l1] != '\t' && s[l1] != '\n'; l1++);
        r = l1 - 1;
    }
}

// Handle, Atomspace
Handle recursive_parse(const std::string& s, AtomSpace& as)
{
    NameServer & nameserver = opencog::nameserver();

    uint l = 0, r = s.length() - 1;

    uint l1 = l, r1 = r;
    get_next_token(s, l1, r1);
    const std::string stype = s.substr(l1, r1-l1+1);

    l = r1 + 1;
    opencog::Type atype = nameserver.getType(stype);
    if (atype == opencog::NOTYPE) {
       throw std::runtime_error("Unknown link type " + stype);
    }
    if (nameserver.isLink(atype)){
        HandleSeq outgoing;
        do {
            l1 = l;
            r1 = r;
            get_next_expr(s, l1, r1);

            if(l1 < r1) {
                std::string expr = s.substr(l1, r1-l1+1);
                if(expr.find("stv") == std::string::npos) {
                    outgoing.push_back(recursive_parse(expr, as));
                } else {
                    //std::cout << "Need to parse " + expr << std::endl;
                }
            }
            l = r1 + 2;
        } while(l < r);
        return as.add_link(atype, std::move(outgoing));
    } else {
        l1 = l;
        r1 = r;
        get_next_token(s, l1, r1);
        std::string token = s.substr(l1, r1-l1+1);

        if(l1 >= r1) {
            throw std::runtime_error("Syntax error in " + s.substr(l, r-l+1));
        }
        l1 = r1 + 1;
        r1 = r;
        get_next_token(s, l1, r1);
        if(l1 < r1) {
            token = s.substr(l, r1 - l1 + 1);
            throw std::runtime_error("Unexpexted token " + token + " in " + s);
        }
        return as.add_node(atype, std::move(token));
    }
    throw std::runtime_error("Syntax error in type " + stype + " in " + s);
}

/// load_file -- load the given file into the given AtomSpace.
void opencog::load_file(std::string fname, AtomSpace& as)
{
    std::ifstream f(fname);
    int cnt = 0;
    while(!f.eof()) {
        std::string line, expr;
        uint count = 0, l = 0, shift = 0;
        int r = -1;
        bool par = false;
        char prev = ' ';
        do {
            getline(f, line);
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
            if(expr.size() > 10000) {
                std::cout << expr << "\n\n";
                exit(-1);
            }
        } while(r == -1 && !f.eof());
        cnt++;
        if(r != -1) {
            assert(0 <= r);
            recursive_parse(expr.substr(l, r - l + 1), as);
        }
    }
    f.close();
}
