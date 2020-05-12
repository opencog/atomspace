#ifndef FAST_LOAD_CPP
#define FAST_LOAD_CPP
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

bool check = false;
void get_next_expr(const std::string& s, int& l, int& r)
{
    int l1 = l;
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
        if(s[l1] == '"') par = !par;
        if(par) continue;
        if(s[l1] == '(') count++;
        if(s[l1] == ')') count--;
    } while(l1 <= r && count > 0);
    if(count != 0) {
        throw std::runtime_error("Invalid syntax #2 in " + s);
    }
    r = l1 - 1;
}

void get_next_token(const std::string& s, int& l, int& r)
{
    for(; l < r && (s[l] == ' ' || s[l] == '\t' || s[l] == '\n'); l++);
    if(s[l] == '"') {
        l++;
        int l1 = l;
        for(; l1 < r && s[l1] != '"'; l1++);
        r = l1-1;
    } else {
        int l1 = l;
        for(; l < r && s[l1] != ' ' && s[l1] != '\t' && s[l1] != '\n'; l1++);
        r = l1-1;
    }
}

// Handle, Atomspace
Handle recursive_parse(const std::string& s, AtomSpace& as)
{
    NameServer & nameserver = opencog::nameserver();

    int l = 0, r = s.length()-1;
    //get_next_expr(s, l, r);
    if(check)
        std::cout << "Parsing: " << s.substr(l, r-l+1) << std::endl;
    int l1 = l, r1 = r;
    get_next_token(s, l1, r1);
    const std::string stype = s.substr(l1, r1-l1+1);
    if(check)
        std::cout << "Type: |" << stype << "|\n";
    l = r1 + 1;
    if(stype.find("Link") != std::string::npos) {
        opencog::Type atype = nameserver.getType(stype);
        if (atype == opencog::NOTYPE) {
           throw std::runtime_error("Unknown link type " + stype);
        }
        HandleSeq outgoing;
        do {
            l1 = l;
            r1 = r;
            get_next_expr(s, l1, r1);
            if(check)
                std::cout << "Next Expr: |" << s.substr(l1, r1-l1+1) << "|\n";
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
    } else if(stype.find("Node") != std::string::npos) {
        opencog::Type atype = nameserver.getType(stype);
        if (atype == opencog::NOTYPE) {
           throw std::runtime_error("Unknown node type " + stype);
        }
        l1 = l, r1 = r;
        get_next_token(s, l1, r1);
        std::string token = s.substr(l1, r1-l1+1);
        if(check)
            std::cout << "Next Token: |" << token << "|\n";
        if(l1 >= r1) {
            throw std::runtime_error("Syntax error in " + s.substr(l, r-l+1));
        }
        l1 = r1 + 1, r1 = r;
        get_next_token(s, l1, r1);
        if(l1 < r1) {
            token = s.substr(l, r1-l1+1);
            throw std::runtime_error("Unexpexted token " + token + " in " + s);
        }
        return as.xadd_node(atype, token);
    }
    throw std::runtime_error("Syntax error in type " + stype + " in " + s);
    return Handle::UNDEFINED;
}

void opencog::load_file(std::string fname, AtomSpace& as)
{
    std::ifstream f(fname);
    int cnt = 0;
    while(!f.eof()) {
        std::string line, expr;
        int count = 0, l = 0, r = -1, shift = 0;
        bool par = false;
        check = false;
        do {
            getline(f, line);
            line += " ";
            expr += line;
            for(uint i = 0; i < line.size(); i++) {
                if(line[i] == '"') par = !par;
                if(par) {
                    //if(line[i] == '(') check = true;
                    continue;
                }
                if(line[i] == '(') {
                    if(count == 0) l = shift+i+1;
                    count++;
                }
                if(line[i] == ')') {
                    count--;
                    if(count == 0) r = shift+i-1;
                }
            }
            shift += line.size();
            if(expr.size() > 1000) {
                std::cout << expr << "\n\n";
                exit(-1);
            }
        } while(r == -1 && !f.eof());
        if(check) {
            std::cout << "Initial Expression: " << expr << "\n\n";
        }
        cnt++;
        if(r != -1) {
            recursive_parse(expr.substr(l, r-l+1), as);
        }
        if(check) { getchar(); }
    }
    f.close();
}



#endif // FAST_LOAD_CPP
