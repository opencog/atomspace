/*
 * LispAST.cc
 *
 * Copyright (C) 2021, 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  September 2022
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/reduct/DivideLink.h>
#include <opencog/atoms/reduct/MinusLink.h>
#include <opencog/atoms/reduct/PlusLink.h>
#include <opencog/atoms/reduct/TimesLink.h>
#include "LispAST.h"

using namespace opencog;

void LispAST::init()
{
	if (not nameserver().isA(_type, LISP_AST))
	{
		const std::string& tname = nameserver().getTypeName(_type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an LispAST, got %s", tname.c_str());
	}
}

LispAST::LispAST(const HandleSeq&& oset, Type t)
	: SexprAST(std::move(oset), t)
{
	init();
}

LispAST::LispAST(const std::string& sexpr)
	: SexprAST(LISP_AST)
{
	parse(sexpr);
}

// ---------------------------------------------------------------

/// Parse assorted built-in operators
Handle make_atom(const std::string& fexp, const HandleSeq&& args)
{
	if (fexp == "+")
		return HandleCast(createPlusLink(std::move(args)));

	if (fexp == "-")
		return HandleCast(createMinusLink(std::move(args)));

	if (fexp == "*")
		return HandleCast(createTimesLink(std::move(args)));

	if (fexp == "/")
		return HandleCast(createDivideLink(std::move(args)));

	if (fexp == ">")
		return createLink(std::move(args), GREATER_THAN_LINK);

	if (fexp == "<")
		return createLink(std::move(args), LESS_THAN_LINK);

	if (fexp == "if")
		return createLink(std::move(args), COND_LINK);

	// Else assume that this is some kind of function defintion.
	return createLink(EXECUTION_OUTPUT_LINK,
		createNode(DEFINED_SCHEMA_NODE, fexp),
		createLink(std::move(args), LIST_LINK));
}

Handle make_tok(const std::string& tok)
{
	if ('$' == tok[0])
		return createNode(VARIABLE_NODE, tok);

	if (isdigit(tok[0]))
		return HandleCast(createNumberNode(std::move(tok)));

	// Assume that anything else will be a DefinedSchemaNode
	return createNode(DEFINED_SCHEMA_NODE, tok);
}

// Extract a single number or string
Handle get_tok(const std::string& sexpr, size_t& l, size_t &r)
{
	l = sexpr.find_first_not_of(" \t\n", l);
	if ('(' == sexpr[l])
		throw SyntaxException(TRACE_INFO, "Expected literal");

	// Get the token name
	r = sexpr.find_first_of(" \t\n)", l);
	const std::string& tok = sexpr.substr(l, r-l);
	l = sexpr.find_first_not_of(" \t\n", r);

	return make_tok(tok);
}

// ---------------------------------------------------------------

/// Decode an s-expression, converting it to Atomese using
/// Lisp-like MeTTa rules.
Handle get_next(const std::string& sexpr, size_t& l, size_t &r)
{
printf("duuude hello world %lu %lu >>%s<<\n", l, r, sexpr.substr(l).c_str());
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Start of fexpr.
	if ('(' == sexpr[l])
	{
		l++; // step past open-paren
		l = sexpr.find_first_not_of(" \t\n", l);
		if ('(' == sexpr[l])
			throw SyntaxException(TRACE_INFO, "Expected literal");

		// Get the first token
		r = sexpr.find_first_of(" \t\n)", l);
		const std::string& tok = sexpr.substr(l, r-l);
		l = sexpr.find_first_not_of(" \t\n", r);
		if (')' == sexpr[l])
			r = std::string::npos;

		HandleSeq oset;
		while (std::string::npos != r)
		{
			Handle h(get_next(sexpr, l, r));
			oset.emplace_back(h);
		}

		// l will be pointing at the trailing paren, so move past that.
		l++;
		l = sexpr.find_first_not_of(" \t\n", l);
		r = 0;
		if (std::string::npos == l or ')' == sexpr[l])
			r = std::string::npos;
		return make_atom(tok, std::move(oset));
	}

	// If its a literal, we are done.
	r = sexpr.find_first_of(" \t\n)", l);
	if (std::string::npos == r)
		throw SyntaxException(TRACE_INFO, "Failed to find closing parenthesis");

	// Found the closing paren; just wrap the string.
	if (')' == sexpr[r])
	{
		Handle htok = get_tok(sexpr, l, r);
		r = std::string::npos;
		return htok;
	}

	// If we are here, r points to whitespace, and l points to the first
	// thing after the initial opening paren.
	while ('(' == sexpr[r-1]) r--;
	Handle htok = get_tok(sexpr, l, r);
	if (')' == sexpr[l])
		r = std::string::npos;

	return htok;
}

// ---------------------------------------------------------------

/// Parse named lambda expressions. Currently, only supports
/// (= x y) expressions, where y is taken to be the definition of x.
/// Here, x is assumed to be a function signature.
Handle define_lambda(const std::string& sexpr, size_t& l, size_t &r)
{
	if ('=' != sexpr[l])
		throw SyntaxException(TRACE_INFO, "Not supported!");

	l++;
	l = sexpr.find_first_not_of(" \t\n", l);
	if (std::string::npos == l)
		throw SyntaxException(TRACE_INFO, "Unexpected blank line");

	// Perhaps the defintion is a simple variable name.
	// For example: `(= foo 6)`
	if ('(' != sexpr[l])
	{
		Handle name = get_tok(sexpr, l, r);
		Handle body = get_next(sexpr, l, r);
		Handle defn = createLink(DEFINE_LINK, name, body);
		return defn;
	}

	// Start of function signature; first, get the function name.
	// For example, `(fun $x $y $z)`
	l++; // step past open-paren
	l = sexpr.find_first_not_of(" \t\n", l);
	if ('(' == sexpr[l])
		throw SyntaxException(TRACE_INFO, "Expected function literal");

	// Get the function name
	r = sexpr.find_first_of(" \t\n)", l);
	const std::string& func_name = sexpr.substr(l, r-l);
	l = sexpr.find_first_not_of(" \t\n", r);
	if (')' == sexpr[l])
		r = std::string::npos;
	Handle fname = createNode(DEFINED_SCHEMA_NODE, func_name);

	// Get the list of variables following it.
	HandleSeq args;
	while (std::string::npos != r)
	{
		Handle h(get_next(sexpr, l, r));
		args.emplace_back(h);
	}
	Handle arglist = createLink(args, VARIABLE_LIST);

	// l will be pointing at the trailing paren, so move past that.
	l++;
	l = sexpr.find_first_not_of(" \t\n", l);
	r = 0;
	if (std::string::npos == l or ')' == sexpr[l])
		r = std::string::npos;

	// Now get the body.
	Handle body = get_next(sexpr, l, r);

	// Build the defintion
	Handle defun = 
		createLink(DEFINE_LINK,
			fname,
			createLink(LAMBDA_LINK, arglist, body));

	return defun;
}

/// Handle top-level expressions. Currently, only supports
/// the following forms:
/// (= x y) where x is a function signature and y is the body.
/// (fun arg) where fun is a previousy defined function, and arg the arg.
///     fun can include the arithmetic ops, etc.
Handle LispAST::next_expr(const std::string& sexpr, size_t& l, size_t &r)
{
	// Assume this is a defintion of some kind.
	if ('=' == sexpr[l])
		return define_lambda(sexpr, l, r);

	// Assume it is a function to be applied to args.
	l--; // Backup to the open-paren.
	return get_next(sexpr, l, r);
}

// ---------------------------------------------------------------

std::string LispAST::to_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
		return indent + "(LispAst \"" + _name + "\") ; " + id_to_string();

	std::string rv = indent + "(LispAst\n";
	for (const Handle& h: _outgoing)
		rv += h->to_string(indent + "  ") + "\n";

	rv += indent + ") ; " + id_to_string();
	return rv;
}

std::string LispAST::to_short_string(const std::string& indent) const
{
	if (0 == _outgoing.size())
	{
		if (0 != indent.size()) return _name;

		return _name + "\n" + to_string(";") + "\n";
	}

	std::string rv = "(";
	for (const Handle& h: _outgoing)
	{
		if (SEXPR_AST == h->get_type())
			rv += h->to_short_string("xx") + " ";
		else
			rv += "(atomese " + h->to_short_string("") + ") ";
	}

	rv[rv.size()-1] = ')';

	// Debugging print
	if (0 == indent.size()) rv += "\n" + to_string(";") + "\n";
	return rv;
}

// ---------------------------------------------------------------

// Content-based comparison.
bool LispAST::operator==(const Atom& other) const
{
	// If other points to this, then have equality.
	if (this == &other) return true;

	// Let Link do most of the work.
	bool linkeq = Link::operator==(other);
	if (not linkeq) return false;

	// Names must match.
	return 0 == _name.compare(LispASTCast(other.get_handle())->_name);
}

// ---------------------------------------------------------------
// Custom factory, because its a hermaphrodite. The ForgeinAST will
// pass us a string, behaving like a node, which we parse into an
// expression tree.

Handle LispAST::factory(const Handle& base)
{
	/* If it's castable, nothing to do. */
	if (LispASTCast(base)) return base;

	if (0 < base->get_arity())
		return HandleCast(createLispAST(std::move(base->getOutgoingSet())));

	return HandleCast(createLispAST(std::move(base->get_name())));
}

/* This runs when the shared lib is loaded. */
static __attribute__ ((constructor)) void init_lispast_factory(void)
{
	classserver().addFactory(LISP_AST, &LispAST::factory);
}

/* ===================== END OF FILE ===================== */
