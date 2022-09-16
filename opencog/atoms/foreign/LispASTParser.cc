/*
 * LispASTParser.cc
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
// printf("Enter get_next %lu %lu >>%s<<\n", l, r, sexpr.substr(l).c_str());
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

/* ===================== END OF FILE ===================== */
