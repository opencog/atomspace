/*
 * opencog/atoms/execution/ExecutionOutputLink.cc
 *
 * Copyright (C) 2009, 2013, 2015 Linas Vepstas
 * All Rights Reserved
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

#include <stdlib.h>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include "DLScheme.h"
#include "ExecutionOutputLink.h"
#include "Force.h"
#include "LibraryManager.h"

using namespace opencog;

void ExecutionOutputLink::check_schema(const Handle& schema) const
{
	// Derived types do thier own validation.
	if (EXECUTION_OUTPUT_LINK != get_type()) return;

	if (not nameserver().isA(schema->get_type(), SCHEMA_NODE) and
	    LAMBDA_LINK != schema->get_type() and
	    // In case it is a pattern matcher query
	    VARIABLE_NODE != schema->get_type() and
	    UNQUOTE_LINK != schema->get_type())
	{
		throw SyntaxException(TRACE_INFO,
		                      "ExecutionOutputLink must have schema! Got %s",
		                      schema->to_string().c_str());
	}
}

ExecutionOutputLink::ExecutionOutputLink(const HandleSeq& oset, Type t)
	: FunctionLink(oset, t)
{
	if (!nameserver().isA(t, EXECUTION_OUTPUT_LINK))
		throw SyntaxException(TRACE_INFO,
		                      "Expection an ExecutionOutputLink!");

	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		       "ExecutionOutputLink must have schema and args! Got arity=%d",
		        oset.size());

	check_schema(oset[0]);
}

ExecutionOutputLink::ExecutionOutputLink(const Handle& schema,
                                         const Handle& args)
	: FunctionLink({schema, args}, EXECUTION_OUTPUT_LINK)
{
	check_schema(schema);
}

ExecutionOutputLink::ExecutionOutputLink(const Link& l)
	: FunctionLink(l)
{
	Type tscope = l.get_type();
	if (EXECUTION_OUTPUT_LINK != tscope)
		throw SyntaxException(TRACE_INFO,
		                      "Expection an ExecutionOutputLink!");

	check_schema(l.getOutgoingAtom(0));
}

/// execute -- execute the function defined in an ExecutionOutputLink
///
/// Each ExecutionOutputLink should have the form:
///
///     ExecutionOutputLink
///         GroundedSchemaNode "lang: func_name"
///         ListLink
///             SomeAtom
///             OtherAtom
///
/// The "lang:" should be either "scm:" for scheme, or "py:" for python.
/// This method will then invoke "func_name" on the provided ListLink
/// of arguments to the function.
///
ValuePtr ExecutionOutputLink::execute(AtomSpace* as, bool silent)
{
	if (_outgoing[0]->get_type() != GROUNDED_SCHEMA_NODE) {
		LAZY_LOG_FINE << "Not a grounded schema. Do not execute it";
		return get_handle();
	}

	return do_execute(as, _outgoing[0], _outgoing[1], silent);
}

/// do_execute -- execute the SchemaNode of the ExecutionOutputLink
///
/// Expects "gsn" to be a GroundedSchemaNode or a DefinedSchemaNode
/// Expects "cargs" to be a ListLink unless there is only one argument
/// Executes the GroundedSchemaNode, supplying cargs as arguments
///
ValuePtr ExecutionOutputLink::do_execute(AtomSpace* as,
                                         const Handle& gsn,
                                         const Handle& cargs,
                                         bool silent)
{
	LAZY_LOG_FINE << "Execute gsn: " << gsn->to_short_string()
	              << "with arguments: " << cargs->to_short_string();

	// Force execution of the arguments. We have to do this, because
	// the user-defined functions are black-boxes, and cannot be trusted
	// to do lazy execution correctly. Right now, forcing is the policy.
	// We could add "scm-lazy:" and "py-lazy:" URI's for user-defined
	// functions smart enough to do lazy evaluation.
	Handle args = force_execute(as, cargs, silent);

	// Get the schema name.
	const std::string& schema = gsn->get_name();

	// Extract the language, library and function
	std::string lang, lib, fun;
	LibraryManager::lang_lib_fun(schema, lang, lib, fun);

	ValuePtr result;

	// At this point, we only run scheme, python schemas and functions from
	// libraries loaded at runtime.
	if (lang == "scm")
	{
		SchemeEval* applier = get_evaluator_for_scheme(as);
		result = applier->apply_v(fun, args);

		// Exceptions were already caught, before leaving guile mode,
		// so we can't rethrow.  Just throw a new exception.
		if (applier->eval_error())
			throw RuntimeException(TRACE_INFO,
			         "Failed evaluation; see logfile for stack trace.");
	}
	else if (lang == "py")
	{
#ifdef HAVE_CYTHON
		// Get a reference to the python evaluator. 
		// Be sure to specify the atomspace in which the
		// evaluation is to be performed.
		PythonEval &applier = PythonEval::instance();
		result = applier.apply(as, fun, args);
#else
		throw RuntimeException(TRACE_INFO,
		                       "Cannot evaluate python GroundedSchemaNode!");
#endif /* HAVE_CYTHON */
	}
	// Used by the Haskel and C++ bindings; can be used with any language
	else if (lang == "lib")
	{
		void* sym = LibraryManager::getFunc(lib,fun);

		// Convert the void* pointer to the correct function type.
		Handle* (*func)(AtomSpace*, Handle*);
		func = reinterpret_cast<Handle* (*)(AtomSpace *, Handle*)>(sym);

		// Execute the function
		Handle* res = func(as, &args);
		if(res != NULL)
		{
			result = *res;
			free(res);
		}
	}
	else
	{
		// Unkown proceedure type
		throw RuntimeException(TRACE_INFO,
		                       "Cannot evaluate unknown Schema %s",
		                       gsn->to_string().c_str());
	}

	// Check for a not-uncommon user-error.  If the user-defined
	// code returns nothing, then a null-pointer-dereference is
	// likely, a bit later down the line, leading to a crash.
	// So head this off at the pass.
	if (nullptr == result)
	{
		// If silent is true, return a simpler and non-logged
		// exception, which may, in some contexts, will be
		// considerably faster than a RuntimeException.
		if (silent)
			throw NotEvaluatableException();

		throw RuntimeException(TRACE_INFO,
		                       "Invalid return value from schema %s\nArgs: %s",
		                       gsn->to_string().c_str(),
		                       cargs->to_string().c_str());
	}

	LAZY_LOG_FINE << "Result: " << result->to_string();
	return result;
}

DEFINE_LINK_FACTORY(ExecutionOutputLink, EXECUTION_OUTPUT_LINK)
