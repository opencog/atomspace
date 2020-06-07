/*
 * opencog/atoms/execution/GroundedPredicateNode.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015, 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/execution/Force.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include <opencog/atoms/grounded/GroundedPredicateNode.h>
#include "DLScheme.h"
#include "LibraryManager.h"


using namespace opencog;

GroundedPredicateNode::GroundedPredicateNode(std::string s)
	: GroundedProcedureNode(GROUNDED_PREDICATE_NODE, std::move(s))
{
}

GroundedPredicateNode::GroundedPredicateNode(Type t, std::string s)
	: GroundedProcedureNode(t, std::move(s))
{
	if (not nameserver().isA(t, GROUNDED_PREDICATE_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GroundedProcedureNode, got %s", tname.c_str());
	}
}

// ----------------------------------------------------------

static void throwSyntaxException(bool silent, const char* message...)
{
   if (silent)
      throw NotEvaluatableException();
   va_list args;
   va_start(args, message);
   throw SyntaxException(TRACE_INFO, message, args);
   va_end(args);
}

// ----------------------------------------------------------

/// Extract a single floating-point double out of an atom, that,
/// when executed, should yeild a value containing a number.
/// Viz, either a NumberNode, or a FloatValue.
static double get_numeric_value(AtomSpace* as, bool silent,
                                Handle h)
{
	Type t = h->get_type();
	if (DEFINED_SCHEMA_NODE == t)
	{
		h = DefineLink::get_definition(h);
		t = h->get_type();
	}

	ValuePtr pap(h);
	if (h->is_executable())
	{
		pap = h->execute(as, silent);
		t = pap->get_type();

		// Pattern matching hack. The pattern matcher returns sets of
		// atoms; if that set contains a single number, then unwrap it.
		// See issue #1502 which proposes to eliminate this SetLink hack.
		if (SET_LINK == t)
		{
			h = HandleCast(pap);
			if (1 != h->get_arity())
				throw SyntaxException(TRACE_INFO,
					"Don't know how to unwrap this: %s",
					h->to_string().c_str());
			pap = h->getOutgoingAtom(0);
			t = pap->get_type();
		}
	}

	if (NUMBER_NODE == t)
	{
		NumberNodePtr n(NumberNodeCast(pap));
		return n->get_value();
	}

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		FloatValuePtr fv(FloatValueCast(pap));
		if (fv->value().empty())
			throw RuntimeException(TRACE_INFO, "FloatValue is empty!");
		return fv->value()[0];
	}

	throwSyntaxException(silent,
		"Don't know how to do arithmetic with this: %s",
		pap->to_string().c_str());

	return std::nan("");
}


/// Perform a GreaterThan check
static bool greater(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
			  "GreaterThankLink expects two arguments");

	double v0 = get_numeric_value(as, silent, oset[0]);
	double v1 = get_numeric_value(as, silent, oset[1]);

	return (v0 > v1);
}

static TruthValuePtr bool_to_tv(bool truf)
{
   if (truf) return TruthValue::TRUE_TV();
   return TruthValue::FALSE_TV();
}

// ----------------------------------------------------------

/// `execute()` -- evaluate a GroundedPredicateNode with arguments.
///
/// Expects "args" to be a ListLink. These arguments will be
///     substituted into the predicate.
///
/// The arguments are "eager-evaluated", because it is assumed that
/// the GPN is unaware of the concept of lazy evaluation, and can't
/// do it itself. The arguments are then inserted into the predicate,
/// and the predicate as a whole is then evaluated.
///
ValuePtr GroundedPredicateNode::execute(AtomSpace* as,
                                        const Handle& cargs,
                                        bool silent)
{
	// Force execution of the arguments. We have to do this, because
	// the user-defined functions are black-boxes, and cannot be trusted
	// to do lazy execution correctly. Right now, forcing is the policy.
	// We could add "scm-lazy:" and "py-lazy:" URI's for user-defined
	// functions smart enough to do lazy evaluation.
	Handle args(force_execute(as, cargs, silent));

	// Get the schema name.
	const std::string& schema = get_name();
	// printf ("Grounded schema name: %s\n", schema.c_str());

	// A very special-case C++ comparison.
	// This compares two NumberNodes, by their numeric value.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:greater"))
	{
		return CastToValue(bool_to_tv(greater(as, args, silent)));
	}

	// A very special-case C++ comparison.
	// This compares a set of atoms, verifying that they are all different.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:exclusive"))
	{
		Arity sz = args->get_arity();
		for (Arity i=0; i<sz-1; i++) {
			Handle h1(args->getOutgoingAtom(i));
			for (Arity j=i+1; j<sz; j++) {
				Handle h2(args->getOutgoingAtom(j));
				if (h1 == h2) return CastToValue(TruthValue::FALSE_TV());
			}
		}
		return CastToValue(TruthValue::TRUE_TV());
	}

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0, 4, "scm:", 4))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;

		SchemeEval* applier = get_evaluator_for_scheme(as);
		return CastToValue(applier->apply_tv(schema.substr(pos), args));
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have scheme support in it; "
			"Cannot evaluate scheme GroundedPredicateNode!");
#endif /* HAVE_GUILE */
	}

	if (0 == schema.compare(0, 3, "py:", 3))
	{
#ifdef HAVE_CYTHON
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 3;
		while (' ' == schema[pos]) pos++;

		// Be sure to specify the atomspace in which to work!
		PythonEval &applier = PythonEval::instance();
		return CastToValue(applier.apply_tv(as, schema.substr(pos), args));
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have python support in it; "
			"Cannot evaluate python GroundedPredicateNode!");
#endif /* HAVE_CYTHON */
	}

	// Generic shared-library foreign-function interface.
	// Currently used only by the Haskell bindings.
	//
	// Extract the language, library and function from schema
	std::string lang, lib, fun;
	LibraryManager::parse_schema(schema, lang, lib, fun);
	if (lang == "lib")
	{
		void* sym = LibraryManager::getFunc(lib,fun);

		// Convert the void* pointer to the correct function type.
		TruthValuePtr* (*func)(AtomSpace*, Handle*);
		func = reinterpret_cast<TruthValuePtr* (*)(AtomSpace *, Handle*)>(sym);

		// Evaluate the predicate
		TruthValuePtr* res = func(as, &args);
		TruthValuePtr result;
		if(res != NULL)
		{
			result = *res;
			free(res);
		}

		if (nullptr == result)
			throwSyntaxException(silent,
			        "Invalid return value from predicate %s\nArgs: %s",
			        to_short_string().c_str(),
			        oc_to_string(cargs).c_str());

		return CastToValue(result);
	}

	// Unkown proceedure type.
	throw RuntimeException(TRACE_INFO,
	     "Cannot evaluate unknown GroundedPredicateNode: %s",
	      schema.c_str());
}

DEFINE_NODE_FACTORY(GroundedPredicateNode, GROUNDED_PREDICATE_NODE)