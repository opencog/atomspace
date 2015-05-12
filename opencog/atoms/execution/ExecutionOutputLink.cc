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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/reduct/AssignLink.h>
#include <opencog/atoms/reduct/PlusLink.h>
#include <opencog/atoms/reduct/TimesLink.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include "ExecutionOutputLink.h"

using namespace opencog;

ExecutionOutputLink::ExecutionOutputLink(const HandleSeq& oset,
                                         TruthValuePtr tv,
                                         AttentionValuePtr av)
	: FunctionLink(EXECUTION_OUTPUT_LINK, oset, tv, av)
{
	if (2 != oset.size() or
	   GROUNDED_SCHEMA_NODE != oset[0]->getType() or
	   LIST_LINK != oset[1]->getType())
	{
		throw RuntimeException(TRACE_INFO,
			"ExecutionOutputLink must have schema and args!");
	}
}

ExecutionOutputLink::ExecutionOutputLink(const Handle& schema,
                                         const Handle& args,
                                         TruthValuePtr tv,
                                         AttentionValuePtr av)
	: FunctionLink(EXECUTION_OUTPUT_LINK, schema, args, tv, av)
{
	if (GROUNDED_SCHEMA_NODE != schema->getType())
		throw RuntimeException(TRACE_INFO, "Expecting GroundedSchemaNode!");

	if (LIST_LINK != args->getType())
		throw RuntimeException(TRACE_INFO,
			"ExecutionOutputLink must have schema and args!");
}

ExecutionOutputLink::ExecutionOutputLink(Link& l)
	: FunctionLink(l)
{
	Type tscope = l.getType();
	if (EXECUTION_OUTPUT_LINK != tscope)
		throw RuntimeException(TRACE_INFO,
			"Expection an ExecutionOutputLink!");
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
Handle ExecutionOutputLink::execute(AtomSpace* as) const
{
	return do_execute(as, _outgoing[0], _outgoing[1]);
}

/// do_execute -- execute the GroundedSchemaNode of the ExecutionOutputLink
///
/// Expects "gsn" to be a GroundedSchemaNode
/// Expects "args" to be a ListLink
/// Executes the GroundedSchemaNode, supplying the args as argument
///
Handle ExecutionOutputLink::do_execute(AtomSpace* as,
                         const Handle& gsn, const Handle& cargs)
{
	// Search for additional execution links, and execute them too.
	// We will know that happend if the returned handle differs from
	// the input handle. If the results are different, add the new
	// results to the atomspace. We need to do this, because scheme,
	// and python expects to find thier arguments in the atomspace,
	// but this is arguably broken, as it pollutes the atomspace with
	// junk that is never cleaned up.  We punt for now, but something
	// should be done about this. XXX FIXME ...
	LinkPtr largs(LinkCast(cargs));
	Handle args(cargs);
	if (largs)
	{
		std::vector<Handle> new_oset;
		bool changed = false;
		for (Handle ho : largs->getOutgoingSet())
		{
			Handle nh(ho);
			FunctionLinkPtr flp(FunctionLinkCast(ho));
			if (flp)
				nh = flp->execute(as);
			new_oset.push_back(nh);
			if (nh != ho) changed = true;
		}
		if (changed)
			args = as->addLink(LIST_LINK, new_oset);
	}

	// Get the schema name.
	const std::string& schema = NodeCast(gsn)->getName();
	// printf ("Grounded schema name: %s\n", schema.c_str());

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0,4,"scm:", 4))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;

		SchemeEval* applier = SchemeEval::get_evaluator(as);
		Handle h(applier->apply(schema.substr(pos), args));

		// Exceptions were already caught, before leaving guile mode,
		// so we can't rethrow.  Just throw a new exception.
		if (applier->eval_error())
			throw RuntimeException(TRACE_INFO,
			    "Failed evaluation; see logfile for stack trace.");
		return h;
#else
		throw RuntimeException(TRACE_INFO,
		    "Cannot evaluate scheme GroundedSchemaNode!");
#endif /* HAVE_GUILE */
	}

	if (0 == schema.compare(0, 3,"py:", 3))
	{
#ifdef HAVE_CYTHON
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 3;
		while (' ' == schema[pos]) pos++;

		// Get a reference to the python evaluator. NOTE: We are
		// passing in a reference to our atom space to invoke
		// the safety checking to make sure the singleton instance
		// is using the same atom space.
		PythonEval &applier = PythonEval::instance(as);

		Handle h = applier.apply(schema.substr(pos), args);

		// Return the handle
		return h;
#else
		throw RuntimeException(TRACE_INFO,
		    "Cannot evaluate python GroundedSchemaNode!");
#endif /* HAVE_CYTHON */
	}

	// Unkown proceedure type.
	throw RuntimeException(TRACE_INFO,
	    "Cannot evaluate unknown GroundedSchemaNode!");
}
