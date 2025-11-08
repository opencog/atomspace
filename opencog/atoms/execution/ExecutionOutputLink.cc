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
#include <opencog/atomspace/Transient.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/value/LinkValue.h>

#include "ExecutionOutputLink.h"
#include "GroundedProcedureNode.h"
#include "Instantiator.h"

using namespace opencog;

void ExecutionOutputLink::check_schema(const Handle& schema) const
{
	// Derived types do their own validation.
	if (EXECUTION_OUTPUT_LINK != get_type()) return;

	Type st = schema->get_type();
	if ((not schema->is_type(FUNCTION_LINK)) and
	    (not schema->is_type(VIRTUAL_LINK)) and
	    LAMBDA_LINK != st and
	    RULE_LINK != st and
	    (not schema->is_type(PROCEDURE_NODE)) and
	    // In case it is a pattern matcher query
	    VARIABLE_NODE != st and
	    UNQUOTE_LINK != st)
	{
		throw SyntaxException(TRACE_INFO,
		                      "ExecutionOutputLink must have schema! Got %s",
		                      schema->to_string().c_str());
	}
}

ExecutionOutputLink::ExecutionOutputLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (!nameserver().isA(t, EXECUTION_OUTPUT_LINK))
		throw SyntaxException(TRACE_INFO,
		                      "Not an ExecutionOutputLink!");

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
/// The "lang:" should be "scm:" for scheme, "py:" for python, or
/// "lib:" for an unix (gnu elf) shared library. See GroundedSchema
/// for details on supported formats.
///
/// This method will then invoke "func_name" on the provided ListLink
/// of arguments to the function.
///
ValuePtr ExecutionOutputLink::execute(AtomSpace* as, bool silent)
{
	Transient scratch(as);

	ValuePtr vp(execute_once(as, scratch.tmp, silent));

	// Should never happen. But it can happen if the API implementation
	// is screwed up. Since nothing should ever be screwed up, this can't
	// happen.
	if (not vp)
		throw SyntaxException(TRACE_INFO,
			"ExecutionOutputLink: Execution gave null result: %s",
				to_string().c_str());

	if (not vp->is_atom()) return vp;

	Handle res(scratch.tmp->add_atom(HandleCast(vp)));
	if (res->is_executable())
		vp = res->execute(scratch.tmp, silent);

	// Need to handle constructions such as
	// (ExecutionOutput
	//    (Lambda
	//        (VariableList (Variable "$A") (Variable "$B"))
	//        (LessThan (Variable "$A") (Variable "$B")))
	else if (res->is_type(EVALUATABLE_LINK))
	{
		Instantiator inst(scratch.tmp);
		vp = inst.execute(res);
	}

	if (vp and vp->is_atom())
		return as->add_atom(HandleCast(vp));

	return vp;
}

/// execute_argseq -- execute a seq of arguments, return a seq of results.
///
/// Somewhat like force_execute(), but assumes that each atom knows
/// how to execute itself correctly. Much like PutLink, this also tries
/// to deal with multiple arguments that are sets (so that a SetLink
/// has the semantics of "apply to all members of the set")
static inline HandleSeq execute_argseq(AtomSpace* scratch, HandleSeq args,
                                       bool silent)
{
	HandleSeq exargs;
	for (const Handle& h: args)
	{
		if (not h->is_executable())
		{
			// XXX should be be unwrapping SetLinks here?
			exargs.push_back(h);
			continue;
		}

		// If we are here, the argument is executable.
		ValuePtr vp = h->execute(scratch, silent);
		if (vp->is_atom())
		{
			exargs.push_back(HandleCast(vp));
			continue;
		}

		// A kind of ugly hack for now. We need to deal
		// with vectors of Atoms or something, I dunno.
		// But this works, for now.
		if (not (vp->is_type(LINK_VALUE) and 1 == vp->size()))
		{
			exargs.push_back(h);
			continue;
		}

		ValuePtr v0 = LinkValueCast(vp)->value()[0];
		if (v0->is_atom())
			exargs.push_back(HandleCast(v0));
		else
			exargs.push_back(h);
	}
	return exargs;
}

ValuePtr ExecutionOutputLink::execute_once(AtomSpace* as, AtomSpace* scratch, bool silent)
{
	Handle sn(_outgoing[0]);
	Handle args(_outgoing[1]);
	if (sn->is_type(GROUNDED_PROCEDURE_NODE))
	{
		GroundedProcedureNodePtr gsn = GroundedProcedureNodeCast(sn);
		if (nullptr == gsn)
			throw SyntaxException(TRACE_INFO,
				"ExecutionOutputLink: Cannot use naked %s",
				sn->to_string().c_str());

		return gsn->execute_args(as, args, silent);
	}

	if (sn->is_type(DEFINED_PROCEDURE_NODE))
		sn = DefineLink::get_definition(sn);

	if (sn->is_type(FUNCTION_LINK))
	{
		FunctionLinkPtr flp = FunctionLinkCast(sn);
		const FreeVariables& vars = flp->get_vars();
		const HandleSeq& oset(LIST_LINK == args->get_type() ?
			args->getOutgoingSet(): HandleSeq{args});

		Handle reduct;
		if (0 < vars.size())
			reduct = vars.substitute_nocheck(sn, oset, silent);
		else
		{
			HandleSeq vfun = sn->getOutgoingSet();
			vfun.insert(vfun.end(), oset.begin(), oset.end());

			// Do NOT put this in the scratch space! It might
			// contain ValueShimLinks, which would be deadly.
			reduct = createLink(std::move(vfun), sn->get_type());
		}
		ValuePtr vp = reduct->execute(scratch, silent);
		return vp;
	}

	if (sn->is_type(VIRTUAL_LINK))
	{
		HandleSeq vrel = sn->getOutgoingSet();
		const HandleSeq& oset(LIST_LINK == args->get_type() ?
			args->getOutgoingSet(): HandleSeq{args});
		vrel.insert(vrel.end(), oset.begin(), oset.end());

		// Do NOT put this in the scratch space! It might
		// contain ValueShimLinks, which would be deadly.
		Handle reduct = createLink(std::move(vrel), sn->get_type());

		Instantiator inst(scratch);
		return inst.execute(reduct);
	}

	Type st = sn->get_type();
	if (LAMBDA_LINK == st or RULE_LINK == st)
	{
		// Unpack and beta-reduce the Lambda link.
		// Execute the args before plugging them in.
		ScopeLinkPtr slp(ScopeLinkCast(sn));
		Handle body(slp->get_body());
		Variables vars(slp->get_variables());
		const HandleSeq& oset(LIST_LINK == args->get_type() ?
			args->getOutgoingSet(): HandleSeq{args});

		HandleSeq xargs(execute_argseq(scratch, oset, silent));
		return vars.substitute_nocheck(body, xargs);
	}

	return get_handle();
}

DEFINE_LINK_FACTORY(ExecutionOutputLink, EXECUTION_OUTPUT_LINK)
