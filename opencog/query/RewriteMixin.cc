/*
 * RewriteMixin.cc
 *
 * Copyright (C) 2009, 2014, 2025 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/free/QuoteReduce.h>
#include <opencog/atomspace/AtomSpace.h>

#include "RewriteMixin.h"

using namespace opencog;

RewriteMixin::RewriteMixin(AtomSpace* as, ContainerValuePtr& qvp)
	: _as(as), _result_queue(qvp),
	_num_results(0), max_results(SIZE_MAX)
{
}

void RewriteMixin::setup_marginals(void)
{
	// Grab the places where we'll record the marginals.
	for (const Handle& var: _varseq)
	{
		ValuePtr vp(_plp->getValue(var));
		ContainerValuePtr cvp(ContainerValueCast(vp));
		if (nullptr == cvp) continue;
		if (cvp->is_closed())
		{
			cvp->clear();
			cvp->open();
		}
		_var_marginals.insert({var, cvp});
	}

	// Record the implicands, too
	for (const Handle& himp: _implicand)
	{
		ValuePtr vp(_plp->getValue(himp));
		ContainerValuePtr cvp(ContainerValueCast(vp));
		if (nullptr == cvp) continue;
		if (cvp->is_closed())
		{
			cvp->clear();
			cvp->open();
		}
		_implicand_grnds.insert({himp, cvp});
	}
}

void RewriteMixin::record_marginals(const GroundingMap& var_soln)
{
	for (const Handle& hv : _varseq)
	{
		// Optional clauses (e.g. AbsentLink) may have variables
		// in them that are not grounded. Those variables won't
		// have a grounding; this will cause std::map::at to throw.
		try
		{
			ValuePtr gvp(var_soln.at(hv));
			auto it = _var_marginals.find(hv);
			if (_var_marginals.end() != it)
				(*it).second->add(gvp);
		}
		catch (...) {}
	}
}

/**
 * instantiate -- create a grounded expression from an ungrounded one,
 * and then execute it.  That is, beta-reduce, then execute.
 *
 * Given a handle to an ungrounded expression, and a set of groundings,
 * this will create a grounded expression.
 *
 * The set of groundings is to be passed in with the map 'vars', which
 * maps variable names to their groundings -- it maps variable names to
 * atoms that already exist in the atomspace.  This method will then go
 * through all of the variables in the expression, and substitute them
 * with their grounding, creating a new expression. The new expression
 * is then executed in the provided AtomSpace.
 */
static ValuePtr instantiate(AtomSpace* as,
                            const GroundingMap& varmap,
                            const Handle& expr,
                            bool silent)
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	Type t = expr->get_type();

	// Execute any DefinedPredicateNodes
	if (nameserver().isA(t, DEFINED_PREDICATE_NODE) or
	    nameserver().isA(t, DEFINED_SCHEMA_NODE))
	{
		Handle defn(DefineLink::get_definition(expr));
		if (not defn->is_executable())
			return defn;
		return defn->execute(as, silent);
	}

	// Beta-reduce, respecting quotes
	QuoteReduce qreduce(varmap);
	Handle grounded(qreduce.walk_tree(expr));

	// Fire executable links.
	// We currently exclude EVALUATABLE_LINK here; the evaluatables
	// will have (true == grounded->is_executable()) but currently
	// a dozen unit tests fail if we execute them. I don't know why.
	Type gt = grounded->get_type();
	if (nameserver().isA(gt, EXECUTABLE_LINK))
		return grounded->execute(as, silent);

	return grounded;
}

/**
 * This callback takes the reported grounding, runs it through the
 * instantiator, to create the implicand, and then records the result
 * in the `result_set`. Repeated solutions are skipped. If the number
 * of unique results so far is less than `max_results`, it then returns
 * false, to search for more groundings.  (The engine will halt its
 * search for a grounding once an acceptable one has been found; so,
 * to continue hunting for more, we return `false` here. We want to
 * find all possible groundings.)
 */
bool RewriteMixin::propose_grounding(const GroundingMap& var_soln,
                                     const GroundingMap& term_soln)
{
	LOCK_PE_MUTEX;
	// PatternMatchEngine::print_solution(var_soln, term_soln);

	// If we found as many as we want, then stop looking for more.
	if (_num_results >= max_results)
		return true;

	_num_results ++;

	// Record marginals for variables.
	record_marginals(var_soln);

	// Catch and ignore SilentExceptions. This arises when
	// running with the URE, which creates ill-formed links
	// (due to rules producing nothing). Ideally this should
	// be treated as a user error, that is, the user should
	// design rule pre-conditions to prevent them from producing
	// nothing.  In practice it is difficult to insure, so
	// meanwhile this try-catch is used.
	// See issue #950 and pull req #962. XXX FIXME later.
	// Tested by BuggyBindLinkUTest and NoExceptionUTest.
	// Well, given that URE is dead meat, maybe we can remove this?
	try {
		if (1 == _implicand.size())
		{
			ValuePtr v(instantiate(_as, var_soln, _implicand[0], true));
			// AbsentLinks can result in nullptr v's
			if (nullptr != v)
			{
				if (v->is_atom())
					v = _as->add_atom(HandleCast(v));
				auto it = _implicand_grnds.find(_implicand[0]);
				if (_implicand_grnds.end() != it)
					(*it).second->add(v);
				insert_result(v);
			}
		}
		else
		{
			ValueSeq vs;
			for (const Handle& himp: _implicand)
			{
				ValuePtr v(instantiate(_as, var_soln, himp, true));
				if (nullptr != v)
				{
					if (v->is_atom())
						v = _as->add_atom(HandleCast(v));
					auto it = _implicand_grnds.find(himp);
					if (_implicand_grnds.end() != it)
						(*it).second->add(v);
					vs.emplace_back(v);
				}
			}
			insert_result(createLinkValue(std::move(vs)));
		}
	} catch (const SilentException& ex) {}

	// If we found as many as we want, then stop looking for more.
	return (_num_results >= max_results);
}

void RewriteMixin::insert_result(ValuePtr v)
{
	if (_result_set.end() != _result_set.find(v)) return;

	// Insert atom into the atomspace immediately. This avoids having
	// the atom appear twice, once unassigned to any AS, and the other
	// in the AS.
	if (v->is_atom())
		v = _as->add_atom(HandleCast(v));

	if (_result_set.end() != _result_set.find(v)) return;

	_result_set.insert(v);
	_result_queue->add(std::move(v));
}

bool RewriteMixin::start_search(void)
{
	if (_result_queue->is_closed())
	{
		_result_queue->clear();
		_result_queue->open();
	}
	return false;
}

bool RewriteMixin::search_finished(bool done)
{
	for (auto& mgs : _var_marginals)
		mgs.second->close();

	for (auto& igs : _implicand_grnds)
		igs.second->close();

	_result_queue->close();
	return done;
}

/* ===================== END OF FILE ===================== */
