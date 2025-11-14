/*
 * opencog/atoms/execution/EvaluationLink.h
 *
 * Copyright (C) 2013,2014,2015 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_EVALUATION_LINK_H
#define _OPENCOG_EVALUATION_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FreeLink.h>
#include <opencog/atoms/value/BoolValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

/**
 * EvaluationLinks are 120 Bytes larger than ordinary links, because
 * the FreeLinks needs that space to hold variables found inside
 * expressions. To avoid this RAM usage, use EdgeLink instead. It's
 * the same idea, but without the RAM overhead.
 *
 * FWIW: a "naked" EdgeLink is 224 Bytes; an EdgeLink with a FloatValue
 * on it, stored in the AtomSpace is about 496 Bytes; see Atom.h for
 * details. So using EvaluationLink costs 120/496=25% extra RAM over
 * EdgeLinks. Ooof.
 */
class EvaluationLink : public FreeLink
{
public:
	EvaluationLink(const HandleSeq&&, Type=EVALUATION_LINK);
	EvaluationLink(const Handle& schema, const Handle& args);
	EvaluationLink(const EvaluationLink&) = delete;
	EvaluationLink& operator=(const EvaluationLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
		return evaluate(as, silent);
	}

	virtual bool is_evaluatable() const { return true; }
	ValuePtr evaluate(AtomSpace* scratch, bool silent) {
		return ValueCast(createBoolValue(bevaluate(scratch, silent)));
	}

	virtual bool bevaluate(AtomSpace* scratch, bool silent=false) {
		return crisp_eval_scratch(_atom_space, get_handle(), scratch, silent);
	}

	static bool crisp_eval_scratch(AtomSpace* main,
	                               const Handle&,
	                               AtomSpace* scratch,
	                               bool silent=false);

	static ValuePtr do_evaluate(AtomSpace* as,
	                            const Handle& evelnk,
	                            bool silent=false)
	{
		return ValueCast(createBoolValue(crisp_eval_scratch(as, evelnk, as, silent)));
	}

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(EvaluationLink)
#define createEvaluationLink CREATE_DECL(EvaluationLink)

/** @}*/
}

#endif // _OPENCOG_EVALUATION_LINK_H
