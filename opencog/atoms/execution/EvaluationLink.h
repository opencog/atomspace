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

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

class EvaluationLink : public FreeLink
{
public:
	EvaluationLink(const HandleSeq&&, Type=EVALUATION_LINK);
	EvaluationLink(const Handle& schema, const Handle& args);
	EvaluationLink(const EvaluationLink&) = delete;
	EvaluationLink& operator=(const EvaluationLink&) = delete;

	virtual bool is_evaluatable() const { return true; }
	TruthValuePtr evaluate(AtomSpace* as, bool silent) {
		return do_evaluate(as, get_handle(), silent);
	}

	static TruthValuePtr do_evaluate(AtomSpace*, const Handle&,
	                                 bool silent=false);
	static TruthValuePtr do_eval_scratch(AtomSpace* main,
	                                     const Handle&,
	                                     AtomSpace* scratch,
	                                     bool silent=false);

	static TruthValuePtr do_evaluate(const AtomSpacePtr& asp, const Handle& h,
	                                 bool silent=false)
	{
		return do_evaluate(asp.get(), h, silent);
	}

	static bool crisp_evaluate(AtomSpace*, const Handle&,
	                           bool silent=false);
	static bool crisp_eval_scratch(AtomSpace* main,
	                               const Handle&,
	                               AtomSpace* scratch,
	                               bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(EvaluationLink)
#define createEvaluationLink CREATE_DECL(EvaluationLink)

/** @}*/
}

#endif // _OPENCOG_EVALUATION_LINK_H
