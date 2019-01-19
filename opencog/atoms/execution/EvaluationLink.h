/*
 * opencog/atoms/execution/EvaluationLink.h
 *
 * Copyright (C) 2013,2014,2015 Linas Vepstas
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

#ifndef _OPENCOG_EVALUTATION_LINK_H
#define _OPENCOG_EVALUTATION_LINK_H

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
	EvaluationLink(const HandleSeq&, Type=EVALUATION_LINK);
	EvaluationLink(const Handle& schema, const Handle& args);
	EvaluationLink(const Link& l);

	ValuePtr evaluate(AtomSpace* as) {
	    return do_evaluate_value(as, get_handle());
	}

	static ValuePtr do_evaluate_value(AtomSpace*, const Handle&,
	                                 bool silent=false);
	static ValuePtr do_eval_scratch_value(AtomSpace* main,
	                                     const Handle&,
	                                     AtomSpace* scratch,
	                                     bool silent=false);
	static ValuePtr do_evaluate_value(AtomSpace*,
	                                 const HandleSeq& schema_and_args,
	                                 bool silent=false);
	static ValuePtr do_evaluate_value(AtomSpace*,
	                                 const Handle& schema, const Handle& args,
	                                 bool silent=false);

	static ValuePtr do_evaluate(AtomSpace* as, const Handle& h,
	                                 bool silent=false) {
		return do_evaluate_value(as, h, silent);
	}
	static ValuePtr do_eval_scratch(AtomSpace* main,
	                                     const Handle& h,
	                                     AtomSpace* scratch,
	                                     bool silent=false) {
		return do_eval_scratch_value(main, h, scratch, silent);
	}
	static TruthValuePtr do_evaluate(AtomSpace* as,
	                                 const HandleSeq& schema_and_args,
	                                 bool silent=false) {
		return TruthValueCast(do_evaluate_value(as, schema_and_args, silent));
	}
	static TruthValuePtr do_evaluate(AtomSpace* as,
	                                 const Handle& schema, const Handle& args,
	                                 bool silent=false) {
		return TruthValueCast(do_evaluate_value(as, schema, silent));
	}

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<EvaluationLink> EvaluationLinkPtr;
static inline EvaluationLinkPtr EvaluationLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<EvaluationLink>(a); }
static inline EvaluationLinkPtr EvaluationLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<EvaluationLink>(a); }

#define createEvaluationLink std::make_shared<EvaluationLink>

/**
 * setLocalPredicate("foo", boo) enables creating GroundedPredicateNode with the name "lib:\\foo",
 * which will call boo on evaluation of corresponding EvaluationLink.
 */
void setLocalPredicate(std::string funcName, TruthValuePtr* (*func)(AtomSpace *, Handle*));

/** @}*/
}

#endif // _OPENCOG_EVALUTATION_LINK_H
