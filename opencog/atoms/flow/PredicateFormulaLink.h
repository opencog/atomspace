/*
 * opencog/atoms/flow/PredicateFormulaLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULA_LINKR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_PREDICATE_FORMULA_LINK_H
#define _OPENCOG_PREDICATE_FORMULA_LINK_H

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PredicateFormulaLink returns the truth value after
/// some computations.
///
class PredicateFormulaLink : public ScopeLink
{
protected:
	void init();

public:
	PredicateFormulaLink(const HandleSeq&&, Type=PREDICATE_FORMULA_LINK);

	PredicateFormulaLink(const PredicateFormulaLink &) = delete;
	PredicateFormulaLink operator=(const PredicateFormulaLink &) = delete;

	// Apply formula to arguments.
	TruthValuePtr apply(AtomSpace*, const HandleSeq&, bool);

	virtual bool is_evaluatable() const { return true; }
	virtual bool is_executable() const { return true; }

	// Return a pointer to the computed truth value.
	virtual TruthValuePtr evaluate(AtomSpace*, bool);
	virtual ValuePtr execute(AtomSpace* as, bool silent) {
		return ValueCast(evaluate(as, silent));
	}

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PredicateFormulaLink)
#define createPredicateFormulaLink CREATE_DECL(PredicateFormulaLink)

/** @}*/
}

#endif // _OPENCOG_PREDICATE_FORMULA_LINK_H
