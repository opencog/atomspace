/*
 * opencog/atoms/scope/GuardLink.h
 *
 * Copyright (C) 2026 BrainyBlaize Dynamics LLC
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
 */

#ifndef _OPENCOG_GUARD_LINK_H
#define _OPENCOG_GUARD_LINK_H

#include <mutex>
#include <opencog/atoms/scope/ScopeLink.h>
#include <opencog/atoms/free/Quotation.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The GuardLink provides methods to determine if a proposed
/// beta reduction is compatible with the argument types and
/// function body. It also provides pattern-matching extraction
/// of variable groundings from input data.
///
class GuardLink : public ScopeLink
{
private:
	mutable std::once_flag _init_flag;
	void do_init(void) const;

	// Flag for recursive glob matching state
	mutable bool _recursive_glob;
	void init_globby_terms(void) const;

	// Globby terms are terms that contain a GlobNode
	mutable HandleSet _globby_terms;

protected:
	// Pattern clause to match against.
	// Might be null if there are only guards.
	mutable Handle _match_pattern;

	// Evaluatable terms to check after extraction
	mutable HandleSeq _guard_clauses;

	void init(void) const;

	bool extract(const Handle& termpattern, const ValuePtr& gnd,
	             ValueMap& valmap, AtomSpace* scratch, bool silent,
	             const Quotation& quotation = Quotation()) const;
	bool eval_guard(const ValueMap&, AtomSpace*, bool) const;

public:
	GuardLink(const HandleSeq&&, Type=GUARD_LINK);
	GuardLink(const GuardLink &) = delete;
	GuardLink& operator=(const GuardLink &) = delete;

	/// Determine if the proposed grounding `gnd` is compatible with
	/// the variable declarations of this ScopeLink. If it is, then
	/// extract groundings for the bound variables (by pattern matching.)
	/// Returns true if a grounding is possible; with groundings returned
	/// in `valmap`.
	bool guard(const ValuePtr& gnd, ValueMap& valmap,
	           AtomSpace* scratch, bool silent=false) const;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(GuardLink)
#define createGuardLink CREATE_DECL(GuardLink)

/** @}*/
}

#endif // _OPENCOG_GUARD_LINK_H
