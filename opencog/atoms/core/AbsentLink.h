/*
 * opencog/atoms/core/AbsentLink.h
 *
 * Copyright (C) 2017,2021 Linas Vepstas
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

#ifndef _OPENCOG_ABSENT_LINK_H
#define _OPENCOG_ABSENT_LINK_H

#include <opencog/atoms/core/UnorderedLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The AbsentLink specifies a set that contains no duplicated elements.
/// That is, all elements of the AbsentLik are pair-wise distinct.
/// The constructor removes duplicates.
///
/// For example,
///
///     AbsentLink
///         SomeAtom
///         SomeAtom
///         OtherAtom
///
/// is exactly the same as
///
///     AbsentLink
///         SomeAtom
///         OtherAtom
///
/// and the copies of the duplicated `SomeAtom` is removed during atom
/// construction.
///
/// Conceptually, the ctor for AbsentLink applies a rule of inference,
/// called the "Rule of contraction (or idempotency of entailment)"
/// https://en.wikipedia.org/wiki/Rule_of_inference
/// https://en.wikipedia.org/wiki/Idempotency_of_entailment
///
class AbsentLink : public UnorderedLink
{
	void init(void);
public:
	AbsentLink(const HandleSeq&&, Type=ABSENT_LINK);

	AbsentLink(const AbsentLink &) = delete;
	AbsentLink& operator=(const AbsentLink &) = delete;

	virtual bool is_evaluatable() const { return true; }
	virtual bool is_executable() const { return true; }

	virtual TruthValuePtr evaluate(AtomSpace*, bool silent=false);
	virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
		return ValueCast(evaluate(as, silent));
	}

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<AbsentLink> AbsentLinkPtr;
static inline AbsentLinkPtr AbsentLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<AbsentLink>(h); }
static inline AbsentLinkPtr AbsentLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<AbsentLink>(a); }

#define createAbsentLink std::make_shared<AbsentLink>

/** @}*/
}

#endif // _OPENCOG_ABSENT_LINK_H
