/*
 * opencog/atoms/core/PresentLink.h
 *
 * Copyright (C) 2017 Linas Vepstas
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

#ifndef _OPENCOG_PRESENT_LINK_H
#define _OPENCOG_PRESENT_LINK_H

#include <opencog/atoms/core/UnorderedLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PresentLink specifies a set that contains no duplicated elements.
/// That is, all elements of the PresentLik are pair-wise distinct.
/// The constructor removes duplicates.
///
/// For example,
///
///     PresentLink
///         SomeAtom
///         SomeAtom
///         OtherAtom
///
/// is exactly the same as
///
///     PresentLink
///         SomeAtom
///         OtherAtom
///
/// and the copies of the duplicated `SomeAtom` is removed during atom
/// contruction.
///
/// Conceptually, the ctor for PresentLink applies a rule of inference,
/// called the "Rule of contraction (or idempotency of entailment)"
/// https://en.wikipedia.org/wiki/Rule_of_inference
/// https://en.wikipedia.org/wiki/Idempotency_of_entailment
///
class PresentLink : public UnorderedLink
{
	void init(void);
public:
	PresentLink(const HandleSeq&&, Type=PRESENT_LINK);

	PresentLink(const PresentLink &) = delete;
	PresentLink& operator=(const PresentLink &) = delete;

	virtual bool is_evaluatable() const { return true; }
	virtual bool is_executable() const { return true; }

	virtual TruthValuePtr evaluate(AtomSpace*, bool silent=false);
	virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
		return ValueCast(evaluate(as, silent));
	}

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<PresentLink> PresentLinkPtr;
static inline PresentLinkPtr PresentLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<PresentLink>(h); }
static inline PresentLinkPtr PresentLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<PresentLink>(a); }

#define createPresentLink std::make_shared<PresentLink>

/** @}*/
}

#endif // _OPENCOG_PRESENT_LINK_H
