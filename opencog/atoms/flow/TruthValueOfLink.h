/*
 * opencog/atoms/flow/TruthValueOfLink.h
 *
 * Copyright (C) 2018 Linas Vepstas
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

#ifndef _OPENCOG_TRUTH_VALUE_OF_LINK_H
#define _OPENCOG_TRUTH_VALUE_OF_LINK_H

#include <opencog/atoms/flow/ValueOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TruthValueOfLink returns the truth value on the indicated atom.
///
class TruthValueOfLink : public ValueOfLink
{
public:
	TruthValueOfLink(const HandleSeq&&, Type=TRUTH_VALUE_OF_LINK);

	TruthValueOfLink(const TruthValueOfLink &) = delete;
	TruthValueOfLink operator=(const TruthValueOfLink &) = delete;

	virtual bool is_evaluatable() const { return true; }

	// Return a pointer to the truth value for the wrapped atom.
	virtual TruthValuePtr evaluate(AtomSpace*, bool);
	virtual ValuePtr execute(AtomSpace* as, bool silent) {
		return ValueCast(evaluate(as, silent));
	}

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TruthValueOfLink)
#define createTruthValueOfLink CREATE_DECL(TruthValueOfLink)

// ====================================================================

/// The StrengthOfLink returns the strength of a truth value on the
/// indicated atom. (Strength is the first of the sequence of floats).
///
class StrengthOfLink : public ValueOfLink
{
public:
	StrengthOfLink(const HandleSeq&&, Type=STRENGTH_OF_LINK);

	StrengthOfLink(const StrengthOfLink&) = delete;
	StrengthOfLink& operator=(const StrengthOfLink&) = delete;

	// Return a pointer to the extracted value.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(StrengthOfLink)
#define createStrengthOfLink CREATE_DECL(StrengthOfLink)

// ====================================================================

/// The ConfidenceOfLink returns the confidence of a truth value on the
/// indicated atom. (Confidence is the first of the sequence of floats).
///
class ConfidenceOfLink : public ValueOfLink
{
public:
	ConfidenceOfLink(const HandleSeq&&, Type=CONFIDENCE_OF_LINK);

	ConfidenceOfLink(const ConfidenceOfLink&) = delete;
	ConfidenceOfLink operator=(const ConfidenceOfLink&) = delete;

	// Return a pointer to the extracted value.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ConfidenceOfLink)
#define createConfidenceOfLink CREATE_DECL(ConfidenceOfLink)

// ====================================================================

/// The CountOfLink returns the count of a truth value on the
/// indicated atom. (Count is the third of the sequence of floats).
///
class CountOfLink : public ValueOfLink
{
public:
	CountOfLink(const HandleSeq&&, Type=COUNT_OF_LINK);

	CountOfLink(const CountOfLink&) = delete;
	CountOfLink& operator=(const CountOfLink&) = delete;

	// Return a pointer to the extracted value.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(CountOfLink)
#define createCountOfLink CREATE_DECL(CountOfLink)

/** @}*/
}

#endif // _OPENCOG_TRUTH_VALUE_OF_LINK_H
