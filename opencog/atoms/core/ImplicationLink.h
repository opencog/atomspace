/*
 * opencog/atoms/core/ImplicationLink.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 * All Rights Reserved
 * Author: Nil Geisweiller
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

#ifndef _OPENCOG_IMPLICATION_LINK_H
#define _OPENCOG_IMPLICATION_LINK_H

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ImplicationLink has 2 forms
///
/// ImplicationLink
///    <P>
///    <Q>
///
/// where P and Q are predicates. Or
///
/// ImplicationLink
///    <variables>
///    <P-body>
///    <Q-body>
///
/// A syntactic sugar form for
///
/// ImplicationLink
///    LambdaLink
///       <variables>
///       <P-body>
///    LambdaLink
///       <variables>
///       <Q-body>
///
/// Due to this syntactic sugar form it must inherit from a
/// ScopeLink. In the case where the former (non syntactic sugar) form
/// is used, it will still inherit from a ScopeLink (due to C++
/// inheritance mechanics), but will merely have an empty bound
/// variable set.
///
class ImplicationLink : public ScopeLink
{
protected:
	ImplicationLink(Type, const HandleSeq&,
	                TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	                AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	void init(void);
	void extract_variables(const HandleSeq& oset);
	// Useless for now, but...
	Handle _implicand;
public:
	ImplicationLink(const HandleSeq&,
	                TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	                AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ImplicationLink(Link &l);
};

typedef std::shared_ptr<ImplicationLink> ImplicationLinkPtr;
static inline ImplicationLinkPtr ImplicationLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<ImplicationLink>(a); }
static inline ImplicationLinkPtr ImplicationLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<ImplicationLink>(a); }

// XXX temporary hack ...
#define createImplicationLink std::make_shared<ImplicationLink>

/** @}*/
}

#endif // _OPENCOG_IMPLICATION_LINK_H
