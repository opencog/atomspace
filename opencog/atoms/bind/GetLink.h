/*
 * opencog/atoms/GetLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_GET_LINK_H
#define _OPENCOG_GET_LINK_H

#include <opencog/atoms/bind/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The body of the ConcreteLink is assumed to collection of clauses
/// to be satsified. Thus, the body is typically an AndLink, ChoiceLink
/// or a SequentialAnd, depending on how they are to be satsified.
/// This is very much like a ConcreteLink, except that it may contain
/// clauses that are virtual (e.g. GreaterThanLink, or EvaluationLinks
/// with GroundedPredicateNodes).
///
/// It is similar to a BindLink, except that a BindLink also causes an
/// implication to be performed: after a grounding is found, the
/// BindLink then causes the implication to run with the resultant
/// grounding.  The GetLink does not specify what should happen
/// with the grounding, although the (cog-satisfy) scheme call returns
/// a truth value.
class GetLink : public PatternLink
{
protected:

	GetLink(Type, const HandleSeq&,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	void init(void);

public:
	GetLink(const Handle& body,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	GetLink(const Handle& varcdecls, const Handle& body,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	GetLink(const HandleSeq&,
	        TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	        AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	GetLink(Link &l);

	bool satisfy(PatternMatchCallback&) const;
};

typedef std::shared_ptr<GetLink> GetLinkPtr;
static inline GetLinkPtr GetLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<GetLink>(a); }
static inline GetLinkPtr GetLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<GetLink>(a); }

// XXX temporary hack ...
#define createGetLink std::make_shared<GetLink>

/** @}*/
}

#endif // _OPENCOG_GET_LINK_H
