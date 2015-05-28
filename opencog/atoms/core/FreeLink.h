/*
 * opencog/atoms/reduct/FreeLink.h
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

#ifndef _OPENCOG_FREE_LINK_H
#define _OPENCOG_FREE_LINK_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FreeLink records all of the free variables that occur within
 * (underneath) it, in traversal order. Those variables are placed in
 * sequential order in _varseq. An index is placed in _index. That is,
 * given a variable, its ordinal number is placed in _index.
 *
 * The FreeLink, as a base class, also provides an important method:
 * reduce()
 *
 * The reduce() method takes the given expression, and applies
 * term reduction rules to obtain a smaller but equivalent expression.
 * Ideally, the reduced expression is the "minimal" such expression.
 * There is no guarantee that reduce is normalizing or strongly
 * normalizing, but that does seem like a desirable goal.
 *
 * An expression that contains free variables will contain the same
 * free variables (or a subset of them) after reduction.
 *
 * Note that both EvaluationLinks and ExecutionOutputLinks are
 * reducible, and the result of reduction is always another Atom.
 * This is in contrast to the concept of evaluation/execution:
 * EvaluationLinks, when evaluated, yield truth values, while
 * ExecutionOutputLinks, when executed, yield Atoms.
 */
class FreeLink : public Link
{
protected:
	HandleSeq _varseq;
	std::map<Handle, unsigned int> _index;

	void init(void);
	void find_vars(std::set<Handle>&, const HandleSeq&);
	void build_index(void);

	FreeLink(Type, const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FreeLink(Type, const Handle& a,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FreeLink(Type, const Handle& a, const Handle& b,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
public:
	FreeLink(const HandleSeq& oset,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());
	FreeLink(const Handle& a,
	         TruthValuePtr tv = TruthValue::NULL_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	FreeLink(Link& l);
	virtual ~FreeLink() {}

	const HandleSeq& get_vars(void) { return _varseq; }

	virtual Handle reduce(void);
};

typedef std::shared_ptr<FreeLink> FreeLinkPtr;
static inline FreeLinkPtr FreeLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<FreeLink>(a); }
static inline FreeLinkPtr FreeLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<FreeLink>(a); }

// XXX temporary hack ...
#define createFreeLink std::make_shared<FreeLink>

/** @}*/
}

#endif // _OPENCOG_FREE_LINK_H
