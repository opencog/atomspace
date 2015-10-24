/*
 * opencog/atoms/core/LambdaLink.h
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

#ifndef _OPENCOG_LAMBDA_LINK_H
#define _OPENCOG_LAMBDA_LINK_H

#include <map>

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LambdaLink is a ScopeLink that implements beta-reduction.
/// It does little more than to provide a method that subsitutes values
/// into the variables bound by ScopeLink.
///
class LambdaLink : public ScopeLink
{
protected:

	LambdaLink(Type, const Handle&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(Type, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

public:
	LambdaLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(const Handle& varcdecls, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(Link &l);

	// Take the list of values `vals`, and substitute them in for the
	// variables in the body of this lambda. The values must satisfy all
	// type restrictions, else an exception will be thrown.
/*
	Handle substitute(const HandleSeq& vals) const
	{
		return VariableList::substitute(_body, vals);
	}
*/
};

typedef std::shared_ptr<LambdaLink> LambdaLinkPtr;
static inline LambdaLinkPtr LambdaLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<LambdaLink>(AtomCast(h)); }
static inline LambdaLinkPtr LambdaLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<LambdaLink>(a); }

// XXX temporary hack ...
#define createLambdaLink std::make_shared<LambdaLink>

/** @}*/
}

#endif // _OPENCOG_LAMBDA_LINK_H
