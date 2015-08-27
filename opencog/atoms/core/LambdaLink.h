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

#include <opencog/atoms/core/VariableList.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LambdaLink consitsts of two parts: An optional variable
/// declaration, followed by an expression body (of arbitrary form).
/// If a variable declaration is present, then it must conform to current
/// variable declaration standards: i.e. it must be either a single
/// VariableNode, a single TypedVariableLink, or a VariableList.  If a
/// variable declaration is missing, then the body is searched for all
/// free variables, these are then bound.
///
/// This class does little other than to check for the above-described
/// format, and unpacke the variable decalrations, if present; it will
/// throw an error if the variables are somehow ill-formed. As usual,
/// the point of unpacked variables is to act as a memo or cache,
/// speeding up later calculations.
///
class LambdaLink : public Link
{
protected:

	/// Variables bound in the body.
	Variables _varlist;

	/// Handle of the body of the expression.
	Handle _body;

	LambdaLink(Type, const Handle&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(Type, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	void init(void);
	void extract_variables(const HandleSeq& oset);
	void init_scoped_variables(const Handle& hvar);

	// utility debug print
	static void prt(const Handle& h)
	{
		printf("%s\n", h->toShortString().c_str());
	}
public:
	LambdaLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(const Handle& varcdecls, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	LambdaLink(Link &l);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }
	const Handle& get_body(void) const { return _body; }

	// Take the list of values `vals`, and substitute them in for the
	// variables in the body of this lambda. The values must satisfy all
	// type restrictions, else an exception will be thrown.
/*
	Handle substitute (const HandleSeq& vals) const
	{
		return VariableList::substitute(_body, vals);
	}
*/
};

typedef std::shared_ptr<LambdaLink> LambdaLinkPtr;
static inline LambdaLinkPtr LambdaLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<LambdaLink>(a); }
static inline LambdaLinkPtr LambdaLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<LambdaLink>(a); }

// XXX temporary hack ...
#define createLambdaLink std::make_shared<LambdaLink>

/** @}*/
}

#endif // _OPENCOG_LAMBDA_LINK_H
