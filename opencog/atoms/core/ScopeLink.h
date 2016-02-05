/*
 * opencog/atoms/core/ScopeLink.h
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

#ifndef _OPENCOG_SCOPE_LINK_H
#define _OPENCOG_SCOPE_LINK_H

#include <opencog/atoms/core/VariableList.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ScopeLink consitsts of two parts: An optional variable
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
class ScopeLink : public Link
{
protected:
	/// Handle of the (optionally present) variable declaration.
	Handle _vardecl;
	/// Handle of the body of the expression.
	Handle _body;


	/// Variables bound in the body.
	Variables _varlist;

	ScopeLink(Type, const Handle&,
	          TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	          AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ScopeLink(Type, const HandleSeq&,
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
	ScopeLink(const HandleSeq&,
	          TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	          AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ScopeLink(const Handle& varcdecls, const Handle& body,
	          TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	          AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	ScopeLink(Link &l);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }
	const Handle& get_vardecl(void) const { return _vardecl; }
	const Handle& get_body(void) const { return _body; }

	// Return true if the other Handle is equal to this one,
	// i.e. is the same, up to alpha conversion. i.e. is the same,
	// up to a renaming of the bound variables.
	bool is_equal(const Handle&) const;

	// Overload equality check!
	virtual bool operator==(const Atom&) const;
	virtual bool operator!=(const Atom&) const;
};

typedef std::shared_ptr<ScopeLink> ScopeLinkPtr;
static inline ScopeLinkPtr ScopeLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<ScopeLink>(AtomCast(h)); }
static inline ScopeLinkPtr ScopeLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<ScopeLink>(a); }

// XXX temporary hack ...
#define createScopeLink std::make_shared<ScopeLink>

/** @}*/
}

#endif // _OPENCOG_SCOPE_LINK_H
