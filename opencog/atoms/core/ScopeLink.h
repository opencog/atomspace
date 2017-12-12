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

#include <opencog/atoms/core/Quotation.h>
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
class ScopeLink;
typedef std::shared_ptr<ScopeLink> ScopeLinkPtr;
class ScopeLink : public Link
{
protected:
	/// Handle of the (optionally present) variable declaration.
	Handle _vardecl;
	/// Handle of the body of the expression.
	Handle _body;

	/// Variables bound in the body.
	Variables _varlist;

	ScopeLink(Type, const Handle&);

protected:
	void init(void);
	void extract_variables(const HandleSeq& oset);
	void init_scoped_variables(const Handle& hvar);

	bool skip_init(Type);
	ContentHash term_hash(const Handle&, UnorderedHandleSet&,
	                      Quotation quotation = Quotation()) const;
	virtual ContentHash compute_hash() const;

public:
	ScopeLink(const HandleSeq&, Type=SCOPE_LINK);
	ScopeLink(const Handle& varcdecls, const Handle& body);
	ScopeLink(const Link &l);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }
	const Handle& get_vardecl(void) const { return _vardecl; }
	const Handle& get_body(void) const { return _body; }

	// Return true if the other Handle is equal to this one,
	// i.e. is the same, up to alpha conversion. i.e. is the same,
	// up to a renaming of the bound variables.
	bool is_equal(const Handle&, bool silent=false) const;

	/**
	 * Return an alpha converted copy of itself. New variable names
	 * can be optionally provided, otherwise there are randomly
	 * generated.
	 *
	 * Warning: the alpha converted handle is not inserted in the
	 * atomspace, it is up to the user to do so.
	 */
	Handle alpha_conversion() const;
	Handle alpha_conversion(const HandleSeq& vars) const;

	/**
	 * Like above but take a mapping from old variable name to new
	 * variable names. If an old variable doesn't have a mapping then
	 * its new random name is randomly generated.
	 */
	Handle alpha_conversion(const HandleMap& vsmap) const;

	/**
	 * Partially substitute a scope link. Given a partial mapping
	 * between variables and values, generate the scope link resulting
	 * from the replacement of the variables by the values. In no
	 * variable is left (i.e. if the provided mapping is complete),
	 * the resulting atom is still a scope link (or inherited) with an
	 * empty variable declaration.
	 */
	Handle partial_substitute(const HandleMap& vm) const;

	/**
	 * Helper for partial_substitute. Given a partial mapping from
	 * variables to values, which of them being variables themselves,
	 * generate a new variable declaration with these new variables.
	 */
	Handle partial_substitute_vardecl(const HandleMap& vm) const;
	static Handle substitute_vardecl(const Handle& vardecl,
	                                 const HandleMap& vm);

	/**
	 * Used by partial_substitute.
	 *
	 * After substitution remaining quotations might be useless or
	 * harmful, which might be the case if they deprive a nested
	 * ScopeLink from hiding supposedly hidden variables, consume
	 * them.
	 *
	 * Specifically this code makes 2 assumptions
	 *
	 * 1. LocalQuotes in front root level And, Or or Not links on the
	 *    pattern body are not consumed because they are supposedly
	 *    used to avoid interpreting them as pattern matcher
	 *    connectors.
	 *
	 * 2. Quote/Unquote are used to wrap scope links so that their
	 *    variable declaration can pattern match grounded or partially
	 *    grounded scope links.
	 *
	 * 3. Quote/Unquote are also used to wrap Evaluation containing
	 *    GroundedPredicate and possibly other atom types with special
	 *    handling by the pattern matcher.
	 *
	 * No other use of quotation is assumed besides the 3 above.
	 */
	Handle consume_ill_quotations() const;
	static Handle consume_ill_quotations(const Handle& vardecl, const Handle& h);
	static Handle consume_ill_quotations(const Variables& variables, Handle h,
	                                     Quotation quotation=Quotation(),
	                                     bool escape=false /* ignore the next
	                                                        * quotation
	                                                        * consumption */);

	/**
	 * Return true iff the variable declaration of local_scope is a
	 * variable of variables wrapped in a UnquoteLink.
	 */
	static bool is_bound_to_ancestor(const Variables& variables,
	                                 const Handle& local_scope);

	// Overload equality check!
	virtual bool operator==(const Atom&) const;
	virtual bool operator!=(const Atom&) const;

	static Handle factory(const Handle&);
};

static inline ScopeLinkPtr ScopeLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<ScopeLink>(AtomCast(h)); }
static inline ScopeLinkPtr ScopeLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<ScopeLink>(a); }

#define createScopeLink std::make_shared<ScopeLink>

/** @}*/
}

#endif // _OPENCOG_SCOPE_LINK_H
