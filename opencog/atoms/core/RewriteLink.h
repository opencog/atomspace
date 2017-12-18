/*
 * opencog/atoms/core/RewriteLink.h
 *
 * Copyright (C) 2017 Nil Geisweiller
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

#ifndef _OPENCOG_REWRITE_LINK_H
#define _OPENCOG_REWRITE_LINK_H

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The RewriteLink extends the ScopeLink class to add a large variety
/// of methods to rewrite various parts of the ScopeLink in various
/// ways.  These are used by the backward and foreward chainers to
/// edit and create PatternLinks on the fly, thus allowing different
/// kinds of queries to be generated and run as chaining proceeds.
///
class RewriteLink;
typedef std::shared_ptr<RewriteLink> RewriteLinkPtr;
class RewriteLink : public ScopeLink
{
protected:
	RewriteLink(Type, const Handle&);

protected:
	void init(void);

public:
	RewriteLink(const HandleSeq&, Type=REWRITE_LINK);
	RewriteLink(const Handle& varcdecls, const Handle& body);
	RewriteLink(const Link &l);

	/**
	 * Return an alpha-converted copy of this atom. Optionally,
	 * new variable names can be provided. If none are provided,
	 * then new randomly generated names are created.
	 *
	 * Warning: the atomspace treats all alpha-convertible atoms
	 * as identical; if the new copy is inserted into the atomspace,
	 * the original version will be returned.  Alpha-converted atoms
	 * can only be used outside of the atomspace, for temporary
	 * operations.
	 */
	Handle alpha_conversion() const;
	Handle alpha_conversion(const HandleSeq& vars) const;

	/**
	 * Like the above, but using a mapping from old variable names to
	 * new variable names. If an existing variable doesn't have a
	 * mapping specified, then a new random name is generated.
	 */
	Handle alpha_conversion(const HandleMap& vsmap) const;

	/**
	 * Partially substitute a scope link. Given a partial mapping
	 * between variables and values, generate the scope link resulting
	 * from the replacement of the variables by the values. If no
	 * variable is left (i.e. if the provided mapping is complete),
	 * the resulting atom is still a scope link (or inherited) with an
	 * empty variable declaration.
	 */
	Handle partial_substitute(const HandleMap& vm) const;

	/**
	 * Like above but uses a sequence of values instead of variable to
	 * value mapping.
	 */
	Handle partial_substitute(const HandleSeq& values) const;
	
	/**
	 * Helper for partial_substitute. Given a partial mapping from
	 * variables to values, which of them being variables themselves,
	 * generate a new variable declaration with these new variables.
	 */
	Handle partial_substitute_vardecl(const HandleMap& vm) const;
	static Handle substitute_vardecl(const Handle& vardecl,
	                                 const HandleMap& vm);

	/**
	 * Helper for partial_substitute. Given the resulting variable
	 * declaration and a partial mapping from variables to values,
	 * perform partial substitution over all bodies, consuming ill
	 * quotations if necessary.
	 */
	HandleSeq partial_substitute_bodies(const Handle& nvardecl,
	                                    const HandleMap& vm) const;

	/**
	 * Like above but take a sequence of values instead of variable to
	 * value mapping.
	 */
	HandleSeq partial_substitute_bodies(const Handle& nvardecl,
	                                    const HandleSeq& values) const;

	/**
	 * Given a new variable declaration, a body of that scope, and a
	 * list of values. Perform the partial substitution over that body
	 * with these values.
	 */
	Handle partial_substitute_body(const Handle& nvardecl,
	                               const Handle& body,
	                               const HandleSeq& values) const;
	
	/**
	 * Used by partial_substitute.
	 *
	 * After substitution remaining quotations might be useless or
	 * harmful, which might be the case if they deprive a nested
	 * RewriteLink from hiding supposedly hidden variables, consume
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
	 * Return true if the variable declaration of local_scope is a
	 * variable of variables wrapped in a UnquoteLink.
	 */
	static bool is_bound_to_ancestor(const Variables& variables,
	                                 const Handle& local_scope);

	static Handle factory(const Handle&);
};

static inline RewriteLinkPtr RewriteLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<RewriteLink>(AtomCast(h)); }
static inline RewriteLinkPtr RewriteLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<RewriteLink>(a); }

#define createRewriteLink std::make_shared<RewriteLink>

/** @}*/
}

#endif // _OPENCOG_REWRITE_LINK_H
