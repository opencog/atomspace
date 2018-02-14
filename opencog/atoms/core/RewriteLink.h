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

/// The RewriteLink extends the ScopeLink class to add several
/// methods to perform alpha-conversion and beta-reduction on the
/// ScopeLink.  Note that the beta-reduction being performed is
/// NOT compatible with classical Lambda Calculus: it mixes together,
/// in one place, both alpha and beta conversions. That's OK -
/// Atomese is not Lambda Calculus; its more natural to do it this
/// way in Atomese.
///
/// The methods here are used by the backward and forward chainers to
/// edit and create PatternLinks on the fly, thus allowing different
/// kinds of queries to be generated and run as chaining proceeds.
///
class RewriteLink;
typedef std::shared_ptr<RewriteLink> RewriteLinkPtr;
class RewriteLink : public ScopeLink
{
protected:
	bool _silent;
	void init(void);

	/**
	 * Perform "substitution" on a variable declaration.  This
	 * returns a new variable declaration, where one of two things
	 * were done.  If the map held a constant value for a variable,
	 * then that variable is removed.  If the map held a different
	 * variable, then the alpha-conversion is performed. This might
	 * return the invalid handle, if all variables were reduced by
	 * constants.
	 */
	Handle substitute_vardecl(const HandleMap& vm) const;
	static Handle substitute_vardecl(const Handle& vardecl,
	                                 const HandleMap& vm);

	/**
	 * Perform "substitution" on all of the "bodies" in the link.
	 * (There may be more than two atoms in the outgoing set; this
	 * performs the substitution on all atoms that are not initial
	 * variable declaration).
	 *
	 * The substitution performs either a beta-reduction, or an
	 * alpha-conversion, depending on the map. If the map specifies
	 * variable->value, then a normal beta reduction is done. If
	 * the maps specifies variable->variable, then an alpha renaming
	 * is done.
	 *
	 * If there are any poorly-formed (ill-formed) quotations,
	 * these are removed.
	 */
	HandleSeq substitute_bodies(const Handle& nvardecl,
	                            const HandleMap& vm) const;

	/**
	 * Like above but uses a mapping from variables to values instead
	 * of a sequence of values.
	 */
	Handle substitute_body(const Handle& nvardecl,
	                       const Handle& body,
	                       const HandleMap& vm) const;

	/**
	 * Return true if the variable declaration of local_scope contains
	 * variables in `variables`. If that is the case then we can infer
	 * that its declaration should in fact remain unquoted.
	 */
	static bool is_bound_to_ancestor(const Variables& variables,
	                                 const Handle& local_scope);
	/**
	 * Like is_bound_to_ancestor but doesn't assume that the handle is
	 * a scope, and test for it as well, returning false if it isn't.
	 */
	static bool is_scope_bound_to_ancestor(const Variables& variables, const Handle& h);

public:
	RewriteLink(const HandleSeq&, Type=REWRITE_LINK);
	RewriteLink(const Handle& varcdecls, const Handle& body);
	RewriteLink(const Link &l);

	void make_silent(bool s) { _silent = s; }

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
	Handle alpha_convert() const;
	Handle alpha_convert(const HandleSeq& vars) const;

	/**
	 * Like the above, but using a mapping from old variable names
	 * to new variable names. If an existing variable doesn't have
	 * a mapping specified, then a new random name is generated.
	 */
	Handle alpha_convert(const HandleMap& vsmap) const;

	/**
	 * Perform a beta-reduction and optional alpha-conversion,
	 * returning the reduced RewriteLink.
	 *
	 * If the map specifies a variable->value, then a standard
	 * beta-reduction is performed, and the variable is removed
	 * from the returned RewriteLink.
	 *
	 * If the map specifies a variable->new-variable, then an
	 * alpha-conversion is performed, replacing the old variable
	 * with the new one.  The variable continues to be bound,
	 * instead of becoming free. Note that this is NOT how
	 * classical lambda calculus works!!
	 *
	 * If the original RewriteLink contains bound variables that
	 * are not mentioned in the map, these are untouched.
	 *
	 * If the RewriteLink is fully reduced, i.e. all variables have
	 * been beta-reduced, then the returned atom is still a
	 * RewriteLink, but with an empty variable declaration.
	 */
	virtual Handle beta_reduce(const HandleMap& vm) const;

	/**
	 * Like the above, but uses a sequence of values, presumed to be
	 * in the same order as the variable declarations. The number of
	 * values must match the number of variables, or there must be
	 * a single value that is eta-convertible and gives the right
	 * number of values.
	 */
	virtual Handle beta_reduce(const HandleSeq& values) const;

	/**
	 * Like the above, but accepting a sequence of values.
	 */
	HandleSeq beta_reduce_bodies(const Handle& nvardecl,
	                             const HandleMap& vm) const;

	/**
	 * Helper function, used by beta_reduce and the unifier.
	 *
	 * After substitution, remaining quotations might be useless or
	 * harmful, which might be the case if they deprive a nested
	 * RewriteLink from hiding supposedly hidden variables, consume
	 * them.
	 *
	 * Specifically this code makes 3 assumptions
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
	 *
	 * Examples:
	 *
	 * 1. Remove UnquoteLink wrapping around a closed term. Assuming
	 * the current link is
	 *
	 * Get
	 *   Variable "$X"
	 *   Quote
	 *     Evaluation
	 *       Unquote GroundedSchema "scm: dummy"
	 *       Unquote Variable "$X"
	 *
	 * then applying this function returns
	 *
	 * Get
	 *   Variable "$X"
	 *   Quote
	 *     Evaluation
	 *       GroundedSchema "scm: dummy"
	 *       Unquote Variable "$X"
	 */
	Handle consume_ill_quotations() const;
	static Handle consume_ill_quotations(const Handle& vardecl, const Handle& h,
	                                     /* Remember if some atom
	                                      * is the clause root of a
	                                      * pattern */
	                                     bool clause_root);
	static Handle consume_ill_quotations(const Variables& variables, const Handle& h,
	                                     bool clause_root);
	static Handle consume_ill_quotations(const Variables& variables, const Handle& h,
	                                     Quotation quotation,
	                                     bool clause_root);
	static Handle consume_ill_quotations(const Variables& variables, const Handle& h,
	                                     // TODO: we probably want to
	                                     // move quotation,
	                                     // needless_quotation,
	                                     // clause_root and more in
	                                     // its own class
	                                     Quotation quotation,
	                                     bool& needless_quotation,
	                                     bool clause_root);
	static HandleSeq consume_ill_quotations(const Variables& variables,
	                                        const HandleSeq& hs,
	                                        Quotation quotation,
	                                        bool& needless_quotation,
	                                        bool clause_root);

	static Handle factory(const Handle&);
};

static inline RewriteLinkPtr RewriteLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<RewriteLink>(h); }
static inline RewriteLinkPtr RewriteLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<RewriteLink>(a); }

#define createRewriteLink std::make_shared<RewriteLink>

/** @}*/
}

#endif // _OPENCOG_REWRITE_LINK_H
