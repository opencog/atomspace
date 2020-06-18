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
	 * were done.  If the map held a constant argument for a variable,
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
	 * variable->argument, then a normal beta reduction is done. If
	 * the maps specifies variable->variable, then an alpha renaming
	 * is done.
	 *
	 * If there are any poorly-formed (ill-formed) quotations,
	 * these are removed.
	 */
	HandleSeq substitute_bodies(const Handle& nvardecl,
	                            const HandleMap& vm) const;

	/**
	 * Like above but uses a mapping from variables to arguments instead
	 * of a sequence of arguments.
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
	static bool is_scope_bound_to_ancestor(const Variables& variables,
	                                       const Handle& h);

	/**
	 * Return true if the type if the type is AND_LINK, OR_LINK or
	 * NOT_LINK, as when used at the root of the pattern body these
	 * links act as logical connectors.
	 */
	static bool is_logical_connector(Type);
	static bool is_logical_connector(const Handle&);

public:
	RewriteLink(const HandleSeq&&, Type=REWRITE_LINK);
	RewriteLink(const Handle& varcdecls, const Handle& body);
	RewriteLink(const RewriteLink &) = delete;
	RewriteLink& operator=(const RewriteLink &) = delete;

	void make_silent(bool s) { _silent = s; }

	/**
	 * Perform a beta-reduction and optional alpha-conversion,
	 * returning the reduced RewriteLink.
	 *
	 * If the map specifies a variable->argument, then a standard
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
	 * Like the above, but uses a sequence of arguments, presumed to be
	 * in the same order as the variable declarations. The number of
	 * arguments must match the number of variables, or there must be
	 * a single argument that is eta-convertible and gives the right
	 * number of arguments.
	 */
	virtual Handle beta_reduce(const HandleSeq& arguments) const;

	/**
	 * Like the above, but accepting a sequence of arguments.
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
	 * Specifically this code does the following
	 *
	 * 1. Remove needless quotations in front closed term, for instance
	 *
	 * (QuoteLink
	 *   (EvaluationLink
	 *     (Unquote (GroundedPredicate "gpn"))
	 *     (Unquote (Variable "$X"))))
	 *
	 * The Unquote in front of the "gpn" is useless so it removed,
	 * resulting into
	 *
	 * (QuoteLink
	 *   (EvaluationLink
	 *     (GroundedPredicate "gpn")
	 *     (Unquote (Variable "$X"))))
	 *
	 * 2. Remove obvious involutions such as
	 *
	 * (Quote
	 *   (Put
	 *     (Unquote (Quote (Lambda (Variable "$X") (Variable "$X"))))
	 *     (Concept "A")))
	 *
	 * (Unquote (Quote ... is an involution so remove it, resulting into
	 *
	 * (Quote
	 *   (Put
	 *     (Lambda (Variable "$X") (Variable "$X"))
	 *     (Concept "A")))
	 *
	 * Note that the root Quote cannot be removed otherwise it would
	 * change the semantics of the hypergraph (as the quotation
	 * prevents the PutLink from being executing).
	 *
	 * 3. Remove quotations around a fully substituted scope, for
	 * instance
	 *
	 * (Quote
	 *   (Lambda
	 *     (Unquote (TypedVariable (Variable "$X") (Type "ConceptNode")))
	 *     (Unquote (And (Concept "A") (Variable "$X")))))
	 *
	 * all quotations can be remove (in fact keeping them would be
	 * harmful because the variable $X would be interpreted as not
	 * being bound to this Lambda, to make sure it is should be bound
	 * to the lambda as opposed to be bound to a parent scope, a
	 * Variables object is passed in argument representing the
	 * variable declaration of that parent scope. Thus, assuming $X
	 * isn't in the variables object, the above would result into
	 *
	 * (Lambda
	 *   (TypedVariable (Variable "$X") (Type "ConceptNode"))
	 *   (And (Concept "A") (Variable "$X")))
	 */
	Handle consume_quotations() const;
	static Handle consume_quotations(const Variables& variables, const Handle& h,
	                                 // TODO: we probably want to
	                                 // move quotation,
	                                 // needless_quotation,
	                                 // clause_root and more in
	                                 // its own class
	                                 Quotation quotation,
	                                 bool& needless_quotation,
	                                 bool clause_root);
	static HandleSeq consume_quotations(const Variables& variables,
	                                    const HandleSeq& hs,
	                                    Quotation quotation,
	                                    bool& needless_quotation,
	                                    bool clause_root);
	static Handle consume_quotations_mere_rec(const Variables& variables,
	                                          const Handle& h,
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
