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
/// format, and unpack the variable declarations, if present; it will
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
	Variables _variables;

	bool _quoted;

	void init(void);
	void extract_variables(const HandleSeq& oset);
	void init_scoped_variables(const Handle& vardecl);

	bool skip_init(Type);
	virtual ContentHash compute_hash() const;
	ContentHash scope_hash(const FreeVariables::IndexMap& index) const;
	ContentHash term_hash(const Handle&,
	                      const FreeVariables::IndexMap& index,
	                      Quotation quotation = Quotation()) const;

public:
	ScopeLink(const HandleSeq&&, Type=SCOPE_LINK);
	ScopeLink(const Handle& varcdecls, const Handle& body);
	ScopeLink(const ScopeLink&) = delete;
	ScopeLink& operator=(const ScopeLink&) = delete;

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _variables; }
	const Handle& get_vardecl(void) const { return _vardecl; }
	const Handle& get_body(void) const { return _body; }

	// Remove any variables that do NOT appear anywhere in the provided
	// HandleSeq. This can be used to clean up vardecls, when variables
	// are declared but then never used (thus making them impossible to
	// ground, or having other nasty side-effects.)
	void trim(const HandleSeq&);

	// Return an alpha-converted copy of this atom. Optionally, new
	// variable names can be provided. If none are provided, then new
	// randomly generated names are created.
	//
	// Warning: the AtomSpace treats all alpha-convertible atoms as
	// identical; if the new copy is inserted into the atomspace, the
	// original version will be returned.  Alpha-converted atoms can
	// only be used outside of the AtomSpace, for temporary operations.
	Handle alpha_convert() const;
	Handle alpha_convert(const HandleSeq& vars) const;

	// Like the above, but using a mapping from old variable names to
	// new variable names. If an existing variable doesn't have a
	// mapping specified, then a new random name is generated.
	Handle alpha_convert(const HandleMap& vsmap) const;

	// Return true if the other Handle is equal to this one,
	// i.e. is the same, up to alpha conversion. i.e. is the same,
	// up to a renaming of the bound variables.
	bool is_equal(const Handle&, bool silent=false) const;

	// Overload equality check!
	virtual bool operator==(const Atom&) const;
	virtual bool operator!=(const Atom&) const;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ScopeLink)
#define createScopeLink CREATE_DECL(ScopeLink)

/** @}*/
}

#endif // _OPENCOG_SCOPE_LINK_H
