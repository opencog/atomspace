/*
 * opencog/atoms/pattern/BindLink.h
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
#ifndef _OPENCOG_BIND_LINK_H
#define _OPENCOG_BIND_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class BindLink : public PatternLink
{
protected:
	void init(void);

	/// The rewrite term
	Handle _implicand;

	// Overwrite PatternLink::extract_variables as BindLink has one
	// more outgoing for the rewrite rule. In addition this method
	// will initialize the rewrite term _implicand.
	void extract_variables(const HandleSeq& oset);

public:
	BindLink(const HandleSeq&, Type=BIND_LINK);
	BindLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
	BindLink(const Handle& body, const Handle& rewrite);
	explicit BindLink(const Link &l);

	bool imply(PatternMatchCallback&, bool check_connectivity=true);
	const Handle& get_implicand(void) { return _implicand; }

	/**
	 * Given a mapping from variables to values, return a copy of
	 * itself with variables substituted by the values. Values could
	 * be variable as well. The variable declaration is automatically
	 * adjusted so only the new variables remain. Optionally, if the
	 * types have changed, a new variable declaration is provided to
	 * replace the existing one. Constant clauses are automatically
	 * removed from the BindLink. If no clause remains then the
	 * pattern body is left with an empty AndLink.
	 *
	 * Examples:
	 *
	 * Assume the instance is:
	 *
	 * (BindLink
	 *   (VariableList (Variable "$X") (Variable "$Y"))
	 *   (Inheritance (Variable "$X") (Variable "$Y"))
	 *   (ExecutionOutputLink
	 *     (GroundedSchemaNode "gsn")
	 *     (Inheritance (Variable "$X") (Variable "$Y"))))
	 *
	 * 1. substitute([(Variable "$W"), (Variable "$Z")]) returns:
	 *
	 * (BindLink
	 *   (VariableList (Variable "$W") (Variable "$Z"))
	 *   (Inheritance (Variable "$W") (Variable "$Z"))
	 *   (ExecutionOutputLink
	 *     (GroundedSchemaNode "gsn")
	 *     (Inheritance (Variable "$W") (Variable "$Z"))))
	 *
	 * 2. substitute([(Variable "$W"), (Variable "$Z")], variables)
	 *    such that variables associates a ConceptNode type to $W and $Z
	 *    returns:
	 *
	 * (BindLink
	 *   (VariableList
	 *     (TypedVariable (Variable "$W") (Type "ConceptNode"))
	 *     (TypedVariable (Variable "$Z") (Type "ConceptNode")))
	 *   (Inheritance (Variable "$W") (Variable "$Z")))
	 *   (ExecutionOutputLink
	 *     (GroundedSchemaNode "gsn")
	 *     (Inheritance (Variable "$W") (Variable "$Z"))))
	 *
	 * 3. substitute([(Variable "$W"), (Concept "B")]) returns:
	 *
	 * (BindLink
	 *   (Variable "$W")
	 *   (Inheritance (Variable "$W") (Concept "B"))
	 *   (ExecutionOutputLink
	 *     (GroundedSchemaNode "gsn")
	 *     (Inheritance (Variable "$W") (Concept "B"))))
	 *
	 * 4. substitute([(Concept "A"), (Concept "B")]) returns:
	 *
	 * (BindLink
	 *   (AndLink)
	 *   (ExecutionOutputLink
	 *     (GroundedSchemaNode "gsn")
	 *     (Inheritance (Concept "$A") (Concept "B"))))
	 */
	Handle substitute(const HandleMap& var2val,
	                  Handle vardecl=Handle::UNDEFINED) const;

	static Handle factory(const Handle&);

private:
	// Helper of substitute
	Handle substitute_vardecl(const Handle& vardecl,
	                          const HandleMap& var2val) const;
};

typedef std::shared_ptr<BindLink> BindLinkPtr;
static inline BindLinkPtr BindLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<BindLink>(a); }
static inline BindLinkPtr BindLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<BindLink>(a); }

// XXX temporary hack ...
#define createBindLink std::make_shared<BindLink>

/** @}*/
}

#endif // _OPENCOG_BIND_LINK_H
