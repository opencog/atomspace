/*
 * ScopeLink.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string>

#include <opencog/util/mt19937ar.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/FreeLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/core/ImplicationLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atomutils/TypeUtils.h>


#include "ScopeLink.h"

using namespace opencog;

void ScopeLink::init(void)
{
	extract_variables(_outgoing);
}

ScopeLink::ScopeLink(const HandleSeq& oset,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(SCOPE_LINK, oset, tv, av)
{
	init();
}

ScopeLink::ScopeLink(const Handle& vars, const Handle& body,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(SCOPE_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

bool ScopeLink::skip_init(Type t)
{
	// Type must be as expected.
	if (not classserver().isA(t, SCOPE_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ScopeLink, got %s", tname.c_str());
	}

	// Certain derived classes want to have a different initialization
	// sequence. We can't use virtual init() in the ctor, so just
	// do an if-statement here.
	if (PUT_LINK == t) return true;
	if (classserver().isA(t, PATTERN_LINK)) return true;
	return false;
}

ScopeLink::ScopeLink(Type t, const Handle& body,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, HandleSeq({body}), tv, av)
{
	if (skip_init(t)) return;
	init();
}

ScopeLink::ScopeLink(Type t, const HandleSeq& oset,
                     TruthValuePtr tv, AttentionValuePtr av)
	: Link(t, oset, tv, av)
{
	if (skip_init(t)) return;
	init();
}

ScopeLink::ScopeLink(Link &l)
	: Link(l)
{
	if (skip_init(l.getType())) return;
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
void ScopeLink::extract_variables(const HandleSeq& oset)
{
	if (oset.size() == 0)
		throw SyntaxException(TRACE_INFO,
			"Expecting a non-empty outgoing set.");

	Type decls = oset.at(0)->getType();

	// If the first atom is not explicitly a variable declaration, then
	// there are no variable declarations. There are two cases that; can
	// apply here: either the body is a lambda, in which case, we copy
	// the variables from the lambda; else we extract all free variables.
	if (VARIABLE_LIST != decls and
	    VARIABLE_NODE != decls and
	    TYPED_VARIABLE_LINK != decls and
	    GLOB_NODE != decls)
	{
		_body = oset[0];

		if (classserver().isA(_body->getType(), LAMBDA_LINK))
		{
			LambdaLinkPtr lam(LambdaLinkCast(_body));
			if (nullptr == lam)
				lam = createLambdaLink(*LinkCast(_body));
			_varlist = lam->get_variables();
			_body = lam->get_body();
		}
		else
		{
			_varlist.find_variables(oset[0]);
		}
		return;
	}

	if (oset.size() < 2)
		throw SyntaxException(TRACE_INFO,
			"Expecting an outgoing set size of at least two; got %s",
			oset[0]->toString().c_str());

	// If we are here, then the first outgoing set member should be
	// a variable declaration.
	_vardecl = oset[0];
	_body = oset[1];

	// Initialize _varlist with the scoped variables
	init_scoped_variables(_vardecl);
}

/* ================================================================= */
///
/// Initialize _varlist given a handle of either VariableList or a
/// variable.
///
void ScopeLink::init_scoped_variables(const Handle& hvar)
{
	// Use the VariableList class as a tool to extract the variables
	// for us.
	VariableList vl(hvar);
	_varlist = vl.get_variables();
}

/* ================================================================= */
///
/// Compare other ScopeLink, return true if it is equal to this one,
/// up to an alpha-conversion of variables.
///
bool ScopeLink::is_equal(const Handle& other) const
{
	if (other == this) return true;
	if (other->getType() != _type) return false;

	ScopeLinkPtr scother(ScopeLinkCast(other));

	// In case we're dealing with a class inheriting from ScopeLink,
	// like BindLink, that has more than one scoped term, like
	// implicand, etc, then we need to check the alpha equivalence
	// over all terms. Before that, let's check that they have the
	// same number of terms.
	Arity vardecl_offset = _vardecl != Handle::UNDEFINED;
	Arity other_vardecl_offset = scother->_vardecl != Handle::UNDEFINED;
	Arity n_scoped_terms = getArity() - vardecl_offset;
	Arity other_n_scoped_terms = other->getArity() - other_vardecl_offset;
	if (n_scoped_terms != other_n_scoped_terms) return false;

	// Variable declarations must match.
	if (scother == nullptr or not _varlist.is_equal(scother->_varlist))
		return false;

	// Other terms, with our variables in place of its variables,
	// should be same as our terms.
	for (Arity i = 0; i < n_scoped_terms; ++i) {
		Handle h = getOutgoingAtom(i + vardecl_offset);
		Handle other_h = other->getOutgoingAtom(i + other_vardecl_offset);
		other_h = scother->_varlist.substitute_nocheck(other_h, _varlist.varseq);
 		// Compare them, they should match.
 		if (*((AtomPtr)h) != *((AtomPtr) other_h)) return false;
 	}

	return true;
}

inline std::string rand_hex_str()
{
	int rnd_id = randGen().randint();
	std::stringstream ss;
	ss << std::hex << rnd_id;
	return ss.str();
}

inline HandleSeq append_rand_str(const HandleSeq& vars)
{
	HandleSeq new_vars;
	for (const Handle& h : vars) {
		std::string new_var_name = h->getName() + "-" + rand_hex_str();
		new_vars.emplace_back(createNode(VARIABLE_NODE, new_var_name));
	}
	return new_vars;
}

Handle ScopeLink::alpha_conversion(HandleSeq vars, Handle vardecl) const
{
	// If hs is empty then generate new variable names
	if (vars.empty())
		vars = append_rand_str(_varlist.varseq);

	// Perform alpha conversion
	HandleSeq hs;
	for (size_t i = 0; i < getArity(); ++i)
		hs.push_back(_varlist.substitute_nocheck(getOutgoingAtom(i), vars));

	// Replace vardecl by the substituted version if any
	if (vardecl.is_undefined() and _vardecl.is_defined())
		vardecl = hs[0];

	// Remove the optional variable declaration from hs
	if (_vardecl.is_defined())
		hs.erase(hs.begin());

	// Filter vardecl
	vardecl = filter_vardecl(vardecl, hs);

	// Insert vardecl back in hs if defined
	if (vardecl.is_defined())
		hs.insert(hs.begin(), vardecl);

	// Create the alpha converted scope link
	return Handle(factory(getType(), hs));
}

bool ScopeLink::operator==(const Atom& ac) const
{
	Atom& a = (Atom&) ac; // cast away constness, for smart ptr.
	return is_equal(a.getHandle());
}

bool ScopeLink::operator!=(const Atom& a) const
{
	return not operator==(a);
}

ScopeLinkPtr ScopeLink::factory(const Handle& h)
{
	return factory(h->getType(), h->getOutgoingSet());
}

ScopeLinkPtr ScopeLink::factory(Type t, const HandleSeq& seq)
{
	if (PUT_LINK == t)
		return createPutLink(seq);

	if (LAMBDA_LINK == t)
		return createLambdaLink(seq);

	if (classserver().isA(t, IMPLICATION_LINK))
		return createImplicationLink(t, seq);

	if (classserver().isA(t, PATTERN_LINK))
		return PatternLink::factory(t, seq);

	if (classserver().isA(t, SCOPE_LINK))
		return createScopeLink(t, seq);

	throw SyntaxException(TRACE_INFO,
		"ScopeLink is not a factory for %s",
		classserver().getTypeName(t).c_str());
}

/* ===================== END OF FILE ===================== */
