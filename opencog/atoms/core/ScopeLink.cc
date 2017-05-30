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
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/LambdaLink.h>

#include "ScopeLink.h"

using namespace opencog;

void ScopeLink::init(void)
{
	extract_variables(_outgoing);
}

ScopeLink::ScopeLink(const Handle& vars, const Handle& body)
	: Link(HandleSeq({vars, body}), SCOPE_LINK)
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
	if (IMPLICATION_SCOPE_LINK == t) return true;
	if (PUT_LINK == t) return true;
	if (classserver().isA(t, PATTERN_LINK)) return true;
	return false;
}

ScopeLink::ScopeLink(Type t, const Handle& body)
	: Link(HandleSeq({body}), t)
{
	if (skip_init(t)) return;
	init();
}

ScopeLink::ScopeLink(const HandleSeq& oset, Type t)
	: Link(oset, t)
{
	if (skip_init(t)) return;
	init();
}

ScopeLink::ScopeLink(const Link &l)
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

	// If we trip over an unquote immediately, then we can assume that
	// the whole links appears in some quote context. This cannot be
	// treated as an ordinary ScopeLink in any way ... halt all further
	// initialization now.
	if (UNQUOTE_LINK == decls)
		return;

	// If the first atom is not explicitly a variable declaration, then
	// there are no variable declarations. There are two cases that can
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
bool ScopeLink::is_equal(const Handle& other, bool silent) const
{
	if (other == this) return true;
	if (other->getType() != _type) return false;

	ScopeLinkPtr scother(ScopeLinkCast(other));
	if (nullptr == scother)
		scother = createScopeLink(*LinkCast(other));

	// If the hashes are not equal, they can't possibly be equivalent.
	if (get_hash() != scother->get_hash()) return false;

	// Some derived classes (such as BindLink) have multiple body parts,
	// so it is not enough to compare this->_body to other->_body.
	// They tricky bit, below, is skipping over variable decls correctly,
	// to find the remaining body parts. Start by counting to make sure
	// that this and other have the same number of body parts.
	Arity vardecl_offset = _vardecl != Handle::UNDEFINED;
	Arity other_vardecl_offset = scother->_vardecl != Handle::UNDEFINED;
	Arity n_scoped_terms = getArity() - vardecl_offset;
	Arity other_n_scoped_terms = other->getArity() - other_vardecl_offset;
	if (n_scoped_terms != other_n_scoped_terms) return false;

	// Variable declarations must match.
	if (not _varlist.is_equal(scother->_varlist)) return false;

	// If all of the variable names are identical in this and other,
	// then no alpha conversion needs to be done; we can do a direct
	// comparison.
	if (_varlist.is_identical(scother->_varlist))
	{
		// Compare them, they should match.
		const HandleSeq& otho(other->getOutgoingSet());
		for (Arity i = 0; i < n_scoped_terms; ++i)
		{
			const Handle& h(_outgoing[i + vardecl_offset]);
			const Handle& other_h(otho[i + other_vardecl_offset]);
			if (h->operator!=(*((AtomPtr) other_h))) return false;
		}
		return true;
	}

	// If we are here, we need to perform alpha conversion to test
	// equality.  Other terms, with our variables in place of its
	// variables, should be same as our terms.
	for (Arity i = 0; i < n_scoped_terms; ++i)
	{
		Handle h = getOutgoingAtom(i + vardecl_offset);
		Handle other_h = other->getOutgoingAtom(i + other_vardecl_offset);
		other_h = scother->_varlist.substitute_nocheck(other_h,
		                                         _varlist.varseq, silent);
		// Compare them, they should match.
		if (*((AtomPtr)h) != *((AtomPtr) other_h)) return false;
	}

	return true;
}

/* ================================================================= */

/// A specialized hashing function, designed so that all alpha-
/// convertable links get exactly the same hash.  To acheive this,
/// the actual variable names have to be excluded from the hash,
/// and a standardized set used instead.
//
// There's a lot of prime-numbers in the code below, but the
// actual mixing and avalanching is extremely poor. I'm hoping
// its good enough for hash buckets, but have not verified.
//
// (In the code below, the numbers of the form `((1UL<<35) - 325)`
// etc. are all prime numbers. "Mixing" refers to code having the
// form `hash += (hash<<5) + other_stuff;` -- the shift and add
// mixes the bits. "Avalanching" refers to single-bit differences
// rapidly turning into multi-bit differences.)
//
// There's also an issue that there are multiple places where the
// hash must not mix, and must stay abelian, in order to deal with
// unordered links and alpha-conversion. (Here, "abelian" refers to
// order independence; addition is abelian; while "mixing" as
// defined above, is non-abelian).
//
ContentHash ScopeLink::compute_hash() const
{
	ContentHash hsh = ((1UL<<35) - 325) * getType();
	hsh += (hsh <<5) + ((1UL<<47) - 649) * _varlist.varseq.size();

	// It is not safe to mix here, since the sort order of the
	// typemaps will depend on the variable names. So must be
	// abelian.
	ContentHash vth = 0;
	for (const auto& pr : _varlist._simple_typemap)
	{
		for (Type t : pr.second) vth += ((1UL<<19) - 87) * t;
	}

	for (const auto& pr : _varlist._deep_typemap)
	{
		for (const Handle& th : pr.second) vth += th->get_hash();
	}
	hsh += (hsh <<5) + (vth % ((1UL<<27) - 235));

	Arity vardecl_offset = _vardecl != Handle::UNDEFINED;
	Arity n_scoped_terms = getArity() - vardecl_offset;

	UnorderedHandleSet hidden;
	for (Arity i = 0; i < n_scoped_terms; ++i)
	{
		const Handle& h(_outgoing[i + vardecl_offset]);
		hsh += (hsh<<5) + term_hash(h, hidden);
	}
	hsh %= (1UL << 63) - 409;

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1UL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	_content_hash = hsh;
	return _content_hash;
}

/// Recursive helper for computing the content hash correctly for
/// scoped links.  The algorithm here is almost identical to that
/// used in VarScraper::find_vars(), with obvious alterations.
ContentHash ScopeLink::term_hash(const Handle& h,
                                 UnorderedHandleSet& bound_vars,
                                 Quotation quotation) const
{
	Type t = h->getType();
	if ((VARIABLE_NODE == t or GLOB_NODE == t) and
	    quotation.is_unquoted() and
	    0 != _varlist.varset.count(h) and
	    0 == bound_vars.count(h))
	{
		// Alpha-convert the variable "name" to its unique position
		// in the sequence of bound vars.  Thus, the name is unique.
		return ((1UL<<24)-77) * (1 + _varlist.index.find(h)->second);
	}

	// Just the plain old hash for all other nodes.
	if (h->isNode()) return h->get_hash();

	// Quotation
	quotation.update(t);

	// Other embedded ScopeLinks might be hiding some of our variables...
	bool issco = classserver().isA(t, SCOPE_LINK);
	UnorderedHandleSet bsave;
	if (issco)
	{
		// Protect current hidden vars from harm.
		bsave = bound_vars;
		// Add the Scope link vars to the hidden set.
		ScopeLinkPtr sco(ScopeLinkCast(h));
		if (nullptr == sco)
			sco = ScopeLinkCast(classserver().factory(h));
		const Variables& vees = sco->get_variables();
		for (const Handle& v : vees.varseq) bound_vars.insert(v);
	}

	// Prevent mixing for UnorderedLinks. The `mixer` var will be zero
	// for UnorderedLinks. The problem is that two UnorderdLinks might
	// be alpha-equivalent, but have their atoms presented in a
	// different order. Thus, the hash must be computed in a purely
	// commutative fashion: using only addition, so as to never create
	// any entropy, until the end.
	//
	// XXX As discussed in issue #1176, a better fix would be to
	// compute the individual term_hashes first, then sort them,
	// and then mix them!  This provides the desired qualities:
	// different unordered links can be directly compared, and also
	// have good mixing/avalanching properties. The code below
	// only allows for compare; it fails to mix.
	//
	bool is_ordered = not classserver().isA(t, UNORDERED_LINK);
	ContentHash mixer = (ContentHash) is_ordered;
	ContentHash hsh = ((1UL<<8) - 59) * t;
	for (const Handle& ho: h->getOutgoingSet())
	{
		hsh += mixer * (hsh<<5) + term_hash(ho, bound_vars, quotation);
	}
	hsh %= (1UL<<63) - 471;

	// Restore saved vars from stack.
	if (issco) bound_vars = bsave;

	return hsh;
}

/* ================================================================= */

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

Handle ScopeLink::alpha_conversion(HandleSeq vars) const
{
	// If hs is empty then generate new variable names
	if (vars.empty())
		vars = append_rand_str(_varlist.varseq);

	// Perform alpha conversion
	HandleSeq hs;
	for (size_t i = 0; i < getArity(); ++i)
		hs.push_back(_varlist.substitute_nocheck(getOutgoingAtom(i), vars));

	// Create the alpha converted scope link
	return classserver().factory(Handle(createLink(hs, getType())));
}

/* ================================================================= */

bool ScopeLink::operator==(const Atom& ac) const
{
	Atom& a = (Atom&) ac; // cast away constness, for smart ptr.
	try {
		return is_equal(a.getHandle(), true);
	} catch (const NestingException& ex) {}
	return false;
}

bool ScopeLink::operator!=(const Atom& a) const
{
	return not operator==(a);
}

DEFINE_LINK_FACTORY(ScopeLink, SCOPE_LINK);

/* ===================== END OF FILE ===================== */
