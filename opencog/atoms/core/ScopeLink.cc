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

// #include <opencog/util/Logger.h>
#include <opencog/util/random.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/hash.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/VariableSet.h>

#include "ScopeLink.h"

using namespace opencog;

void ScopeLink::init(void)
{
	// If unquoted_below() returnes true, that means we are quoted,
	// and so nothing to be done. Skip variable extraction.
	if (unquoted_below(_outgoing)) return;
	extract_variables(_outgoing);
}

ScopeLink::ScopeLink(const Handle& vars, const Handle& body)
	: Link({vars, body}, SCOPE_LINK)
{
	init();
}

bool ScopeLink::skip_init(Type t)
{
	// Type must be as expected.
#if 0
	// ScopeLinks are created directly in unit tests, so this safety
	// check won't work.
	if (SCOPE_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"ScopeLinks are private and cannot be instantiated.");
#endif
	if (not nameserver().isA(t, SCOPE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a ScopeLink, got %s", tname.c_str());
	}

	// Certain derived classes want to have a different initialization
	// sequence. We can't use virtual init() in the ctor, so just
	// do an if-statement here.
	if (IMPLICATION_SCOPE_LINK == t) return true;
	if (PUT_LINK == t) return true;
	if (PREDICATE_FORMULA_LINK == t) return true;
	if (nameserver().isA(t, PATTERN_LINK)) return true;
	return false;
}

ScopeLink::ScopeLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (skip_init(t)) return;
	init();
}

/* ================================================================= */
///
/// Find and unpack variable declarations, if any; otherwise, just
/// find all free variables.
///
void ScopeLink::extract_variables(const HandleSeq& oset)
{
	size_t sz = oset.size();
	if (0 == sz)
		throw SyntaxException(TRACE_INFO,
			"Expecting an outgoing set size of at least one; got %s",
			to_short_string().c_str());

	Type decls = oset.at(0)->get_type();

	// If we trip over an unquote immediately, then we can assume that
	// the whole link appears in some quote context. This cannot be
	// treated as an ordinary ScopeLink in any way ... halt all further
	// initialization now.
	if (UNQUOTE_LINK == decls)
		return;

	// If the first atom is not explicitly a variable declaration, then
	// there are no prenex-order variable declarations. (There might
	// still be in-line variable declarations; these are allowed for
	// some link types, e.g. the various query links.) Anyway, handle
	// one of two cases: either the body is a lambda, in which case,
	// we copy the variables from the lambda; else we extract all free
	// variables.
	if (VARIABLE_LIST != decls and VARIABLE_SET != decls and
	    // A VariableNode could a be valid body, if it has no variable
	    // declaration, that is if the Scope has only one argument.
	    (VARIABLE_NODE != decls or oset.size() == 1) and
	    TYPED_VARIABLE_LINK != decls and
	    ANCHOR_NODE != decls and
	    GLOB_NODE != decls)
	{
		_body = oset[0];

		if (nameserver().isA(_body->get_type(), LAMBDA_LINK))
		{
			LambdaLinkPtr lam(LambdaLinkCast(_body));
			_variables = lam->get_variables();
			_body = lam->get_body();
		}
		else
		{
			_variables.find_variables(oset[0]);
		}
		return;
	}


	// If we are here, then the first outgoing set member should be
	// a variable declaration. JoinLinks need not have a body.
	_vardecl = oset[0];

	if (2 <= sz)
	{
		_body = oset[1];

		// If the user is using an AnchorNode, but not otherwise specifying
		// variables, we have to fish them out of the body.
		if (ANCHOR_NODE == decls)
		{
			_variables.find_variables(_body);
			return;
		}
	}

	// Initialize _variables with the scoped variables
	init_scoped_variables(_vardecl);
}

/* ================================================================= */
///
/// Initialize _variables given a handle representing a variable
/// declaration.
///
void ScopeLink::init_scoped_variables(const Handle& vardecl)
{
	_variables = Variables(vardecl);
	if (vardecl->get_type() == VARIABLE_SET) {
		// Outgoing set without variable declaration
		HandleSeq owv(std::next(_outgoing.begin()), _outgoing.end());
		_variables.canonical_sort(owv);
	}
}

/* ================================================================= */

inline Handle append_rand_str(const Handle& var)
{
	std::string new_var_name = randstr(var->get_name() + "-");
	return createNode(var->get_type(), std::move(new_var_name));
}

inline HandleSeq append_rand_str(const HandleSeq& vars)
{
	HandleSeq new_vars;
	for (const Handle& h : vars)
		new_vars.push_back(append_rand_str(h));
	return new_vars;
}

/**
 * Wrap every glob node with a ListLink
 *
 * Since GlobNodes can be matched/substituted with one or more
 * arguments, The arguments are expected to be wrapped with
 * ListLink.
 * In case of alpha conversion, Alpha converted GlobNodes in a
 * program needs to be wrapped before passed to substitute the Glob.
 */
inline HandleSeq wrap_glob_with_list(const HandleSeq& vars)
{
	HandleSeq new_vars;
	for (const Handle& var : vars) {
		if (GLOB_NODE == var->get_type())
			new_vars.push_back(createLink(HandleSeq{var}, LIST_LINK));
		else new_vars.push_back(var);
	}
	return new_vars;
}

Handle ScopeLink::alpha_convert() const
{
	HandleSeq vars = append_rand_str(_variables.varseq);
	return alpha_convert(vars);
}

Handle ScopeLink::alpha_convert(const HandleSeq& vars) const
{
	const auto wrapped = wrap_glob_with_list(vars);
	// Perform alpha conversion
	HandleSeq hs;
	for (size_t i = 0; i < get_arity(); ++i)
		hs.push_back(_variables.substitute_nocheck(getOutgoingAtom(i), wrapped));

	// Create the alpha converted scope link
	return createLink(std::move(hs), get_type());
}

Handle ScopeLink::alpha_convert(const HandleMap& vsmap) const
{
	HandleSeq vars;
	for (const Handle& var : _variables.varseq) {
		auto it = vsmap.find(var);
		vars.push_back(it == vsmap.end() ? append_rand_str(var) : it->second);
	}
	return alpha_convert(vars);
}

/* ================================================================= */
///
/// Compare other ScopeLink, return true if it is equal to this one,
/// up to an alpha-conversion of variables.
///
bool ScopeLink::is_equal(const Handle& other, bool silent) const
{
	if (other == this) return true;
	if (other->get_type() != _type) return false;

	ScopeLinkPtr scother(ScopeLinkCast(other));

	// If the hashes are not equal, they can't possibly be equivalent.
	if (get_hash() != scother->get_hash()) return false;

	// Some derived classes (such as BindLink) have multiple body parts,
	// so it is not enough to compare this->_body to other->_body.
	// The tricky bit, below, is skipping over variable decls correctly,
	// to find the remaining body parts. Start by counting to make sure
	// that this and other have the same number of body parts.
	Arity vardecl_offset = _vardecl != Handle::UNDEFINED;
	Arity other_vardecl_offset = scother->_vardecl != Handle::UNDEFINED;
	Arity n_scoped_terms = get_arity() - vardecl_offset;
	Arity other_n_scoped_terms = other->get_arity() - other_vardecl_offset;
	if (n_scoped_terms != other_n_scoped_terms) return false;

	// Variable declarations must match.
	if (not _variables.is_equal(scother->_variables)) return false;

	// If all of the variable names are identical in this and other,
	// then no alpha conversion needs to be done; we can do a direct
	// comparison.
	if (_variables.is_identical(scother->_variables))
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
		other_h = scother->_variables.substitute_nocheck(other_h,
		                                                 _variables.varseq, silent);
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
// "Mixing" refers to the idea of combining together two values, such
// that thier bits are mixed together (in the formal, mathematical
// definition, which includes ideas about increasing entropy).  Hash
// functions are designed to be very good at mixing.
//
// There are multiple places where the combination of two hash values
// must not mix, and must stay order-independent (i.e. abelian). This
// is needed to deal with unordered links and alpha-conversion.
// Addition is "abelian": A+B = B+A while most functions that mix are
// non-abalian -- the result depends on the order of the operations.

ContentHash ScopeLink::compute_hash() const
{
	// If there are no variables, it's likely that we are quoted,
	// and so nothing to be done. Skip the complicated computations.
	if (0 == _variables.varseq.size())
		return Link::compute_hash();

	return scope_hash(_variables.index);
}

ContentHash ScopeLink::scope_hash(const FreeVariables::IndexMap& index) const
{
	ContentHash hsh = get_fvna_offset<sizeof(ContentHash)>();
	fnv1a_hash(hsh, get_type());
	fnv1a_hash(hsh, _variables.varseq.size());

	// It is not safe to mix here, since the sort order of the
	// typemaps will depend on the variable names. So must be
	// abelian. That is, we must use addition.
	ContentHash vth = 0;
	for (const auto& pr : _variables._typemap)
	{
		// Semantic equivalance: an untyped variable is
		// equivalent to a typed variable of type "ATOM".
		if (pr.second->is_untyped()) continue;

		vth += pr.second->get_typedecl()->get_hash();
	}
	fnv1a_hash(hsh, vth);

	// As to not mix together VariableList and VariableSet
	fnv1a_hash(hsh, _variables._ordered);

	// If there's an AnchorNode, hash that too.
	if (_variables._anchor)
		fnv1a_hash(hsh, _variables._anchor->get_hash());

	Arity vardecl_offset = _vardecl != Handle::UNDEFINED;
	Arity n_scoped_terms = get_arity() - vardecl_offset;
	UnorderedHandleSet hidden;
	for (Arity i = 0; i < n_scoped_terms; ++i)
	{
		const Handle& h(_outgoing[i + vardecl_offset]);
		fnv1a_hash(hsh, term_hash(h, index));
	}

	// Links will always have the MSB set.
	ContentHash mask = ((ContentHash) 1UL) << (8*sizeof(ContentHash) - 1);
	hsh |= mask;

	if (Handle::INVALID_HASH == hsh) hsh -= 1;
	return hsh;
}

/// Recursive helper for computing the content hash correctly for
/// scoped links.  The algorithm here is almost identical to that
/// used in VarScraper::find_vars(), with obvious alterations.
ContentHash ScopeLink::term_hash(const Handle& h,
                                 const FreeVariables::IndexMap& index,
                                 Quotation quotation) const
{
	Type t = h->get_type();
	if ((VARIABLE_NODE == t or GLOB_NODE == t) and quotation.is_unquoted())
	{
		auto it = index.find(h);
		if (it != index.end())
		{
			// Alpha-convert the variable "name" to its unique position
			// in the sequence of bound vars.  Thus, the name is unique.
			ContentHash hsh = get_fvna_offset<sizeof(ContentHash)>();
			fnv1a_hash(hsh, 1 + it->second);
			return hsh;
		}
		// Otherwise treat that variable as a constant, i.e. move on
	}

	// Just the plain old hash for all other nodes.
	if (h->is_node()) return h->get_hash();

	// If it is a scope, call specialized scope hash with the index
	// complemented with the new variables of that scope, hiding the
	// old ones if necessary.  This fixes an issue regarding
	// alpha-equivalence of scopes containing inner scopes, see
	// https://github.com/opencog/atomspace/issues/2507
	if (nameserver().isA(t, SCOPE_LINK) and quotation.is_unquoted())
	{
		FreeVariables::IndexMap new_index(index);
		ScopeLinkPtr sco(ScopeLinkCast(h));
		const Variables& sco_vars = sco->get_variables();
		const FreeVariables::IndexMap& sco_index = sco_vars.index;
		for (const auto& vi : sco_index)
			new_index[vi.first] = vi.second + index.size();
		return sco->scope_hash(new_index);
	}

	// Otherwise h is a regular link, recursively calculate its hash

	// Quotation
	quotation.update(t);

	// As discussed in issue #1176, compute the individual term_hashes
	// first, then sort them, and then mix them!  This provides the
	// desired qualities: different unordered links can be directly
	// compared, and also have good mixing/avalanching properties.
	std::vector<ContentHash> hash_vec;
	for (const Handle& ho: h->getOutgoingSet())
	{
		hash_vec.push_back(term_hash(ho, index, quotation));
	}

	// hash_vec should be sorted only for unordered links
	if (h->is_unordered_link()) {
		std::sort(hash_vec.begin(), hash_vec.end());
	}

	ContentHash hsh = get_fvna_offset<sizeof(ContentHash)>();
	fnv1a_hash(hsh, t);
	for (ContentHash & t_hash: hash_vec) {
		fnv1a_hash(hsh, t_hash);
	}

	return hsh;
}

/* ================================================================= */

bool ScopeLink::operator==(const Atom& ac) const
{
	Atom& a = (Atom&) ac; // cast away constness, for smart ptr.
	try {
		return is_equal(a.get_handle(), true);
	} catch (const NestingException& ex) {}
	return false;
}

bool ScopeLink::operator!=(const Atom& a) const
{
	return not operator==(a);
}

DEFINE_LINK_FACTORY(ScopeLink, SCOPE_LINK);

/* ===================== END OF FILE ===================== */
