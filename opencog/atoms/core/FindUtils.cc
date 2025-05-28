/**
 * FindUtils.cc
 *
 * Utilities for finding atoms in trees.
 *
 * Copyright (C) 2009, 2014, 2015 Opencog Foundation
 * All Rights Reserved
 *
 * Authors: Linas Vepstas <linasvepstas@gmail.com>
 *          Nil Geisweiller
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
 *
 * Created by Linas Vepstas February 2008
 */

#include <opencog/util/oc_assert.h>
#include "FindUtils.h"

namespace opencog {

FindAtoms::FindAtoms(Type t, bool subclass)
	: _target_types({t})
{
	if (not subclass) return;
	nameserver().getChildrenRecursive(t,
		std::inserter(_target_types, _target_types.end()));
}

FindAtoms::FindAtoms(Type ta, Type tb, bool subclass)
	: FindAtoms(ta, subclass)
{
	if (not subclass) return;
	nameserver().getChildrenRecursive(tb,
		std::inserter(_target_types, _target_types.end()));
}

FindAtoms::FindAtoms(const Handle& atom)
	: _target_types(),
	  _target_atoms({atom})
{}

FindAtoms::FindAtoms(const HandleSet& selection)
	: _target_types(),
	  _target_atoms(selection)
{}

void FindAtoms::search_set(const Handle& h)
{
	find_rec(h);
}

void FindAtoms::search_set(const HandleSeq& hlist)
{
	for (const Handle& h : hlist) find_rec(h);
}

FindAtoms::Loco FindAtoms::find_rec(const Handle& h, Quotation quotation)
{
	Type t = h->get_type();
	if (quotation.is_unquoted() and
	    (1 == _target_types.count(t) or _target_atoms.count(h) == 1))
	{
		varset.insert(h);
		return IMM; //! Don't explore link-typed vars!
	}

	quotation.update(t);

	for (Type stopper : stopset)
	{
		if (nameserver().isA(t, stopper)) return NOPE;
	}

	if (h->is_link())
	{
		bool held = false;
		bool imm = false;
		for (const Handle& oh : h->getOutgoingSet())
		{
			Loco where = find_rec(oh, quotation);
			if (NOPE != where) held = true;
			if (IMM == where) imm = true;
		}
		if (imm) least_holders.insert(h);
		if (held)
		{
			holders.insert(h);
			return YEP;
		}
	}
	return NOPE;
}

bool is_atom_in_tree(const Handle& tree, const Handle& atom)
{
	if (tree == atom) return true;
	if (not tree->is_link()) return false;

	// Recurse downwards...
	for (const Handle& h: tree->getOutgoingSet()) {
		if (is_atom_in_tree(h, atom)) return true;
	}
	return false;
}

bool is_quoted_in_tree(const Handle& tree, const Handle& atom)
{
	return max_quotation_level(tree, atom) > 0;
}

bool is_unquoted_in_tree(const Handle& tree, const Handle& atom)
{
	return min_quotation_level(tree, atom) < 1;
}

int min_quotation_level(const Handle& tree,
                        const Handle& atom,
                        Quotation quotation)
{
	// Base case
	if (content_eq(tree, atom)) return quotation.level();
	if (not tree->is_link()) return std::numeric_limits<int>::max();

	// Recursive case
	quotation.update(tree->get_type());
	int result = std::numeric_limits<int>::max();
	for (const Handle& h : tree->getOutgoingSet())
		result = std::min(result, min_quotation_level(h, atom, quotation));
	return result;
}

int max_quotation_level(const Handle& tree,
                        const Handle& atom,
                        Quotation quotation)
{
	// Base case
	if (tree == atom) return quotation.level();
	if (not tree->is_link()) return std::numeric_limits<int>::min();

	// Recursive case
	quotation.update(tree->get_type());
	int result = std::numeric_limits<int>::min();
	for (const Handle& h : tree->getOutgoingSet())
		result = std::max(result, max_quotation_level(h, atom, quotation));
	return result;
}

bool is_unscoped_in_tree(const Handle& tree, const Handle& atom)
{
	// Base cases
	if (content_eq(tree, atom)) return true;
	if (not tree->is_link()) return false;
	if (nameserver().isA(tree->get_type(), SCOPE_LINK))
	{
		ScopeLinkPtr stree(ScopeLinkCast(tree));
		const HandleSet& varset = stree->get_variables().varset;
		if (varset.find(atom) != varset.cend())
			return false;
	}

	// Recursive case
	for (const Handle& h : tree->getOutgoingSet())
		if (is_unscoped_in_tree(h, atom))
			return true;
	return false;
}

bool is_constant_in_tree(const Handle& tree, const Handle& atom)
{
	// Base cases
	if (content_eq(tree, atom)) return true;
	if (not tree->is_link()) return false;

	// Halt recursion if the term is executable.
	if (tree->is_executable()) return false;

	// Recursive case
	for (const Handle& h : tree->getOutgoingSet())
		if (is_constant_in_tree(h, atom))
			return true;
	return false;
}

static bool found_helper(const Handle& tree,
                         const Handle& subtree,
                         const Handle& atom,
           bool (*reject)(const Handle&, const Handle&, const Handle&))
{
	// Base cases
	if (content_eq(subtree, atom)) return true;
	if (not subtree->is_link()) return false;
	if (reject(tree, subtree, atom)) return false;

	// Recursive case
	for (const Handle& h : subtree->getOutgoingSet())
		if (found_helper(tree, h, atom, reject))
			return true;
	return false;
}

bool is_found_in_tree(const Handle& tree, const Handle& atom,
           bool (*reject)(const Handle&, const Handle&, const Handle&))
{
	return found_helper(tree, tree, atom, reject);
}

bool is_unquoted_unscoped_in_tree(const Handle& tree, const Handle& atom)
{
	return is_unquoted_in_tree(tree, atom) and is_unscoped_in_tree(tree, atom);
}

bool is_free_in_tree(const Handle& tree, const Handle& atom)
{
	// The atom may occur in two distinct places in a tree: in
	// one place, it is scoped, and in another, its in an executable
	// term. We have to reject both, and not one-at-a-time.
	auto scoped_or_executable =
	 [](const Handle& tree, const Handle& subtr, const Handle& ato)
	{
		// Plow through any quotes.
		if (is_quoted_in_tree(tree, subtr)) return false;

		// Halt recursion if scoped.
		if (nameserver().isA(subtr->get_type(), SCOPE_LINK))
		{
			ScopeLinkPtr stree(ScopeLinkCast(subtr));
			const HandleSet& varset = stree->get_variables().varset;
			if (varset.find(ato) != varset.cend())
				return true;
		}
		return false;
	};
	return is_unquoted_in_tree(tree, atom) and
	       is_found_in_tree(tree, atom, scoped_or_executable);
}

bool is_unquoted_unscoped_in_any_tree(const HandleSeq& hs,
                                      const Handle& v)
{
	for (const Handle& h : hs)
		if (is_unquoted_unscoped_in_tree(h, v))
			return true;
	return false;
}

bool is_free_in_any_tree(const HandleSeq& hs, const Handle& v)
{
	for (const Handle& h : hs)
		if (is_free_in_tree(h, v))
			return true;
	return false;
}

bool any_atom_in_tree(const Handle& tree, const HandleSet& atoms)
{
	for (const Handle& n: atoms)
	{
		if (is_atom_in_tree(tree, n)) return true;
	}
	return false;
}

bool any_unquoted_in_tree(const Handle& tree, const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unquoted_in_tree(tree, n)) return true;
	return false;
}

bool any_unscoped_in_tree(const Handle& tree, const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unscoped_in_tree(tree, n)) return true;
	return false;
}

bool any_constant_in_tree(const Handle& tree, const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_constant_in_tree(tree, n)) return true;
	return false;
}

bool any_unquoted_unscoped_in_tree(const Handle& tree,
                                   const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unquoted_in_tree(tree, n) and is_unscoped_in_tree(tree, n))
			return true;
	return false;
}

bool any_free_in_tree(const Handle& tree,
                      const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_free_in_tree(tree, n))
			return true;
	return false;
}

bool any_free_in_tree(const Handle& tree,
                      const HandleSeq& atoms)
{
	for (const Handle& n: atoms)
		if (is_free_in_tree(tree, n))
			return true;
	return false;
}

unsigned int num_unquoted_unscoped_in_tree(const Handle& tree,
                                           const HandleSet& atoms)
{
	unsigned int count = 0;
	for (const Handle& n: atoms)
	{
		if (is_unquoted_unscoped_in_tree(tree, n)) count++;
	}
	return count;
}

HandleSet unquoted_unscoped_in_tree(const Handle& tree,
                                    const HandleSet& atoms)
{
	HandleSet rv;
	for (const Handle& n: atoms)
	{
		if (is_unquoted_unscoped_in_tree(tree, n)) rv.insert(n);
	}
	return rv;
}

bool is_atom_in_any_tree(const HandleSeq& trees,
                         const Handle& atom)
{
	for (const Handle& tree: trees)
		if (is_atom_in_tree(tree, atom)) return true;
	return false;
}

bool is_unquoted_in_any_tree(const HandleSeq& trees,
                             const Handle& atom)
{
	for (const Handle& tree: trees)
		if (is_unquoted_in_tree(tree, atom)) return true;
	return false;
}

bool contains_atomtype(const Handle& clause, Type atom_type,
                       Quotation quotation)
{
	Type clause_type = clause->get_type();
	if (quotation.is_unquoted() and nameserver().isA(clause_type, atom_type))
		return true;

	quotation.update(clause_type);

	if (not clause->is_link()) return false;

	for (const Handle& subclause: clause->getOutgoingSet())
	{
		if (contains_atomtype(subclause, atom_type, quotation)) return true;
	}
	return false;
}

size_t contains_atomtype_count(const Handle& clause, Type atom_type,
                               Quotation quotation)
{
	size_t cnt = 0;

	Type clause_type = clause->get_type();
	if (quotation.is_unquoted() and nameserver().isA(clause_type, atom_type))
		cnt++;

	quotation.update(clause_type);

	if (not clause->is_link()) return 0;

	for (const Handle& subclause: clause->getOutgoingSet())
	{
		cnt += contains_atomtype_count(subclause, atom_type, quotation);
	}
	return cnt;
}

HandleSet get_free_variables(const Handle& h, Quotation quotation)
{
	Type t = h->get_type();

	// Base cases
	if ((t == VARIABLE_NODE or t == GLOB_NODE) and quotation.is_unquoted())
		return {h};
	if (h->is_node())
		return {};

	// Recursive cases
	OC_ASSERT(h->is_link());
	quotation.update(t);
	HandleSet results = get_free_variables(h->getOutgoingSet(), quotation);
	// If the link was a scope link then remove the scoped
	// variables from the free variables found.
	if (nameserver().isA(h->get_type(), SCOPE_LINK))
	{
		ScopeLinkPtr sh(ScopeLinkCast(h));
		const HandleSet& varset = sh->get_variables().varset;
		for (auto& v : varset)
			results.erase(v);
	}
	return results;
}

HandleSet get_free_variables(const HandleSeq& hs, Quotation quotation)
{
	HandleSet results;
	for (const Handle& h : hs) {
		HandleSet free_var = get_free_variables(h, quotation);
		results.insert(free_var.begin(), free_var.end());
	}
	return results;
}

HandleSet get_all_uniq_atoms(const Handle& h)
{
	// Base cases
	if (!h)
		return {};
	if (h->is_node())
		return {h};

	// Recursive cases
	if (h->is_link()) {
		HandleSet results({h});
		for (const Handle& child : h->getOutgoingSet()) {
			HandleSet aas = get_all_uniq_atoms(child);
			results.insert(aas.begin(), aas.end());
		}
		return results;
	}

	// Please the compiler
	return {};
}

bool is_closed(const Handle& h, Quotation quotation)
{
	return get_free_variables(h, quotation).empty();
}

bool is_constant(const Handle& h, Quotation quotation)
{
	if (not is_closed(h, quotation)) return false;

	if (contains_atomtype(h, TYPE_NODE, quotation)) return false;
	if (contains_atomtype(h, TYPE_CHOICE, quotation)) return false;
	if (contains_atomtype(h, TYPE_OUTPUT_LINK, quotation)) return false;

	return true;
}

} // namespace opencog
