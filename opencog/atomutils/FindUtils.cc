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

#include "FindUtils.h"

namespace opencog {

FindAtoms::FindAtoms(Type t, bool subclass)
	: _target_types({t})
{
	if (subclass)
	{
		classserver().getChildrenRecursive(t, inserter(_target_types));
	}
}

FindAtoms::FindAtoms(Type ta, Type tb, bool subclass)
	: FindAtoms(ta, subclass)
{
	if (subclass)
	{
		classserver().getChildrenRecursive(tb, inserter(_target_types));
	}
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
	Type t = h->getType();
	if (quotation.is_unquoted() and
	    (1 == _target_types.count(t) or _target_atoms.count(h) == 1))
	{
		varset.insert(h);
		return IMM; //! Don't explore link-typed vars!
	}

	quotation.update(t);

	for (Type stopper : stopset)
	{
		if (classserver().isA(t, stopper)) return NOPE;
	}

	if (h->isLink())
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
	if (not tree->isLink()) return false;

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
	if (not tree->isLink()) return std::numeric_limits<int>::max();

	// Recursive case
	quotation.update(tree->getType());
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
	if (not tree->isLink()) return std::numeric_limits<int>::min();

	// Recursive case
	quotation.update(tree->getType());
	int result = std::numeric_limits<int>::min();
	for (const Handle& h : tree->getOutgoingSet())
		result = std::max(result, max_quotation_level(h, atom, quotation));
	return result;
}

bool is_unscoped_in_tree(const Handle& tree, const Handle& atom)
{
	// Base cases
	if (content_eq(tree, atom)) return true;
	if (not tree->isLink()) return false;
	ScopeLinkPtr stree(ScopeLinkCast(tree));
	if (nullptr != stree) {
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

bool is_unquoted_unscoped_in_tree(const Handle& tree, const Handle& atom)
{
	return is_unquoted_in_tree(tree, atom) and is_unscoped_in_tree(tree, atom);
}

bool is_free_in_tree(const Handle& tree, const Handle& atom)
{
	return is_unquoted_unscoped_in_tree(tree, atom);
}

bool is_unquoted_unscoped_in_any_tree(const HandleSeq& hs,
                                      const Handle& v)
{
	for (const Handle& h : hs)
		if (is_unquoted_unscoped_in_tree(h, v))
			return true;
	return false;
}

bool is_free_in_any_tree(const HandleSeq& hs, const Handle& atom)
{
	return is_unquoted_unscoped_in_any_tree(hs, atom);
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
	{
		if (is_unquoted_in_tree(tree, n)) return true;
	}
	return false;
}

bool any_unscoped_in_tree(const Handle& tree, const HandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unscoped_in_tree(tree, n)) return true;
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

unsigned int num_unquoted_in_tree(const Handle& tree,
                                  const HandleSet& atoms)
{
	unsigned int count = 0;
	for (const Handle& n: atoms)
	{
		if (is_unquoted_in_tree(tree, n)) count++;
	}
	return count;
}

bool is_atom_in_any_tree(const HandleSeq& trees,
                         const Handle& atom)
{
	for (const Handle& tree: trees)
	{
		if (is_atom_in_tree(tree, atom)) return true;
	}
	return false;
}

bool is_unquoted_in_any_tree(const HandleSeq& trees,
                             const Handle& atom)
{
	for (const Handle& tree: trees)
	{
		if (is_unquoted_in_tree(tree, atom)) return true;
	}
	return false;
}

bool contains_atomtype(const Handle& clause, Type atom_type, Quotation quotation)
{
	Type clause_type = clause->getType();
	if (quotation.is_unquoted() and classserver().isA(clause_type, atom_type))
		return true;

	quotation.update(clause_type);

	if (not clause->isLink()) return false;

	for (const Handle& subclause: clause->getOutgoingSet())
	{
		if (contains_atomtype(subclause, atom_type, quotation)) return true;
	}
	return false;
}

HandleSet get_free_variables(const Handle& h, Quotation quotation)
{
	Type t = h->getType();

	// Base cases
	if (t == VARIABLE_NODE and quotation.is_unquoted())
		return {h};
	if (h->isNode())
		return {};

	// Recursive cases
	OC_ASSERT(h->isLink());
	quotation.update(t);
	HandleSet results = get_free_variables(h->getOutgoingSet(), quotation);
	// If the link was a scope link then remove the scoped
	// variables from the free variables found.
	ScopeLinkPtr sh(ScopeLinkCast(h));
	if (nullptr != sh) {
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

bool is_closed(const Handle& h, Quotation quotation)
{
	return get_free_variables(h, quotation).empty();
}

} // namespace opencog
