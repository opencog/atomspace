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
	: _target_types({ta, tb})
{
	if (subclass)
	{
		classserver().getChildrenRecursive(ta, inserter(_target_types));
		classserver().getChildrenRecursive(tb, inserter(_target_types));
	}
}

FindAtoms::FindAtoms(const Handle& atom)
	: _target_types(),
	  _target_atoms({atom})
{}

FindAtoms::FindAtoms(const OrderedHandleSet& selection)
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

FindAtoms::Loco FindAtoms::find_rec(const Handle& h, int quotation_level)
{
	Type t = h->getType();
	if (quotation_level < 1 and
	    (1 == _target_types.count(t) or _target_atoms.count(h) == 1))
	{
		varset.insert(h);
		return IMM; //! Don't explore link-typed vars!
	}

	if (t == QUOTE_LINK) quotation_level++;
	else if (t == UNQUOTE_LINK) quotation_level--;

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
			Loco where = find_rec(oh, quotation_level);
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
                        int level_from_root)
{
	// Base case
	if (tree == atom) return level_from_root;
	if (not tree->isLink()) return std::numeric_limits<int>::max();

	// Recursive case
	if (tree->getType() == QUOTE_LINK) level_from_root++;
	else if (tree->getType() == UNQUOTE_LINK) level_from_root--;
	int result = std::numeric_limits<int>::max();
	for (const Handle& h : tree->getOutgoingSet())
		result = std::min(result, min_quotation_level(h, atom, level_from_root));
	return result;
}

int max_quotation_level(const Handle& tree,
                        const Handle& atom,
                        int level_from_root)
{
	// Base case
	if (tree == atom) return level_from_root;
	if (not tree->isLink()) return std::numeric_limits<int>::min();

	// Recursive case
	if (tree->getType() == QUOTE_LINK) level_from_root++;
	else if (tree->getType() == UNQUOTE_LINK) level_from_root--;
	int result = std::numeric_limits<int>::min();
	for (const Handle& h : tree->getOutgoingSet())
		result = std::max(result, max_quotation_level(h, atom, level_from_root));
	return result;
}

bool is_unscoped_in_tree(const Handle& tree, const Handle& atom)
{
	// Base cases
	if (tree == atom) return true;
	if (not tree->isLink()) return false;
	ScopeLinkPtr stree(ScopeLinkCast(tree));
	if (nullptr != stree) {
		const OrderedHandleSet& varset = stree->get_variables().varset;
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

bool is_unquoted_unscoped_in_any_tree(const HandleSeq& hs,
                                      const Handle& v)
{
	for (const Handle& h : hs)
		if (is_unquoted_unscoped_in_tree(h, v))
			return true;
	return false;
}

bool any_atom_in_tree(const Handle& tree, const OrderedHandleSet& atoms)
{
	for (const Handle& n: atoms)
	{
		if (is_atom_in_tree(tree, n)) return true;
	}
	return false;
}

bool any_unquoted_in_tree(const Handle& tree, const OrderedHandleSet& atoms)
{
	for (const Handle& n: atoms)
	{
		if (is_unquoted_in_tree(tree, n)) return true;
	}
	return false;
}

bool any_unscoped_in_tree(const Handle& tree, const OrderedHandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unscoped_in_tree(tree, n)) return true;
	return false;
}

bool any_unquoted_unscoped_in_tree(const Handle& tree,
                                   const OrderedHandleSet& atoms)
{
	for (const Handle& n: atoms)
		if (is_unquoted_in_tree(tree, n) and is_unscoped_in_tree(tree, n))
			return true;
	return false;
}

unsigned int num_unquoted_in_tree(const Handle& tree,
                                  const OrderedHandleSet& atoms)
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

bool contains_atomtype(const Handle& clause, Type atom_type)
{
	Type clause_type = clause->getType();
	if (classserver().isA(clause_type, atom_type)) return true;
	if (QUOTE_LINK == clause_type) return false;
	// if (classserver().isA(clause_type, SCOPE_LINK)) return false;

	if (not clause->isLink()) return false;

	for (const Handle& subclause: clause->getOutgoingSet())
	{
		if (contains_atomtype(subclause, atom_type)) return true;
	}
	return false;
}

bool contains_atomtype(const HandleSeq& clauses, Type atom_type)
{
	for (const Handle& clause: clauses)
	{
		if (contains_atomtype(clause, atom_type)) return true;
	}
	return false;
}

OrderedHandleSet get_free_variables(const Handle& h, int quotation_level)
{
	Type t = h->getType();

	// Base cases
	if (t == VARIABLE_NODE and quotation_level < 1)
		return {h};
	if (h->isNode())
		return {};

	// Recursive cases
	OrderedHandleSet results;
	OC_ASSERT(h->isLink());
	if (t == QUOTE_LINK) quotation_level++;
	else if (t == UNQUOTE_LINK) quotation_level--;
	for (const Handle& ch : h->getOutgoingSet()) {
		OrderedHandleSet ch_free_var = get_free_variables(ch, quotation_level);
		results.insert(ch_free_var.begin(), ch_free_var.end());
	}
	// If the link was a scope link then remove the scoped
	// variables from the free variables found.
	ScopeLinkPtr sh(ScopeLinkCast(h));
	if (nullptr != sh) {
		const OrderedHandleSet& varset = sh->get_variables().varset;
		for (auto& v : varset)
			results.erase(v);
	}
	return results;
}

bool is_closed(const Handle& h)
{
	return get_free_variables(h).empty();
}

} // namespace opencog
