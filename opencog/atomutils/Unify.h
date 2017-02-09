/**
 * Unify.h
 *
 * Utilities for unifying atoms.
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 * Author: Nil Geisweiller
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

#ifndef _OPENCOG_UNIFY_UTILS_H
#define _OPENCOG_UNIFY_UTILS_H

#include <boost/operators.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/Quotation.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atomutils/TypeUtils.h>

namespace opencog {

class Unify
{
public:
	// A context holds the quotation state and the current shadowing
	// variables of a atom (typically coming from ancestor scopes).
	//
	// The context is important to have for both unification, in
	// particular sub-unification, see Unify::subunify, and closure,
	// see Unify::substitution_closure, because quoted or shadowed
	// variables should not be substituted.
	//
	// This notion of context is distinct and unrelated to
	// ContextLink.
	struct Context : public boost::totally_ordered<Context>
	{
		// Default ctor
		Context(const Quotation& quotation=Quotation(),
		        const OrderedHandleSet& shadow=OrderedHandleSet());

		// Quotation state
		Quotation quotation;

		// Set of shadowing variables
		OrderedHandleSet shadow;

		/**
		 * Update the context over an atom. That is if the atom is a
		 * consumable quotation then update the context quotation. If
		 * the atom is a scope link then update the context shadow.
		 */
		void update(const Handle& h);

		/**
		 * Return true iff the given atom in that context is a free
		 * variable, that is unquoted and unshadowed.
		 */
		bool is_free_variable(const Handle& h) const;

		/**
		 * Comparison.
		 */
		bool operator==(const Context& context) const;
		bool operator<(const Context& context) const;
	};

	// Contextual Handle
	//
	// TODO: the notion of equality between 2 CHandles might one where
	// the Context isn't necessarily equal but where the 2 handles
	// (besides being equal) have the same quotation and same
	// (free inter shadow) variables.
	struct CHandle : public boost::totally_ordered<CHandle>
	{
		CHandle(const Handle& handle, const Context& context=Context());

		Handle handle;
		Context context;

		/**
		 * Return true iff the atom in that context is a free
		 * variable, that is unquoted and unshadowed.
		 */
		bool is_free_variable() const;

		/**
		 * Return the set of free visible variables from that context.
		 */
		OrderedHandleSet get_free_variables() const;

		/**
		 * Return true if 2 contextual handles are satisfiable in some
		 * restricted sense, it only covers the cases where atoms are
		 * equal and have the same free variables.
		 */
		bool satisfiable(const CHandle& ch) const;

		/**
		 * Comparison.
		 */
		bool operator==(const CHandle& ch) const;
		bool operator<(const CHandle& ch) const;
	};

	// Partition block
	typedef std::set<CHandle> Block;

	// Mapping from partition blocks to type
	typedef std::map<Block, Handle> Partition;

	// This is in fact a typed block but is merely named Block due to
	// being so frequently used.
	typedef Partition::value_type TypedBlock;

	// Set of partitions, that is a solution set, typically assumed
	// satisfiable when used in standalone.
	typedef std::set<Partition> Partitions;

	// TODO: the type of a typed block is currently a handle of the
	// variable or ground it is exists, instead of an actual type.
	struct SolutionSet : public boost::equality_comparable<SolutionSet>
	{
		// Default ctor
		SolutionSet(bool s=true, const Partitions& p=Partitions());

		// Whether the unification satisfiable. Not that satisfiable is
		// different than empty. An empty solution set may still be
		// satisfiable, that would be the case of two candidates that
		// match but have no variables.
		bool satisfiable;

		// Set of typed partitions
		Partitions partitions;

		bool operator==(const SolutionSet& other) const;
	};

	// Mapping from Handle (typically a variable) to a contextual handle
	typedef std::map<Handle, CHandle> HandleCHandleMap;

	// Subtitution values and their corresponding variable declaration
	// (cause some values will be variables).
	typedef std::map<HandleCHandleMap, Handle> TypedSubstitutions;
	typedef TypedSubstitutions::value_type TypedSubstitution;

	/**
	 * Generate typed substitution rules, given a satisfiable
	 * SolutionSet and the term from which to select the variables as
	 * values in case multiple choices are possible.
	 *
	 * Remarks: lhs_vardecl and rhs_vardecl are passed by copy because
	 * they will be filled with free variables in case they are empty.
	 */
	TypedSubstitutions typed_substitutions(const SolutionSet& sol,
	                                       const Handle& pre,
	                                       const Handle& lhs=Handle::UNDEFINED,
	                                       const Handle& rhs=Handle::UNDEFINED,
	                                       Handle lhs_vardecl=Handle::UNDEFINED,
	                                       Handle rhs_vardecl=Handle::UNDEFINED) const;

	/**
	 * Like typed_substitutions but generate a single typed
	 * substitution of a supposedly satisfiable partition.
	 */
	TypedSubstitution typed_substitution(const Partition& partition,
	                                     const Handle& pre,
	                                     const Handle& lhs_vardecl,
	                                     const Handle& rhs_vardecl) const;

	/**
	 * Calculate the closure of a typed substitution. That is apply
	 * self-substitution to each values till a fixed point is
	 * reached. For instance given the variable to value mapping
	 *
	 * (Variable "$X") -> (Inheritance (Variable "$P") (Variable "$Q"))
	 * (Variable "$P") -> (Concept "A")
	 * (Variable "$Q") -> (Concept "B")
	 *
	 * produces
	 *
	 * (Variable "$X") -> (Inheritance (Concept "A") (Concept "B"))
	 * (Variable "$P") -> (Concept "A")
	 * (Variable "$Q") -> (Concept "B")
	 */
	HandleCHandleMap substitution_closure(const HandleCHandleMap& var2val) const;

	/**
	 * If the quotations are useless or harmful, which might be the
	 * case if they deprive a ScopeLink from hiding supposedly hidden
	 * variables, consume them.
	 *
	 * Specifically this code makes 2 assumptions
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
	 * No other use of quotation is assumed besides the 2 above.
	 */
	BindLinkPtr consume_ill_quotations(BindLinkPtr bl) const;
	Handle consume_ill_quotations(const Variables& variables, Handle h,
	                              Quotation quotation=Quotation(),
	                              bool escape=false /* ignore the next
	                                                 * quotation
	                                                 * consumption */) const;

	/**
	 * Return true iff the variable declaration of local_scope is a
	 * variable of variables wrapped in a UnquoteLink.
	 */
	bool is_bound_to_ancestor(const Variables& variables,
	                          const Handle& local_scope) const;

	/**
	 * Return true iff the handle or type correspond to a pattern
	 * matcher connector.
	 */
	bool is_pm_connector(const Handle& h) const;
	bool is_pm_connector(Type t) const;

	/**
	 * Given a typed substitution, perform the substitution over a scope
	 * link (for now only BindLinks are supported).
	 */
	Handle substitute(BindLinkPtr bl, const TypedSubstitution& ts) const;

	/**
	 * This algorithm perform unification by recursively
	 *
	 * 1. Generate all equality partitions
	 *
	 * 2. Decorate partition blocks with types
	 *
	 * 3. Check that each partition is satisfiable
	 *
	 * For now the types in 2. are represented by the substitutions, for
	 * instance the typed block {{X, A}, A} means that X is A. Later one
	 * we will replace that by deep types as to represents things like
	 * {{X, Y}, ConceptNode} meaning that X and Y must be concept nodes is
	 * order to be satisfiable, of course the deep will still need to
	 * capture grounds such as {{X, A}, A}, it's not clear excatly how,
	 * maybe Linas deep type implementation can already do that.
	 *
	 * To solve 3, for each partition block, it computes the type that
	 * intersects all its elements and repeat till a fixed point is
	 * reached. To do that efficiently we would need to build a dependency
	 * DAG, but at first we can afford to compute type intersections is
	 * random order.
	 *
	 * Also, permutations are supported though very slow.
	 *
	 * Examples:
	 *
	 * 1.
	 *
	 * unify((Variable "$X"), (Concept "A"))
	 * ->
	 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>}}
	 *
	 * meaning that the partition block {(Variable "$X"), (Concept "A")}
	 * has type (Concept "A"), and there is only one partition in the
	 * solution set.
	 *
	 * 2.
	 *
	 * unify((Concept "A"), (Concept "$X"))
	 * ->
	 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>}}
	 *
	 * 3.
	 *
	 * unify((Inheritance (Concept "A") (Concept "B")), (Variable "$X"))
	 * ->
	 * {{<{(Variable "$X"), (Inheritance (Concept "A") (Concept "B"))},
	 *    (Inheritance (Concept "A") (Concept "B"))>}}
	 *
	 * 4.
	 *
	 * unify((Inheritance (Concept "A") (Variable "$Y")),
	 *       (Inheritance (Variable "$X") (Concept "B"))
	 * ->
	 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>,
	 *   <{(Variable "$Y"), (Concept "B")}, (Concept "B")>}}
	 *
	 * 5.
	 *
	 * unify((And (Concept "A") (Concept "B")),
	 *       (And (Variable "$X") (Variable "$Y"))
	 * ->
	 * {
	 *  {<{(Variable "$X"), (Concept "A")}, (Concept "A")>,
	 *   <{(Variable "$Y"), (Concept "B")}, (Concept "B")>},
	 *
	 *  {<{(Variable "$X"), (Concept "B")}, (Concept "B")>,
	 *   <{(Variable "$Y"), (Concept "A")}, (Concept "A")>}
	 * }
	 *
	 * mean that the solution set has 2 partitions, one where X unifies to
	 * A and Y unifies to B, and another one where X unifies to B and Y
	 * unifies to A.
	 */
	SolutionSet operator()(const Handle& lhs, const Handle& rhs,
	                       const Handle& lhs_vardecl=Handle::UNDEFINED,
	                       const Handle& rhs_vardecl=Handle::UNDEFINED);

private:
	Handle _lhs_vardecl;
	Handle _rhs_vardecl;

	/**
	 * Find the least abstract atom in the given block.
	 */
	CHandle find_least_abstract(const TypedBlock& block, const Handle& pre) const;

	/**
	 * Unify lhs and rhs. _lhs_vardecl and _rhs_vardecl should be set
	 * prior to run this method.
	 */
	// TODO: replace quotation by context
	SolutionSet unify(const CHandle& lhs, const CHandle& rhs) const;
	SolutionSet unify(const Handle& lhs, const Handle& rhs,
	                  Context lhs_context=Context(),
	                  Context rhs_context=Context()) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, considering
	 * all permutations.
	 */
	// TODO: replace quotation by context
	SolutionSet unordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
	                            Context lhs_context=Context(),
	                            Context rhs_context=Context()) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, in the
	 * provided order.
	 */
	// TODO: replace quotation by context
	SolutionSet ordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
	                          Context lhs_context=Context(),
	                          Context rhs_context=Context()) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, considering
	 * all pairwise combinations.
	 */
	SolutionSet comb_unify(const std::set<CHandle>& lhs,
	                       const std::set<CHandle>& rhs) const;

	/**
	 * Return if the atom is an unordered link.
	 */
	bool is_unordered(const Handle& h) const;

	/**
	 * Return a copy of a HandleSeq with the ith element removed.
	 */
	HandleSeq cp_erase(const HandleSeq& hs, Arity i) const;

	/**
	 * Build elementary solution set between 2 atoms given that at least
	 * one of them is a variable.
	 */
	SolutionSet mkvarsol(const CHandle& lhs, const CHandle& rhs) const;
	SolutionSet mkvarsol(const Handle& lhs, const Handle& rhs,
	                     Context lhs_context, Context rhs_context) const;

public:                         // TODO: being friend with UnifyUTest
                                // somehow doesn't work
	friend class UnifyUTest;

	/**
	 * Join 2 solution sets. Generate the product of all consistent
	 * solutions (with partitions so that all blocks are typed with a
	 * defined Handle).
	 */
	SolutionSet join(const SolutionSet& lhs, const SolutionSet& rhs) const;

private:
	/**
	 * Join a satisfiable partition sets with a satisfiable partition.
	 */
	Partitions join(const Partitions& lhs, const Partition& rhs) const;

	/**
	 * Join 2 partitions. The result can be set of partitions (see
	 * join(const Partition&, const TypedBlock&) for explanation).
	 */
	Partitions join(const Partition& lhs, const Partition& rhs) const;

	/**
	 * Join a block with a partition set. The partition set is assumed
	 * non empty and satisfiable.
	 */
	Partitions join(const Partitions& partitions, const TypedBlock& block) const;

	/**
	 * Join a partition and a block. If the block has no element in
	 * common with any block of the partition, merely insert
	 * it. Otherwise fuse the blocks with common elements into
	 * one. During this fusion new unification problems may arise
	 * (TODO: explain why) thus possibly multiple partitions will be
	 * returned.
	*/
	Partitions join(const Partition& partition, const TypedBlock &block) const;

	/**
	 * Join a block to a partition to form a single block. It is
	 * assumed that all blocks have elements in common.
	*/
	TypedBlock join(const std::vector<TypedBlock>& common_blocks,
	                const TypedBlock& block) const;

	/**
	 * Join 2 blocks (supposedly satisfiable).
	 *
	 * That is compute their type intersection and if defined, then build
	 * the block as the union of the 2 blocks, typed with their type
	 * intersection.
	 */
	TypedBlock join(const TypedBlock& lhs, const TypedBlock& rhs) const;

	/**
	 * Unify all terms that are not in the intersection of block and
	 * each block of common_blocks.
	 *
	 * TODO: should probably support quotation.
	 */
	SolutionSet subunify(const std::vector<TypedBlock>& common_blocks,
	                     const TypedBlock& block) const;

	/**
	 * Unify all terms that are not in the intersection of blocks lhs
	 * and rhs.
	 *
	 * TODO: should probably support quotation.
	 */
	SolutionSet subunify(const TypedBlock& lhs, const TypedBlock& rhs) const;

	/**
	 * Return true if a unification block is satisfiable. A unification
	 * block is non satisfiable if it's type is undefined (bottom).
	 */
	bool is_satisfiable(const TypedBlock& block) const;
};

/**
 * Till content equality between atoms become the default.
 */
bool ch_content_eq(const Unify::CHandle& lhs, const Unify::CHandle& rhs);
bool ohs_content_eq(const OrderedHandleSet& lhs, const OrderedHandleSet& rhs);
bool hm_content_eq(const HandleMap& lhs, const HandleMap& rhs);
bool hchm_content_eq(const Unify::HandleCHandleMap& lhs,
                     const Unify::HandleCHandleMap& rhs);
bool ts_content_eq(const Unify::TypedSubstitution& lhs,
                   const Unify::TypedSubstitution& rhs);
bool tss_content_eq(const Unify::TypedSubstitutions& lhs,
                    const Unify::TypedSubstitutions& rhs);

/**
 * Strip the context from hchm.
 */
HandleMap strip_context(const Unify::HandleCHandleMap& hchm);

/**
 * Calculate type intersection. For example: say you have for a block
 * with
 *
 * X
 * ListLink(Y)
 * ListLink(Z)
 *
 * meaning that X is equal to ListLink Y which is equal to ListLink Z,
 * each having the following types at that point (i.e. not having
 * reached the fixed point yet)
 *
 * X:Atom
 * ListLink(Y):ListLink(Atom)
 * ListLink(Z):ListLink(Atom)
 *
 * then their type intersection will be
 *
 * ListLink(Atom)
 *
 * which is supposed to represent the set of all potential groundings
 * that may satisfy that block.
 *
 * TODO: For now though it's only a very limited type intersection,
 *       should support structural types, etc.
 *
 * TODO: This can be probably by optimized by using VariableListPtr
 *       instead of Handle, so we don't rebuild it every time.
 */
Handle type_intersection(const Unify::CHandle& lch, const Unify::CHandle& rch,
                         const Handle& lhs_vardecl, const Handle& rhs_vardecl);
Handle type_intersection(const Handle& lhs, const Handle& rhs,
                         const Handle& lhs_vardecl=Handle::UNDEFINED,
                         const Handle& rhs_vardecl=Handle::UNDEFINED,
                         Quotation lhs_quotation=Quotation(),
                         Quotation rhs_quotation=Quotation());

/**
 * Return a simplification of a type union, by eliminating all types
 * that are redundant. For instance
 *
 * {Node, ConceptNode, ListLink}
 *
 * would return
 *
 * {Node, ListLink}
 *
 * As ConceptNode inherits Node.
 */
std::set<Type> simplify_type_union(std::set<Type>& type);

/**
 * Return the union type of a variable given its variable declaration.
 * If the variable declaration is empty (Handle::UNDEFINED) then the
 * union type is not empty, instead it contains the singleton
 * {ATOM}. An empty union type would instead mean the bottom type
 * (that nothing can inherit).
 */
std::set<Type> get_union_type(const Handle& h, const Handle& vardecl);

/**
 * Return true if lhs inherit rhs. If lhs is not a variable then it
 * relays that to VariableList::is_type, otherwise their type
 * declarations are compared.
 */
bool inherit(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl,
             Quotation lhs_quotation=Quotation(),
             Quotation rhs_quotation=Quotation());

/**
 * Extreme crude version of the above when we have no variable
 * declarations. Basically 2 variables inherits each other and a non
 * variable inherits a variable. Everything else return false.
 */
bool inherit(const Handle& lhs, const Handle& rhs);

/**
 * Return true if lhs inherits rhs.
 */
bool inherit(Type lhs, Type rhs);

/**
 * Return true if a type inherits a type union.
 */
bool inherit(Type lhs, const std::set<Type>& rhs);

/**
 * Return true if lhs inherits rhs. That is if all elements of lhs
 * inherits rhs.
 */
bool inherit(const std::set<Type>& lhs, const std::set<Type>& rhs);

/**
 * Generate a VariableList of the free variables of a given atom h.
 */
VariableListPtr gen_varlist(const Handle& h);
Handle gen_vardecl(const Handle& h);

/**
 * Generate a VariableList of the free variables of a given contextual
 * atom ch.
 */
VariableListPtr gen_varlist(const Unify::CHandle& ch);

/**
 * Given an atom h and its variable declaration vardecl, turn the
 * vardecl into a VariableList if not already, and if undefined,
 * generate a VariableList of the free variables of h.
 */
VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl);

/**
 * Merge two vardecls into one. If a variable is present in both
 * vardecls then the more restrictive one replaces the less
 * restrictive one.
 *
 * TODO: give example.
 */
Handle merge_vardecl(const Handle& lhs_vardecl, const Handle& rhs_vardecl);

std::string oc_to_string(const Unify::Context& c);
std::string oc_to_string(const Unify::CHandle& ch);
std::string oc_to_string(const Unify::Block& pb);
std::string oc_to_string(const Unify::Partition& hshm);
std::string oc_to_string(const Unify::TypedBlock& ub);
std::string oc_to_string(const Unify::Partitions& par);
std::string oc_to_string(const Unify::SolutionSet& sol);
std::string oc_to_string(const Unify::HandleCHandleMap& hchm);
std::string oc_to_string(const Unify::TypedSubstitution& ts);
std::string oc_to_string(const Unify::TypedSubstitutions& tss);
	
} // namespace opencog

#endif // _OPENCOG_UNIFY_UTILS_H
