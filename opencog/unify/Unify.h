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

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/proto/atom_types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/Context.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atomutils/TypeUtils.h>

namespace opencog {

class Unify
{
	friend class UnifyUTest;

public:
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
		 * Return true iff the atom in that context is a variable,
		 * free or not.
		 */
		bool is_variable() const;

		/**
		 * Return true iff the atom in that context is a free
		 * variable, that is unquoted and unshadowed.
		 */
		bool is_free_variable() const;

		/**
		 * Return the set of free visible variables from that context.
		 */
		HandleSet get_free_variables() const;

		/**
		 * Return iterator of the variable declaration containing a given
		 * variable, if so.
		 */
		Context::VariablesStack::const_iterator
		find_variables(const Handle& h) const;

		/**
		 * Return true iff has a consumable quotation
		 */
		bool is_consumable() const;

		/**
		 * Return true iff it is quoted.
		 */
		bool is_quoted() const;

		/**
		 * Return true iff it is unquoted.
		 */
		bool is_unquoted() const;

		/**
		 * Update its context, and if consumable update the Handle as
		 * well, that is replace itself by its child.
		 */
		void update();

		/**
		 * Return true if 2 contextual handles are satisfiable in some
		 * restricted sense, it assumes that
		 *
		 * 1. at least one of them is a node
		 * 2. none is a free variable (i.e. unfree or not a variable)
		 *
		 * Given these 2 assumptions fulfilled it will check whether
		 * the 2 atoms are equal, and if there are not could there be
		 * alpha equivalent (assuming they are variables).
		 */
		bool is_node_satisfiable(const CHandle& other) const;

		/**
		 * Comparison.
		 */
		bool operator==(const CHandle& other) const;
		bool operator<(const CHandle& other) const;

		/**
		 * Cast operators
		 */
		explicit operator bool() const;
	};

	// Pair of CHandles
	typedef std::pair<CHandle, CHandle> CHandlePair;

	// Partition block
	typedef std::set<CHandle> Block;

	// Mapping from partition blocks to type
	typedef std::map<Block, CHandle> Partition;

	// This is in fact a typed block but is merely named Block due to
	// being so frequently used.
	typedef Partition::value_type TypedBlock;

	// Useful for representing common block before sub-unification.
	typedef std::vector<TypedBlock> TypedBlockSeq;

	// Set of partitions, that is a solution set, typically assumed
	// satisfiable when used in standalone.
	typedef std::set<Partition> Partitions;

	// Empty partition set
	static const Partitions empty_partitions;

	// Partition set singleton containing an empty partition. This is
	// the simplest satisfiable solution set.
	static const Partitions empty_partition_singleton;

	// TODO: the type of a typed block is currently a handle of the
	// variable or ground it is exists, instead of an actual type.
	struct SolutionSet : Partitions
	{
		// Default ctor
		explicit SolutionSet(const Partitions& p);
		// Helper ctor. Initialize with the empty partition as
		// singleton, i.e. a satisfiable solution, if s == true, or
		// the empty partition set if unsatisfiable.
		explicit SolutionSet(bool s=false);

		// Return true iff the solution set is satisfiable which is
		// indicated by whether it is empty or not.
		bool is_satisfiable() const;
	};

	// Mapping from Handle (typically a variable) to a contextual handle
	typedef std::map<Handle, CHandle> HandleCHandleMap;

	// Subtitution values and their corresponding variable declaration
	// after substitution (cause some values may be variables).
	//
	// TODO: maybe we could simplify a great deal of code by replacing
	// Handle by Variables.
	typedef std::map<HandleCHandleMap, Handle> TypedSubstitutions;
	typedef TypedSubstitutions::value_type TypedSubstitution;

	/**
	 * Ctor. Initialize for the unification of lhs and rhs, with
	 * respective variable declarations lhs_vardecl and rhs_vardecl.
	 */
	Unify(const Handle& lhs, const Handle& rhs,
	      const Handle& lhs_vardecl=Handle::UNDEFINED,
	      const Handle& rhs_vardecl=Handle::UNDEFINED);

	/**
	 * Generate typed substitution rules, given a satisfiable
	 * SolutionSet and the term from which to select the variables as
	 * values in case multiple choices are possible.
	 */
	TypedSubstitutions typed_substitutions(const SolutionSet& sol,
	                                       const Handle& pre) const;

	/**
	 * Like typed_substitutions but generate a single typed
	 * substitution of a supposedly satisfiable partition.
	 */
	TypedSubstitution typed_substitution(const Partition& partition,
	                                     const Handle& pre) const;

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
	 * Calculate the variable declaration of the underlying new scope
	 * link after substitution would have occured given a substitution
	 * mapping.
	 */
	Handle substitution_vardecl(const HandleCHandleMap&	var2val) const;

	/**
	 * If the quotations are useless or harmful, which might be the
	 * case if they deprive a ScopeLink from hiding supposedly hidden
	 * variables, consume them. See RewriteLink::consume_quotations
	 * comment for more details.
	 */
	static BindLinkPtr consume_quotations(BindLinkPtr bl);

	/**
	 * Return true iff the handle or type correspond to a pattern
	 * matcher connector.
	 */
	static bool is_pm_connector(const Handle& h);
	static bool is_pm_connector(Type t);

	/**
	 * Given a typed substitution, perform the substitution over a scope
	 * link (for now only BindLinks are supported).
	 *
	 * If an atomspace is provided then remove constant clauses
	 * present in the atomspace.
	 */
	static Handle substitute(BindLinkPtr bl,
	                         const TypedSubstitution& ts,
	                         const AtomSpace* queried_as=nullptr);

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
	 * If an atomspace is provided then remove constant clauses
	 * present in the atomspace.
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
	 *
	 * TODO: replace by RewriteLink methods!
	 */
	static Handle substitute(BindLinkPtr bl, const HandleMap& var2val,
	                         Handle vardecl=Handle::UNDEFINED,
	                         const AtomSpace* queried_as=nullptr);

	/**
	 * Substitute the variable declaration of a BindLink. Remove
	 * variables that are substituted by values. If all variables are
	 * removed it returns Handle::UNDEFINED.
	 *
	 * TODO: replace by RewriteLink methods!
	 */
	static Handle substitute_vardecl(const Handle& vardecl,
	                                 const HandleMap& var2val);

	/**
	 * Given a pattern term (a conjunction of clauses) and a variable
	 * declaration, remove the constant clauses. If all clauses are
	 * constants then return an empty AndLink.
	 *
	 * If an atomspace is provided then check that the constant is in
	 * the atomspace as well, otherwise do not remove it.
	 *
	 * The variable declaration is assumed defined. That is if there
	 * are no variable, rather than Handle::UNDEFINED the vardecl will
	 * have to be a empty VariableList. That is because an undefined
	 * variable declaration is ambiguous as we don't know what whether
	 * it means empty or containing all free variables.
	 */
	static Handle remove_constant_clauses(const Handle& vardecl,
	                                      const Handle& clauses,
	                                      const AtomSpace* queried_as=nullptr);

	/**
	 * Perform unification by recursively
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
	SolutionSet operator()();

private:
	// Terms to unify
	Handle _lhs;
	Handle _rhs;

	// Common variable declaration of the two terms to unify.
	Variables _variables;

public:                         // ???? It's a friend yet
	/**
	 * Set Unify::_variables given the variable declarations of the
	 * two terms to unify.
	 */
	void set_variables(const Handle& lhs, const Handle& rhs,
	                   const Handle& lhs_vardecl=Handle::UNDEFINED,
	                   const Handle& rhs_vardecl=Handle::UNDEFINED);

private:
	/**
	 * Find the least abstract atom in the given block.
	 */
	CHandle find_least_abstract(const TypedBlock& block, const Handle& pre) const;

	/**
	 * Unify lhs and rhs. _lhs_vardecl and _rhs_vardecl should be set
	 * prior to run this method.
	 *
	 * Could probably optimize by memoizing unify, due to subunify
	 * being called redundantly.
	 */
	SolutionSet unify(const CHandle& lhs, const CHandle& rhs) const;
	SolutionSet unify(const Handle& lhs, const Handle& rhs,
	                  Context lhs_context=Context(),
	                  Context rhs_context=Context()) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, considering
	 * all permutations.
	 */
	SolutionSet unordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
	                            Context lhs_context=Context(),
	                            Context rhs_context=Context()) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, in the
	 * provided order.
	 */
	SolutionSet ordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
	                          Context lhs_context=Context(),
	                          Context rhs_context=Context()) const;

	/**
	 * Unify all pairs of CHandles.
	 */
	SolutionSet pairwise_unify(const std::set<CHandlePair>& pchs) const;

	/**
	 * Unify all elements of lhs with all elements of rhs, considering
	 * all pairwise combinations.
	 */
	SolutionSet comb_unify(const std::set<CHandle>& lhs,
	                       const std::set<CHandle>& rhs) const;

	/**
	 * Unify all pairs of elements in chs.
	 */
	SolutionSet comb_unify(const std::set<CHandle>& chs) const;

	/**
	 * Return if the atom is an unordered link.
	 */
	bool is_unordered(const Handle& h) const;

	/**
	 * Return a copy of a HandleSeq with the ith element removed.
	 */
	HandleSeq cp_erase(const HandleSeq& hs, Arity i) const;

	/**
	 * Build elementary solution set between 2 atoms given that at
	 * least one of them is a variable.
	 *
	 * Passed by copy because this method may use and modify the copy.
	 */
	SolutionSet mkvarsol(CHandle lhs, CHandle rhs) const;

public:                         // TODO: being friend with UnifyUTest
                                // somehow doesn't work
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
	SolutionSet join(const SolutionSet& lhs, const Partition& rhs) const;

	/**
	 * Join 2 partitions. The result can be set of partitions (see
	 * join(const Partition&, const TypedBlock&) for explanation).
	 */
	SolutionSet join(const Partition& lhs, const Partition& rhs) const;

	/**
	 * Join a block with a partition set. The partition set is assumed
	 * non empty and satisfiable.
	 */
	SolutionSet join(const SolutionSet& sol, const TypedBlock& block) const;

	/**
	 * Join a partition and a block. If the block has no element in
	 * common with any block of the partition, merely insert
	 * it. Otherwise fuse the blocks with common elements into
	 * one. During this fusion new unification problems may arise
	 * (TODO: explain why) thus possibly multiple partitions will be
	 * returned.
	*/
	SolutionSet join(const Partition& partition, const TypedBlock &block) const;

	/**
	 * Join a block to a partition to form a single block. It is
	 * assumed that all blocks have elements in common.
	*/
	TypedBlock join(const TypedBlockSeq& common_blocks,
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
	 */
	SolutionSet subunify(const TypedBlockSeq& common_blocks,
	                     const TypedBlock& block) const;

	/**
	 * Unify all terms that are not in the intersection of blocks lhs
	 * and rhs.
	 */
	SolutionSet subunify(const TypedBlock& lhs, const TypedBlock& rhs) const;

	/**
	 * Return true if a unification block is satisfiable. A unification
	 * block is not satisfiable if it's type is undefined (bottom).
	 */
	bool is_satisfiable(const TypedBlock& block) const;

	/**
	 * Calculate type intersection.
     *
     * What we would like:
     * ==================
     *
     * For example: say you have for a block
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
     * What we have:
     * ============
     *
     * The type is represented by the term itself. For instance, to
	 * take the example above, the terms and their types are
     *
	 * X:X
	 * ListLink(Y):ListLink(Y)
	 * ListLink(Z):ListLink(Z)
     *
     * and their intersection is the most restricted one, for that one
     * looks at the type declarations of X, Y and Z. So assuming that
     * Y has type Node and Z has type ConceptNode, then Z is most
     * resticted, so the result of their intersection will be
     *
     * ListLink(Z)
     *
     * In case the intersection is empty, then Handle::UNDEFINED is
     * returned.
     *
     * A contextual handle is returned to keep track of scoped
     * variable, which are essentially considered as constant.
	 */
public:
	CHandle type_intersection(const CHandle& lch, const CHandle& rch) const;
private:

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
	TypeSet simplify_type_union(TypeSet& type) const;

	/**
	 * Return the union type of a variable. If the variable
	 * declaration is empty (Handle::UNDEFINED) then the union type is
	 * not empty, instead it contains the singleton {ATOM}. An empty
	 * union type would instead mean the bottom type (that nothing can
	 * inherit).
	 */
	TypeSet get_union_type(const Handle& h) const;

	/**
	 * Return true if lhs inherit rhs. If lhs is not a variable then it
	 * relays that to VariableList::is_type, otherwise their type
	 * declarations are compared.
	 */
	bool inherit(const CHandle& lhs, const CHandle& rhs) const;
	bool inherit(const Handle& lhs, const Handle& rhs,
	             Context lc=Context(), Context rc=Context()) const;

	/**
	 * Return true if lhs inherits rhs.
	 */
	bool inherit(Type lhs, Type rhs) const;

	/**
	 * Return true if a type inherits a type union.
	 */
	bool inherit(Type lhs, const TypeSet& rhs) const;

	/**
	 * Return true if lhs inherits rhs. That is if all elements of lhs
	 * inherits rhs.
	 */
	bool inherit(const TypeSet& lhs, const TypeSet& rhs) const;
};

bool unifiable(const Handle& lhs, const Handle& rhs,
               const Handle& lhs_vardecl=Handle::UNDEFINED,
               const Handle& rhs_vardecl=Handle::UNDEFINED);

/**
 * Till content equality between atoms become the default.
 */
bool hm_content_eq(const HandleMap& lhs, const HandleMap& rhs);
bool hchm_content_eq(const Unify::HandleCHandleMap& lhs,
                     const Unify::HandleCHandleMap& rhs);
bool ts_content_eq(const Unify::TypedSubstitution& lhs,
                   const Unify::TypedSubstitution& rhs);
bool tss_content_eq(const Unify::TypedSubstitutions& lhs,
                    const Unify::TypedSubstitutions& rhs);

/**
 * Strip the context from hchm. Add quotation links when necessary.
 */
HandleMap strip_context(const Unify::HandleCHandleMap& hchm);

/**
 * Generate a VariableList of the free variables of a given contextual
 * atom ch.
 */
VariableListPtr gen_varlist(const Unify::CHandle& ch);

/**
 * Merge two vardecls into one. If a variable is present in both
 * vardecls then the more restrictive one replaces the less
 * restrictive one.
 *
 * TODO: give example.
 */
Variables merge_variables(const Variables& lv, const Variables& rv);
Handle merge_vardecl(const Handle& l_vardecl, const Handle& r_vardecl);

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const Unify::CHandle& ch,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Block& pb,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Partition& hshm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedBlock& tb,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedBlockSeq& tbs,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Partitions& par,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::HandleCHandleMap& hchm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedSubstitution& ts,
                         const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedSubstitutions& tss,
                         const std::string& indent=empty_string);
	
} // namespace opencog

#endif // _OPENCOG_UNIFY_UTILS_H
