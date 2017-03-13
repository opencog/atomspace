/*
 * opencog/atoms/pattern/PatternLink.h
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

#ifndef _OPENCOG_PATTERN_LINK_H
#define _OPENCOG_PATTERN_LINK_H

#include <unordered_map>

#include <opencog/atoms/base/Quotation.h>
#include <opencog/atoms/pattern/Pattern.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/// The PatternLink specifies an (optional) list of variables, and a
/// pattern (containing those variables) that is to be grounded
/// (satisfied).  If no list of variables is explicltly specified,
/// then the pattern is searched for all (free) variables, and these
/// then become implicitly bound.  When processed by the pattern
/// matcher, these are the variables that get grounded.
///
/// The body of the PatternLink is assumed to collection of clauses
/// to be satsified. Thus, the body is typically an AndLink, ChoiceLink
/// or a SequentialAndLink, depending on how they are to be satsified.
///
/// The PatternLink is used as a base class for GetLink, BindLink
/// and SatisfactionLink. It provides all the methods and tools needed
/// to unpack the clauses, extract variables, check for connectivity,
/// discover the evaluatable terms, and so on. The pattern matcher
/// needs all of these different parts unpacked into C++ structures
/// so that it can quickly, directly access them during traversal.
/// Thus, this class is designed to pre-process everything that the
/// pattern matcher will need to have later on; it also acts as a cache,
/// avoiding repeated unpacking, if the pattern is used more than once.
///
/// Given the initial list of variables and clauses, the constructors
/// extract the optional clauses and the dynamically-evaluatable clauses.
/// This also computes the connectivity diagram of the clauses.
/// Two clauses are considered to be pair-wise connected if they both
/// contain a common shared variable, AND that shared variable does
/// not appear in a "virtual link" (e.g. a GreaterThanLink, or an
/// EvaluationLink with a GroundedPredicateNode in it).  Virtual links
/// cannot be directly grounded by the pattern matcher (because they
/// don't actually "exist" as "real" atoms in the atomspace). Thus,
/// the code here splits up the pattern into multiple connected
/// components; the components themselves are connected only by
/// virtual links.
///
/// The (cog-satisfy) and (cog-execute!) scheme calls can ground this
/// link, and return a truth value.
class PatternLink;
typedef std::shared_ptr<PatternLink> PatternLinkPtr;
class PatternLink : public ScopeLink
{
protected:
	// The pattern that is specified by this link.
	Pattern _pat;

	/// The graph components. Set by validate_clauses()
	/// "virtual" clauses are those that contain virtual links.
	/// "fixed" clauses are those that do not.
	/// The list of component_vars are the variables that appear
	/// in the corresponding component.
	HandleSeq _fixed;
	size_t _num_virts;
	HandleSeq _virtual;

	size_t _num_comps;
	HandleSeqSeq _components;
	std::vector<OrderedHandleSet> _component_vars;
	HandleSeq _component_patterns;

	void unbundle_clauses(const Handle& body);
	void unbundle_clauses_rec(const std::set<Type>&,
	                          const HandleSeq&);

	void locate_defines(HandleSeq& clauses);
	void locate_globs(HandleSeq& clauses);
	void validate_clauses(OrderedHandleSet& vars,
	                      HandleSeq& clauses,
	                      HandleSeq& constants);

	void extract_optionals(const OrderedHandleSet &vars,
	                       const HandleSeq &component);

	void unbundle_virtual(const OrderedHandleSet& vars,
	                      const HandleSeq& clauses,
	                      HandleSeq& concrete_clauses,
	                      HandleSeq& virtual_clauses,
	                      OrderedHandleSet& black_clauses);

	bool add_dummies();

	void trace_connectives(const std::set<Type>&,
	                       const HandleSeq& clauses,
	                       Quotation quotation=Quotation());

	void make_connectivity_map(const HandleSeq&);
	void make_map_recursive(const Handle&, const Handle&);
	void check_connectivity(const HandleSeqSeq&);
	void check_satisfiability(const OrderedHandleSet&,
	                          const std::vector<OrderedHandleSet>&);

	void make_term_trees();
	void make_term_tree_recursive(const Handle&, Handle,
	                              PatternTermPtr&);

	void init(void);
	void common_init(void);
	void setup_components(void);

protected:
	// utility debug print
	static void prt(const Handle& h)
	{
		printf("%s\n", h->toShortString().c_str());
	}

public:
	PatternLink(const HandleSeq&, Type=PATTERN_LINK);
	PatternLink(const Handle& body);
	PatternLink(const Handle& varcdecls, const Handle& body);
	PatternLink(const Variables&, const Handle&);
	PatternLink(const Link &l);

	// Used only to set up multi-component links.
	// DO NOT call this! (unless you are the component handler).
	PatternLink(const OrderedHandleSet& vars,
	            const VariableTypeMap& typemap,
	            const HandleSeq& component,
	            const OrderedHandleSet& optionals);

	// A backwards-compatibility constructor. Do not use.
	PatternLink(const OrderedHandleSet&,
	            const HandleSeq&);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }
	const Pattern& get_pattern(void) const { return _pat; }

	// Return the list of fixed and virtual clauses we are holding.
	const HandleSeq& get_fixed(void) const { return _fixed; }
	const HandleSeq& get_virtual(void) const { return _virtual; }

	bool satisfy(PatternMatchCallback&) const;

	void debug_log(void) const;

	static Handle factory(const Handle&);
};

static inline PatternLinkPtr PatternLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<PatternLink>(a); }
static inline PatternLinkPtr PatternLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<PatternLink>(a); }

// XXX temporary hack ...
#define createPatternLink std::make_shared<PatternLink>

/** @}*/
}

#endif // _OPENCOG_PATTERN_LINK_H
