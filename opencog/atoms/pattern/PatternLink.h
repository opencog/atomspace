/*
 * opencog/atoms/pattern/PatternLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
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

#ifndef _OPENCOG_PATTERN_LINK_H
#define _OPENCOG_PATTERN_LINK_H

#include <unordered_map>

#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/PrenexLink.h>
#include <opencog/atoms/pattern/Pattern.h>

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
class PatternLink;
LINK_PTR_DECL(PatternLink)
class PatternLink : public PrenexLink
{
protected:
	// The pattern that is specified by this link.
	Pattern _pat;

	/// The graph components. Set by validate_clauses().
	///
	/// The `_fixed` field is used as a temporary, to accumulate terms
	/// that can be used to determine graph connectivity. For the most
	/// part, they consist of terms that must be literally present in
	/// order to be satisfied. With the exception of IdenticalLink, the
	/// fixed clauses are never virtual or evaluatable. (Identical links
	/// never split graph connecivity, as one 'side' can always be traced
	/// into the other.)
	///
	/// The `_fixed` field is cleared after connectivity is determined.
	///
	/// "virtual" clauses are those that contain virtual links.
	/// They are always evaluatable, i.e. are usually never found
	/// in the AtomSpace in their grounded form. If the only
	/// connection between different parts of the pattern are virtual
	/// clauses, then the pattern will split into multiple components,
	/// each of which must be grounded separately, and then assembled
	/// by determining if the virtual clasues that tie them together
	/// are true or not.
	///
	PatternTermSeq _fixed;
	size_t _num_virts;
	HandleSeq _virtual;

	/// The list of component_vars are the variables that appear
	/// in the corresponding component.
	size_t _num_comps;
	HandleSeqSeq _components;
	HandleSetSeq _component_vars;
	HandleSeq _component_patterns;

	PatternTermPtr make_term_tree(const Handle&);
	void make_term_tree_recursive(const PatternTermPtr&,
	                              PatternTermPtr&);

	void pin_term(const PatternTermPtr&);
	void pin_term_recursive(const PatternTermPtr&,
	                        const PatternTermPtr&);

	void record_mandatory(const PatternTermPtr&);
	bool record_literal(const PatternTermPtr&, bool reverse=false);
	void unbundle_clauses(const Handle& body);
	bool unbundle_clauses_rec(const PatternTermPtr&,
	                          const TypeSet&,
	                          bool reverse=false);

	void locate_defines(const PatternTermSeq& clauses);
	void validate_variables(HandleSet& vars,
	                        const HandleSeq& clauses);

	bool is_virtual(const Handle&);

	void locate_cacheable(const PatternTermSeq& clauses);

	bool need_dummies(const PatternTermPtr&);
	bool add_unaries(const PatternTermPtr&);
	void add_dummies(const PatternTermPtr&);

	void make_connectivity_map(void);
	void make_map_recursive(const Handle&, const PatternTermPtr&);
	void check_connectivity(const HandleSeqSeq&);
	void check_satisfiability(const HandleSet&,
	                          const HandleSetSeq&);

	void get_clause_variables(const PatternTermPtr&);
	void clauses_get_variables(const PatternTermSeq&);

	void init(void);
	void init_bottom(void);
	void common_init(void);
	void disjointed_init(void);
	void setup_components(void);

protected:
	// utility debug print
	static void prt(const Handle& h)
	{
		printf("%s\n", h->to_short_string().c_str());
	}

public:
	PatternLink(const HandleSeq&&, Type=PATTERN_LINK);
	PatternLink(const Handle& body);
	PatternLink(const Handle& varcdecls, const Handle& body);
	PatternLink(const Variables&, const Handle&);

	PatternLink(const PatternLink&) = delete;
	PatternLink& operator=(const PatternLink&) = delete;

	// Used only to set up multi-component links.
	// DO NOT call this! (unless you are the component handler).
	PatternLink(const HandleSet& vars,
	            const Variables& varspec,
	            const HandleSeq& component,
	            const PatternTermSeq& absents);

	// A backwards-compatibility constructor. Do not use.
	PatternLink(const HandleSet&,
	            const HandleSeq&);

	// Runtime just-in-time analysis
	PatternLinkPtr jit_analyze(void);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _variables; }
	const Pattern& get_pattern(void) const { return _pat; }

	const HandleSeqSeq& get_components(void) const { return _components; }
	const HandleSeq& get_component_patterns(void) const
		{ return _component_patterns; }

	// Return the list virtual clauses we are holding.
	const HandleSeq& get_virtual(void) const { return _virtual; }

	void debug_log(std::string) const;

	static Handle factory(const Handle&);

	// For printing not only the link itself but all the associated
	// C++ attributes
	std::string to_long_string(const std::string& indent) const;
};

#define createPatternLink CREATE_DECL(PatternLink)

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const PatternLink& pl,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_PATTERN_LINK_H
