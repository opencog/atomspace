/*
 * opencog/atoms/PatternLink.h
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

#include <opencog/query/Pattern.h>
#include <opencog/atoms/bind/VariableList.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/// The PatternLink specifies an (optional) list of variables, and a
/// pattern (containing those variables) that is to be grounded
/// (satisfied).  If no list of variables is specified, then all free
/// variables are extracted and used for grounding.
///
/// The body of the PatternLink is assumed to collection of clauses
/// to be satsified. Thus, the body is typically an AndLink, OrLink
/// or a SequentialAnd, depending on how they are to be satsified.
/// This is very much like a SatisfactionLink, with one exception:
/// NONE OF THE CLAUSES MAY BE VIRTUAL!! This restriction is
/// "artificial", in that it simplfies how pattern matching is done.
/// Thus, this class is not really intended for "general use", but
/// is for internal use only.
///
/// Given the initial list of variables and clauses, the constructors
/// extract the optional clauses and the dynamically-evaluatable clauses.
/// This also computes the connectivity diagram of the clauses.
///
/// It is assumed that the set of clauses form a single, connected
/// component; i.e. that the clauses are pair-wise connected by common,
/// shared variables, and that this pair-wise connection extends over
/// the entire set of clauses. There is no other restriction on the
/// connection topology; they can form any graph whatsoever (as long as
/// it is connected).
///
/// The (cog-satisfy) scheme call can ground this link, and return
/// a truth value.
class PatternLink : public Link
{
protected:
	// The link to be grounded.
	Handle _body;

	// The variables to be grounded
	Variables _varlist;

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
	std::vector<HandleSeq> _components;
	std::vector<std::set<Handle>> _component_vars;
	HandleSeq _component_patterns;

	void extract_variables(const HandleSeq& oset);
	void unbundle_clauses(const Handle& body);
	void validate_clauses(std::set<Handle>& vars,
	                      HandleSeq& clauses);

	void extract_optionals(const std::set<Handle> &vars,
	                       const std::vector<Handle> &component);

	void unbundle_virtual(const std::set<Handle>& vars,
	                      const HandleSeq& clauses,
	                      HandleSeq& concrete_clauses,
	                      HandleSeq& virtual_clauses,
	                      std::set<Handle>& black_clauses);

	void trace_connectives(const std::set<Type>&,
	                       const HandleSeq& clauses);

	void make_connectivity_map(const HandleSeq&);
	void check_connectivity(const std::vector<HandleSeq>&);
	void make_map_recursive(const Handle&, const Handle&);

	void init(void);
	void common_init(void);
	void setup_components(void);

	// Only derived classes can call this
	PatternLink(Type, const HandleSeq&,
	            TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	            AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	// utility debug print
	static void prt(const Handle& h)
	{
		printf("%s\n", h->toShortString().c_str());
	}

public:
	PatternLink(const HandleSeq&,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	PatternLink(const Handle& varcdecls, const Handle& body,
	         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	         AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	PatternLink(Link &l);

	// Used only to set up multi-component links.
	// DO NOT call this!
	PatternLink(const std::set<Handle>& vars,
	             const VariableTypeMap& typemap,
	             const HandleSeq& component,
	             const std::set<Handle>& optionals);

	// Return the list of variables we are holding.
	const Variables& get_variables(void) const { return _varlist; }

	const Handle& get_body(void) const { return _body; }

	// XXX temp hack till things get sorted out; remove this method
	// later.
	const Pattern& get_pattern(void) { return _pat; }

	bool satisfy(PatternMatchCallback&) const;

	void debug_print(void) const;
};

typedef std::shared_ptr<PatternLink> PatternLinkPtr;
static inline PatternLinkPtr PatternLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<PatternLink>(a); }
static inline PatternLinkPtr PatternLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<PatternLink>(a); }

// XXX temporary hack ...
#define createPatternLink std::make_shared<PatternLink>

/** @}*/
}

#endif // _OPENCOG_PATTERN_LINK_H
