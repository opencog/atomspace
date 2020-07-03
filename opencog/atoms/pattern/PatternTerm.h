/*
 * PatternTerm.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 * All Rights Reserved
 *
 * Created by Jacek Świergocki <jswiergo@gmail.com> July 2015
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#ifndef _OPENCOG_PATTERN_TERM_H
#define _OPENCOG_PATTERN_TERM_H

#include <vector>

#include <opencog/util/Logger.h>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Quotation.h>

namespace opencog {

/**
 * The PatternTerm class is used to provide a "context" for an atom.
 * This context is used to determine if the atom is quoted or not.
 * That is, whether any atom may be quoted or not quoted depends on
 * the holders of that atom.  Context is also required if a given
 * atom appears in multiple locations in a pattern, especially if
 * those locations are in unordered links. This additional context
 * is needed to traverse unordered links correctly.
 *
 * Pattern trees are converted to PatternTerm trees during pattern
 * pre-processing.  For a given query pattern, specified as an atom, an
 * equivalent PatternTerm tree is constructed.  The PatternTerm tree
 * differs from the original tree in that any atom that appears two or
 * more times in the query is "disambiguated", and given a distinct,
 * unique PatternTerm that corresponds to it.  Thus, each instance of
 * PatternTerm corresponds to one atom in at one fixed location in the
 * pattern.  That is, the relation between instances of PatternTerm and
 * atoms is many-to-one, because a given atom may occur in the pattern
 * in several positions.
 *
 * For example, given the query
 *    SetLink
 *        VariableNode $a
 *        BlahBlahLink
 *             VariableNode $a
 *             VariableNode $b
 *
 * The VariableNode $a occurs twice, in two different locations. Each
 * location will get it's own unique instance of PatternTerm.
 *
 * For a given term in the pattern, the _handle attribute points to the
 * corresponding atom of the query.  Roots of the PatternTerm trees
 * reference the roots of query clauses. Term tree roots have its
 * _parent attributes UNDEFINED.
 *
 * Term trees are built by the PatternLink constructor, which acts as
 * a pre-processor or a "pattern compiler", analyzing the pattern and
 * generating additional structures that allow the pattern matcher to
 * run quickly and efficiently.
 */

class PatternTerm;
typedef std::shared_ptr<PatternTerm> PatternTermPtr;
typedef std::weak_ptr<PatternTerm> PatternTermWPtr;
typedef std::vector<PatternTermPtr> PatternTermSeq;
typedef std::vector<PatternTermWPtr> PatternTermWSeq;
typedef std::set<PatternTermPtr> PatternTermSet;

class PatternTerm
	: public std::enable_shared_from_this<PatternTerm>
{
protected:
	Handle _handle;
	Handle _quote;

	// TODO: it would probably be more efficient to swap which of these
	// two is weak, since I think _outgoing is requested far more often
	// than _parent, and having it run faster would be a performance win.
	PatternTermPtr _parent;
	PatternTermWSeq _outgoing;

	// Quotation level and local quotation
	Quotation _quotation;

	// True if any pattern subtree rooted in this tree node contains
	// a variable bound to the search pattern. Trees without any bound
	// search variables are constants, and are satisfied by themselves.
	bool _has_any_bound_var;

	// True if none of the outgoing set of this particular term are
	// variables bound to the search pattern. Unlike the above flag,
	// this flag is set for the immediate outgoing set only, and not
	// any deeper terms.  That is, deeper terms may contain bound
	// variables, but this flag will not be set.
	bool _has_bound_var;

	// As above, but zero terms deep. This one is the variable.
	bool _is_bound_var;

	// True if any pattern subtree rooted in this tree node contains
	// an GlobNode. Trees without any GlobNodes can be searched in a
	// straight-forward manner; those with them need to have all
	// possible glob matches explored.
	bool _has_any_globby_var;

	// As above, but only one level deep.
	bool _has_globby_var;

	// As above, but zero levels deep. This is a glob.
	bool _is_globby_var;

	// As above, but for evaluatables.
	bool _has_any_evaluatable;
	bool _has_evaluatable;

	// An evaluatable term, with two or more variables in it.
	// In general, these bridge across components.
	bool _is_virtual;

	// True if any pattern subtree rooted in this tree node contains
	// an unordered link. Trees without any unordered links can be
	// searched in a straight-forward manner; those with them need to
	// have all possible permutations explored.
	bool _has_any_unordered_link;

	// True if quoted, or if it should be taken literally, and not
	// evaluated or interpreted. Usually, this means that this term
	// is underneath a PresentLink, an AbsentLink, a ChoiceLink, or
	// a QuoteLink. This applies only to non-variables (as variables
	// are still variables, unless they are quoted or scope-hidden.)
	bool _is_literal;

	// True if this contains a set of subterms, all of which must be
	// simultaneously present in the pattern. All of the sub-terms are
	// necessarily literal. This corresponds to PRESENT_LINK in the
	// default interpretation.
	bool _is_present;

	// True if this is a term that must be absent in a given grounding.
	// This corresponds to the ABSENT_LINK in the default interpretation,
	// and is effectively the same thing as NOT_LINK(PRESENT_LINK).
	bool _is_absent;

	// True if this contains a set of subterms, one of which must be
	// present in the pattern. All of the sub-terms are present, or
	// are literal. This corresponds to CHOICE_LINK in the default
	// interpretation; it can also be an OR_LINK when that OR_LINK
	// is in a boolean evaluatable context.
	bool _is_choice;

	// True if this is a term that must be present in every successful
	// patten grounding. There are no groundings at all, unless this
	// term is in each and every one of them. This corresponds to
	// the ALWAYS_LINK in the default interpretation.
	bool _is_always;

	void addAnyBoundVar();
	void addAnyGlobbyVar();
	void addAnyEvaluatable();

public:
	static const PatternTermPtr UNDEFINED;

	PatternTerm(void);
	PatternTerm(const PatternTermPtr& parent, const Handle& h);

	const Handle& getHandle() const noexcept { return _handle; }

	PatternTermPtr getParent() const noexcept { return _parent; }
	bool isDescendant(const PatternTermPtr&) const;
	PatternTermPtr getRoot() noexcept {
		PatternTermPtr root = shared_from_this();
		while (root->_parent->_handle) root = _parent;
		return root;
	}

	PatternTermPtr addOutgoingTerm(const Handle&);
	PatternTermSeq getOutgoingSet() const;

	Arity getArity() const { return _outgoing.size(); }
	PatternTermPtr getOutgoingTerm(Arity pos) const;

	const Handle& getQuote() const noexcept {
		return (isQuoted() and nullptr != _quote) ?  _quote : _handle; }
	Quotation& getQuotation() { return _quotation; };
	const Quotation& getQuotation() const noexcept { return _quotation; }
	bool isQuoted() const { return _quotation.is_quoted(); }

	void markLiteral();
	bool isLiteral() const { return _is_literal; }

	void markPresent();
	bool isPresent() const { return _is_present; }

	void markAbsent();
	bool isAbsent() const { return _is_absent; }

	void markChoice();
	bool isChoice() const { return _is_choice; }

	void markAlways();
	bool isAlways() const { return _is_always; }

	void addBoundVariable();
	bool hasAnyBoundVariable() const noexcept { return _has_any_bound_var; }
	bool hasBoundVariable() const noexcept { return _has_bound_var; }
	bool isBoundVariable() const noexcept { return _is_bound_var; }

	void addGlobbyVar();
	bool hasAnyGlobbyVar() const noexcept { return _has_any_globby_var; }
	bool hasGlobbyVar() const noexcept { return _has_globby_var; }
	bool isGlobbyVar() const noexcept { return _is_globby_var; }

	void addEvaluatable();
	bool hasAnyEvaluatable() const noexcept { return _has_any_evaluatable; }
	bool hasEvaluatable() const noexcept { return _has_evaluatable; }

	void markVirtual();
	bool isVirtual() const noexcept { return _is_virtual; }

	void addUnorderedLink();
	bool hasUnorderedLink() const noexcept { return _has_any_unordered_link; }
	bool isUnorderedLink() const noexcept { return _handle->is_unordered_link(); }
	bool isLink() const noexcept { return _handle->is_link(); }

	bool operator==(const PatternTerm&);

	// Work around gdb's inability to build a string on the fly;
	// See http://stackoverflow.com/questions/16734783 for explanation.
	std::string to_string() const;
	std::string to_string(const std::string& indent) const;
	std::string to_short_string() const;
	std::string to_short_string(const std::string& indent) const;
	std::string to_full_string() const;
	std::string to_full_string(const std::string& indent) const;
	std::string flag_string() const;
};

#define createPatternTerm std::make_shared<PatternTerm>

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const PatternTerm& pt,
                         const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermPtr& pt,
                         const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermSeq& pt,
                         const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermSet& pt,
                         const std::string& indent=empty_string);

} // namespace opencog

namespace std {

/**
 * Overload the standard comparison operator for PatternTerm pointers.
 * This uses content-based compare for individual atoms, and then moving
 * up the term inclusion path, if the atoms are identical. Thus, this
 * is almost the same as `std::less<Handle>` but not quite.
 *
 * This is needed for std::map<PatternTermPtr, ...> and similar.
 * This is performance-sensitive; it is used during pattern matching
 * to walk over permutations of unordered links.
 */
template<>
struct less<opencog::PatternTermPtr>
{
	bool operator()(const opencog::PatternTermPtr& lhs, const opencog::PatternTermPtr& rhs) const
	{
		const opencog::Handle& lHandle = lhs->getHandle();
		const opencog::Handle& rHandle = rhs->getHandle();
		if (lHandle == rHandle)
		{
			if (not lHandle) return false;
			return lhs->getParent() < rhs->getParent();
		}
		return lHandle < rHandle;
	}

};

template<>
struct hash<opencog::PatternTermPtr>
{
	std::size_t
	operator()(const opencog::PatternTermPtr& ptm) const noexcept
	{ return ptm->getHandle()->get_hash(); }
};

template<>
struct equal_to<opencog::PatternTermPtr>
{
	bool
	operator()(const opencog::PatternTermPtr& lptm,
	           const opencog::PatternTermPtr& rptm) const noexcept
	{ return lptm->operator==(*rptm); }
};

/** Needed for std::unordered_map<PatternTermSeq, ...> and similar. */
template<>
struct hash<opencog::PatternTermSeq>
{
	std::size_t
	operator()(const opencog::PatternTermSeq& seq) const noexcept
	{
		std::size_t hash = 0;
		for (const opencog::PatternTermPtr& ptm : seq)
			hash += ptm->getHandle()->get_hash();
		return hash;
	}
};

template<>
struct equal_to<opencog::PatternTermSeq>
{
	bool
	operator()(const opencog::PatternTermSeq& lseq,
	           const opencog::PatternTermSeq& rseq) const noexcept
	{
		size_t lsz = lseq.size();
		if (lsz != rseq.size()) return false;
		for (size_t i=0; i<lsz; i++)
		{
			const opencog::PatternTermPtr& lptm(lseq[i]);
			const opencog::PatternTermPtr& rptm(rseq[i]);
			if (not lptm->operator==(*rptm)) return false;
		}
		return true;
	}
};

}; // namespace std;

#endif // _OPENCOG_PATTERN_TERM_H
