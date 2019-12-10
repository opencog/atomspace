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

class PatternTerm
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

	// True if the pattern subtree rooted in this tree node does not
	// contain any bound variables. This means that the term is constant
	// and may be self-grounded.
	bool _has_any_bound_var;

public:
	static const PatternTermPtr UNDEFINED;

	PatternTerm();

	PatternTerm(const PatternTermPtr& parent, const Handle& h);

	const Handle& getHandle() const noexcept { return _handle; }

	PatternTermPtr getParent() const noexcept { return _parent; }
	bool isDescendant(const PatternTermPtr&) const;

	void addOutgoingTerm(const PatternTermPtr& ptm);
	PatternTermSeq getOutgoingSet() const;

	Arity getArity() const { return _outgoing.size(); }
	PatternTermPtr getOutgoingTerm(Arity pos) const;

	const Handle& getQuote() const noexcept { return _quote; }
	Quotation& getQuotation() { return _quotation; };
	const Quotation& getQuotation() const noexcept { return _quotation; }
	bool isQuoted() const { return _quotation.is_quoted(); }

	void addBoundVariable();
	bool hasAnyBoundVariable() const noexcept { return _has_any_bound_var; }

	bool operator==(const PatternTerm&);

	// Work around gdb's inability to build a string on the fly;
	// See http://stackoverflow.com/questions/16734783 for explanation.
	std::string to_string() const;
	std::string to_string(const std::string& indent) const;
};

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const PatternTerm& pt,
                         const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermPtr& pt,
                         const std::string& indent=empty_string);

} // namespace opencog

namespace std {

/**
 * Overload the standard comparison operator for PatternTerm pointers.
 * This uses content-baed compare for individual atoms, and then moving
 * up the term inclusion path, if the atoms are identical. Thus, this
 * is almost the same as `std::less<Handle>` but not quite.
 *
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

}; // namespace std;

#endif // _OPENCOG_PATTERN_TERM_H
