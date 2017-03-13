/*
 * PatternTerm.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 * All Rights Reserved
 *
 * Created by Jacek Świergocki <jswiergo@gmail.com> July 2015
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

#ifndef _OPENCOG_PATTERN_TERM_H
#define _OPENCOG_PATTERN_TERM_H

#include <vector>

#include <opencog/util/Logger.h>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Quotation.h>

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
 * Term trees are build in the course of preprocessing stage before
 * pattern matcher starts searching for variable groundings. Each
 * clause is then recursively traversed from the root downwards,
 * through outgoing atoms. The _outgoing attribute stores a list of
 * the children created during this traversal. It corresponds
 * one-to-one to outgoing sets of the referenced atom.
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

	void addOutgoingTerm(const PatternTermPtr& ptm);

	Handle getHandle();

	PatternTermPtr getParent();

	PatternTermSeq getOutgoingSet() const;

	Arity getArity() const;

	Quotation& getQuotation();
	const Quotation& getQuotation() const;
	bool isQuoted() const;

	bool hasAnyBoundVariable() const;

	PatternTermPtr getOutgoingTerm(Arity pos) const;

	void addBoundVariable();

	// Work around gdb's incapability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 for more
	// explanation.
	std::string toString() const;
	std::string toString(std::string indent) const;
};

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const PatternTerm& pt);
std::string oc_to_string(const PatternTermPtr& pt_ptr);

} // namespace opencog

using namespace opencog;

namespace std {

/**
 * We need to overload standard comparison operator for PatternTerm pointers.
 * Now we do not care much about complexity of this comparison. The cases of
 * queries having repeated atoms that are deep should be very rare. So we just
 * traverse up towards root node. Typically we compare only the first level
 * handles on this path.
 */
template<>
struct less<PatternTermPtr>
{
	bool operator()(const PatternTermPtr& lhs, const PatternTermPtr& rhs) const
	{
		const Handle& lHandle = lhs->getHandle();
		const Handle& rHandle = rhs->getHandle();
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
