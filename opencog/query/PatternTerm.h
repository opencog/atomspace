/*
 * PatternTerm.h
 *
 * Copyright (C) 2015 OpenCog Foundation
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
 *
 * Created by Jacek Świergocki <jswiergo@gmail.com> July 2015
 */

#ifndef _OPENCOG_PATTERN_TERM_H
#define _OPENCOG_PATTERN_TERM_H

#include <vector>

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/Link.h>

namespace opencog {

/*
 * PatternTerm class is used by pattern matcher to navigate through the
 * pattern query. PatternTerm is a node of tree data structure. Pattern
 * matcher searches for solutions by traversing this tree structure.
 *
 * Each pattern term corresponds to one atom in the query. The relation
 * beetween pattern terms and atoms is one-to-many, because atoms may repeat
 * in the query in many positions. The _handle attribute points to the
 * corresponding atom of the query. Roots of the pattern term trees reference
 * to the roots of query clauses. Term tree roots have its _parent attributes
 * UNDEFINED.
 *
 * Term trees are build in the course of preprocessing stage before
 * pattern matcher starts searching for variable groundigns. Each clause
 * is then recursively traversed from the root downwards through outgoing
 * atoms. The _outgoing attribute stores a list of childs created during this
 * traversal. They corresponds one-to-one to outgoing sets of referenced
 * atoms.
 */

class PatternTerm;
typedef std::shared_ptr<PatternTerm> PatternTermPtr;
typedef std::vector<PatternTermPtr> PatternTermSeq;

class PatternTerm
{
	protected:
		Handle _handle;
		PatternTermPtr _parent;
		PatternTermSeq _outgoing;

	public:
		static const PatternTermPtr UNDEFINED;

		PatternTerm()
		{
			_handle = Handle::UNDEFINED;
			_parent = PatternTerm::UNDEFINED;
		}

		PatternTerm(const PatternTermPtr& parent, const Handle& h)
		{
			_parent = parent;
			_handle = h;
		}

		void addOutgoingTerm(const PatternTermPtr& ptm)
		{
			_outgoing.push_back(ptm);
		}

		inline Handle getHandle()
		{
			return _handle;
		};
	
		inline PatternTermPtr getParent()
		{
			return _parent;
		};

		inline const PatternTermSeq& getOutgoingSet() const
		{
			return _outgoing;
		}

		inline Arity getArity() const {
			return _outgoing.size();
		}

		inline PatternTermPtr getOutgoingTerm(Arity pos) const
			throw (RuntimeException)
		{
			// Checks for a valid position
			if (pos < getArity()) {
				return _outgoing[pos];
			} else {
				throw RuntimeException(TRACE_INFO,
				                       "invalid outgoing set index %d", pos);
			}
		}

		inline std::string toString(std::string indent = ":") const
		{
			if (_handle == Handle::UNDEFINED) return "-";
			std::string str = _parent->toString();
			str += indent + std::to_string(_handle.value());
			return str;
		}

};

} // namespace opencog

using namespace opencog;

namespace std {

/*
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
			if (lHandle == Handle::UNDEFINED) return false;
			return lhs->getParent() < rhs->getParent();
		}
		return lHandle < rHandle;
	}

};

}; // namespace std;

#endif // _OPENCOG_PATTERN_TERM_H
