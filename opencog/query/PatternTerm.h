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

};

} // namespace opencog

using namespace opencog;

namespace std {

// We need to overload standard comparison operator for PatternTerm pointers.
// Now we do not care much about complexity of this comparison. The cases of
// queries having repeated atoms that are deep should be very rare. So we just
// traverse up towards root node. Typically we compare only the first level
// handles on this path.
template<>
struct less<PatternTermPtr>
{
	bool operator()(const PatternTermPtr& lhs, const PatternTermPtr& rhs)
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
