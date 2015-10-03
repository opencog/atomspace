/*
 * opencog/atomspace/HandleSeqIndex.h
 *
 * Copyright (C) 2008,2009 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_HANDLE_SEQ_INDEX_H
#define _OPENCOG_HANDLE_SEQ_INDEX_H

#include <cstddef>
#include <map>

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Implements a Handle-sequence index as an RB-tree (C++ map)
 * That is, given a HandleSeq, it will return a (single) Handle
 * associated with that HandleSeq.  This map is in the "opposite"
 * direction from the HandleSetIndex.
 */
class HandleSeqIndex
{
	private:
		std::map<const HandleSeq*, Link*, handle_seq_ptr_less> idx;

	public:
		void insert(const HandleSeq& seq, Link* l)
		{
			idx.insert(std::pair<const HandleSeq*, Link*>(&seq, l));
		}
		Link* get(const HandleSeq& seq) const
		{
			std::map<const HandleSeq*, Link*>::const_iterator it;
			it = idx.find(&seq);
			if (it != idx.end()) return it->second;
			return nullptr;
		}
		void remove(const HandleSeq& seq)
		{
			idx.erase(&seq);
		}
		size_t size(void) const
		{
			return idx.size();
		}
		void remove(bool (*)(const Handle&));
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_HANDLE_SEQ_INDEX_H
