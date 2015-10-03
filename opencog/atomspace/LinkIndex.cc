/*
 * opencog/atomspace/LinkIndex.cc
 *
 * Copyright (C) 2008 Linas Vepstas <linasvepstas@gmail.com>
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

#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/LinkIndex.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/atom_types.h>

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

LinkIndex::LinkIndex(void)
{
	resize();
}

void LinkIndex::resize()
{
	idx.resize(classserver().getNumberOfClasses());
}

size_t LinkIndex::size() const
{
	size_t cnt = 0;
	for (HandleSeqIndex hsi: idx) cnt += hsi.size();
	return cnt;
}

void LinkIndex::insertAtom(const AtomPtr& a)
{
	Type t = a->getType();
	HandleSeqIndex &hsi = idx[t];

	LinkPtr l(LinkCast(a));
	if (NULL == l) return;

	hsi.insert(l->getOutgoingSet(), l.get());
}

void LinkIndex::removeAtom(const AtomPtr& a)
{
	Type t = a->getType();
	HandleSeqIndex &hsi = idx.at(t);

	LinkPtr l(LinkCast(a));
	if (NULL == l) return;

	hsi.remove(l->getOutgoingSet());
}

Handle LinkIndex::getHandle(Type t, const HandleSeq &seq) const
{
	const HandleSeqIndex &hsi = idx.at(t);
	Link* l = hsi.get(seq);
	if (l) return l->getHandle();
	return Handle::UNDEFINED;
}

void LinkIndex::remove(bool (*filter)(const Handle&))
{
	for (HandleSeqIndex s : idx)
		s.remove(filter);
}

UnorderedHandleSet LinkIndex::getHandleSet(Type type,
                          const HandleSeq& seq, bool subclass) const
{
	UnorderedHandleSet hs;
	if (subclass)
	{
		Type max = classserver().getNumberOfClasses();
		for (Type s = 0; s < max; s++)
		{
			// The 'AssignableFrom' direction is unit-tested in AtomSpaceUTest.cxxtest
			if (classserver().isA(s, type))
			{
				const HandleSeqIndex &hsi = idx.at(s);
				Link* l = hsi.get(seq);
				if (l)
					hs.insert(l->getHandle());
			}
		}
	}
	else
	{
		Handle h(getHandle(type, seq));
		if (Handle::UNDEFINED != h)
			hs.insert(h);
	}
	return hs;
}

// ================================================================
