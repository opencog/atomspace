/*
 * opencog/atomspace/TypeIndex.cc
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

#include "TypeIndex.h"
#include <opencog/atoms/atom_types/NameServer.h>

using namespace opencog;

TypeIndex::TypeIndex(void) :
	_nameserver(nameserver())
{
	resize();
}

void TypeIndex::resize(void)
{
	_num_types = nameserver().getNumberOfClasses();
	_idx.resize(_num_types + 1);
}

// ================================================================

TypeIndex::iterator TypeIndex::begin(Type t, bool sub) const
{
	iterator it(t, sub);
	it.send = _idx.end();

	// A subclass of t is NEVER smaller than t.
	// Thus, we can start our search there.
	it.s = _idx.begin();
	it.s += t;
	it.currtype = t;
	it.se = it.s->begin();

	// If its not empty, then go for it.
	if (it.se != it.s->end()) return it;

	// If its the emptyset, and we are not subclassing, then we're done.
	if (not sub)
	{
		it.s = it.send;
		return it;
	}

	// Find the first type which is a subtype, and is not empty.
	++it.s;
	it.currtype++;
	while (it.s != it.send)
	{
		if (nameserver().isA(it.currtype, it.type))
		{
			it.se = it.s->begin();
			if (it.se != it.s->end()) return it;
		}
		it.currtype++;
		++it.s;
	}

	return it;
}

TypeIndex::iterator TypeIndex::end(void) const
{
	iterator it(_num_types, false);
	it.se = _idx.at(_num_types).end();
	it.s = _idx.end();
	it.send = _idx.end();
	it.currtype = _num_types;
	return it;
}

TypeIndex::iterator::iterator(Type t, bool sub)
{
	type = t;
	subclass = sub;
}

TypeIndex::iterator& TypeIndex::iterator::operator=(iterator v)
{
	s = v.s;
	send = v.send;
	se = v.se;
	currtype = v.currtype;
	type = v.type;
	subclass = v.subclass;
	return *this;
}

Handle TypeIndex::iterator::operator*(void)
{
	if (s == send) return Handle::UNDEFINED;
	return *se;
}

bool TypeIndex::iterator::operator==(iterator v)
{
	if ((v.s == v.send) && (s == send)) return true;
	return v.se == se;
}

bool TypeIndex::iterator::operator!=(iterator v)
{
	if ((v.s == v.send) && (s != send)) return v.se != se;
	if ((v.s != v.send) && (s == send)) return v.se != se;
	return false;
}

TypeIndex::iterator& TypeIndex::iterator::operator++()
{
	return operator++(1);
}

// XXX this is broken, for i != 1 ... FIXME.
TypeIndex::iterator& TypeIndex::iterator::operator++(int i)
{
	if (s == send) return *this;

	++se;
	if (se == s->end())
	{
		// If we are not subclassing, then we are really really done.
		if (not subclass)
		{
			s = send;
			return *this;
		}

		// Otherwise, move on to the next type.
		do
		{
			++s;
			currtype++;

			// Find the first type which is a subtype, and start iteration there.
			if (nameserver().isA(currtype, type))
			{
				se = s->begin();
				if (se != s->end()) return *this;
			}
		} while (s != send);
	}

	return *this;
}

// ================================================================
