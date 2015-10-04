/*
 * opencog/atomspace/Handle.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013 Linas Vepstas <linas@linas.org>
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
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

#include <climits>
#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/AtomTable.h>

using namespace opencog;

const Handle Handle::UNDEFINED;
const AtomPtr Handle::NULL_POINTER;

Handle::Handle(const UUID u)
{
	_ptr = do_res(u)._ptr;
}

UUID Handle::value(void) const
{
    const Atom* a = operator->();
    if (a) return a->getUUID();
    return ULONG_MAX;
}

Atom* Handle::operator->()
{
	ProtoAtom* pa = _ptr.get();
	return dynamic_cast<Atom*>(pa);
}

Atom* Handle::operator->() const
{
	ProtoAtom* pa = _ptr.get();
	return dynamic_cast<Atom*>(pa);
}

Handle::operator AtomPtr() const
{
	return std::dynamic_pointer_cast<Atom>(_ptr);
}

Handle::operator AtomPtr()
{
	return std::dynamic_pointer_cast<Atom>(_ptr);
}

// ===================================================
// Atom comparison.

#if 0
bool Handle::atoms_eq(const AtomPtr& a, const AtomPtr& b)
{
    if (a == b) return true;
    if (NULL == a or NULL == b) return false;
    return *a == *b;
}
#endif

bool Handle::atoms_less(const ProtoAtomPtr& pa, const ProtoAtomPtr& pb)
{
    if (pa == pb) return false;
    if (NULL == pa) return true;
    if (NULL == pb) return false;

    AtomPtr a(std::dynamic_pointer_cast<Atom>(pa));
    AtomPtr b(std::dynamic_pointer_cast<Atom>(pb));
    UUID ua = a->getUUID();
    UUID ub = b->getUUID();
    if (INVALID_UUID != ua or INVALID_UUID != ub) return ua < ub;

    // If both UUID's are invalid, we still need to compare
    // the atoms somehow. The need to compare in some "reasonable"
    // way, so that std::set<Handle> works correctly when it uses
    // the sttd::less<Handle> operator, which calls this function.
    // Performing an address-space comparison is all I can think
    // of...
    // if (*a == *b) return false; lets not do this cpu-time-waster...
    return a.get() < b.get();
}

// ===================================================
// Handle resolution stuff.

// Its a vector, not a set, because its priority ranked.
std::vector<const AtomTable*> Handle::_resolver;

void Handle::set_resolver(const AtomTable* tab)
{
    _resolver.push_back(tab);
}

void Handle::clear_resolver(const AtomTable* tab)
{
    auto it = std::find(_resolver.begin(), _resolver.end(), tab);
    if (it != _resolver.end())
        _resolver.erase(it);
}

// Search several atomspaces, in order.  First one to come up with
// the atom wins.  Seems to work, for now.
inline Handle Handle::do_res(UUID uuid)
{
    for (const AtomTable* at : _resolver) {
        Handle h(at->getHandle(uuid));
        if (NULL != h) return h;
    }
    return Handle();
}
