/*
 * opencog/atoms/base/Handle.cc
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
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
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

bool Handle::atoms_less(const Atom* pa, const Atom* pb)
{
    if (pa == pb) return false;
    if (NULL == pa) return true;
    if (NULL == pb) return false;

    const Atom* a(dynamic_cast<const Atom*>(pa));
    const Atom* b(dynamic_cast<const Atom*>(pb));
    UUID ua = a->getUUID();
    UUID ub = b->getUUID();
    if (INVALID_UUID != ua or INVALID_UUID != ub) return ua < ub;

    // If both UUID's are invalid, we still need to compare
    // the atoms somehow. The need to compare in some "reasonable"
    // way, so that std::set<Handle> works correctly when it uses
    // the std::less<Handle> operator, which calls this function.
    // Performing an address-space comparison is all I can think
    // of...
    // if (*a == *b) return false; lets not do this cpu-time-waster...
    return a < b;
}

bool Handle::content_based_atoms_less(const Atom* a, const Atom* b)
{
    if (a == b) return false;
    if (nullptr == a) return true;
    if (nullptr == b) return false;

    // Otherwise we compare them by content. This is expensive but
    // useful when you really want a deterministic comparison.
    return a->operator<(*b);
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

namespace std {

// Hack around the lack template use in Handle.h due to circular dependencies
#define GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(T) \
ostream& operator<<(ostream& out, const T& hs) { \
	size_t i = 0; \
	for (const Handle& h : hs) { \
		out << "atom[" << i << "]:" << endl; \
		if ((AtomPtr)h != nullptr) out << h->toString(); \
		i++; \
	} \
	return out; \
}
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::HandleSeq)
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::OrderedHandleSet)
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::UnorderedHandleSet)

string hs_to_string(const HandleSeq& hs)
{
	stringstream ss; ss << hs; return ss.str();
}
string ohs_to_string(const OrderedHandleSet& ohs)
{
	stringstream ss; ss << ohs; return ss.str();
}
string uhs_to_string(const UnorderedHandleSet& uhs)
{
	stringstream ss; ss << uhs; return ss.str();
}
std::string varmap_to_string(const HandleMap& varmap)
{
	stringstream ss;
	int i = 0;
	for (const auto& p : varmap) {
		ss << "key[" << i << "]:" << std::endl << p.first->toString()
		   << "value[" << i << "]:" << std::endl << p.second->toString();
		i++;
	}
	return ss.str();
}
std::string varmultimap_to_string(const HandleMultimap& varmultimap)
{
	stringstream ss;
	int i = 0;
	for (const auto& p : varmultimap) {
		ss << "key[" << i << "]:" << std::endl << p.first->toString()
		   << "value[" << i << "]:" << std::endl;
		for (const auto s : p.second)
			ss << s->toString();
		i++;
	}
	return ss.str();
}
std::string atomtype_to_string(Type type)
{
	return classserver().getTypeName(type);
}

} // ~namespace std

