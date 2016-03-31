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

namespace opencog {

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
    // way, so that OrderedHandleSet works correctly when it uses
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

std::string oc_to_string(const Handle& h)
{
	return std::h_to_string(h);
}
std::string oc_to_string(const OrderedHandleSet& hs)
{
	return std::ohs_to_string(hs);
}
std::string oc_to_string(const UnorderedHandleSet& uhs)
{
	return std::uhs_to_string(uhs);
}
std::string oc_to_string(const HandleMap& hm)
{
	return std::hmap_to_string(hm);
}
std::string oc_to_string(const HandleMultimap& hmm)
{
	return std::hmultimap_to_string(hmm);
}
std::string oc_to_string(const HandleMapSeq& hms)
{
	return std::hmaps_to_string(hms);
}
std::string oc_to_string(Type type)
{
	return classserver().getTypeName(type);
}

} // ~namespace opencog

namespace std {

// Hack around the lack template use in Handle.h due to circular dependencies
#define GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(T) \
ostream& operator<<(ostream& out, const T& hs) { \
	out << "size = " << hs.size() << endl; \
	size_t i = 0; \
	for (const opencog::Handle& h : hs) { \
		out << "atom[" << i << "]:" << endl << h_to_string(h); \
		i++; \
	} \
	return out; \
}
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::HandleSeq)
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::OrderedHandleSet)
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::UnorderedHandleSet)

string h_to_string(const opencog::Handle& h)
{
	if ((opencog::AtomPtr)h == nullptr)
		return "nullatom\n";
	else
		return h->toString();
}
string hs_to_string(const opencog::HandleSeq& hs)
{
	stringstream ss; ss << hs; return ss.str();
}
string ohs_to_string(const opencog::OrderedHandleSet& ohs)
{
	stringstream ss; ss << ohs; return ss.str();
}
string uhs_to_string(const opencog::UnorderedHandleSet& uhs)
{
	stringstream ss; ss << uhs; return ss.str();
}
string hmap_to_string(const opencog::HandleMap& hmap)
{
	stringstream ss;
	ss << "size = " << hmap.size() << std::endl;
	int i = 0;
	for (const auto& p : hmap) {
		ss << "key[" << i << "]:" << std::endl << h_to_string(p.first)
		   << "value[" << i << "]:" << std::endl << h_to_string(p.second);
		i++;
	}
	return ss.str();
}
string hmultimap_to_string(const opencog::HandleMultimap& hmultimap)
{
	stringstream ss;
	ss << "size = " << hmultimap.size() << std::endl;
	int i = 0;
	for (const auto& p : hmultimap) {
		ss << "key[" << i << "]:" << std::endl << h_to_string(p.first)
		   << "value[" << i << "]:" << std::endl;
		for (const auto s : p.second)
			ss << h_to_string(s);
		i++;
	}
	return ss.str();
}
string hmaps_to_string(const opencog::HandleMapSeq& hms)
{
	stringstream ss;
	ss << "size = " << hms.size() << std::endl;
	for (unsigned i = 0; i < hms.size(); i++)
		ss << "--- map[" << i << "] ---" << std::endl
		   << hmap_to_string(hms[i]);
	return ss.str();
}

} // ~namespace std
