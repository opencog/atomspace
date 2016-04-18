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
    // The is link/node is a low-cost way of checking if the
    // pointer really is an atom pointer, and not just protoAtom,
    // which does not have a uuid.
    if (a and (a->isLink() or a->isNode())) return a->getUUID();
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

std::string h_to_string(const Handle& h)
{
	if ((AtomPtr)h == nullptr)
		return "nullatom\n";
	else
		return h->toString();
}
std::string hp_to_string(const HandlePair& hp)
{
	std::stringstream ss;
	ss << "first:" << std::endl << h_to_string(hp.first);
	ss << "second:" << std::endl << h_to_string(hp.second);
	return ss.str();
}
std::string hs_to_string(const HandleSeq& hs)
{
	std::stringstream ss; std::operator<<(ss, hs); return ss.str();
}
std::string ohs_to_string(const OrderedHandleSet& ohs)
{
	std::stringstream ss; std::operator<<(ss, ohs); return ss.str();
}
std::string uhs_to_string(const UnorderedHandleSet& uhs)
{
	std::stringstream ss; std::operator<<(ss, uhs); return ss.str();
}
std::string hmap_to_string(const HandleMap& hmap)
{
	std::stringstream ss;
	ss << "size = " << hmap.size() << std::endl;
	int i = 0;
	for (const auto& p : hmap) {
		ss << "key[" << i << "]:" << std::endl << h_to_string(p.first)
		   << "value[" << i << "]:" << std::endl << h_to_string(p.second);
		i++;
	}
	return ss.str();
}
std::string hmultimap_to_string(const HandleMultimap& hmultimap)
{
	std::stringstream ss;
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
std::string hmaps_to_string(const HandleMapSeq& hms)
{
	std::stringstream ss;
	ss << "size = " << hms.size() << std::endl;
	for (unsigned i = 0; i < hms.size(); i++)
		ss << "--- map[" << i << "] ---" << std::endl
		   << hmap_to_string(hms[i]);
	return ss.str();
}
std::string hps_to_string(const HandlePairSeq& hps)
{
	std::stringstream ss;
	ss << "size = " << hps.size() << std::endl;
	size_t i = 0;
	for (const std::pair<Handle, Handle>& hp : hps) {
		ss << "atom.first[" << i << "]:" << std::endl << h_to_string(hp.first);
		ss << "atom.second[" << i << "]:" << std::endl << h_to_string(hp.second);
		i++;
	}
	return ss.str();
}
std::string atomtype_to_string(Type type)
{
	std::stringstream ss;
	ss << classserver().getTypeName(type) << std::endl;
	return ss.str();
}
std::string aptr_to_string(const AtomPtr& aptr)
{
	return h_to_string(aptr->getHandle());
}
std::string lptr_to_string(const LinkPtr& lptr)
{
	return h_to_string(lptr->getHandle());
}

std::string oc_to_string(const Handle& h)
{
	return h_to_string(h);
}
std::string oc_to_string(const HandlePair& hp)
{
	return hp_to_string(hp);
}
std::string oc_to_string(const HandleSeq& hs)
{
	return hs_to_string(hs);
}
std::string oc_to_string(const OrderedHandleSet& ohs)
{
	return ohs_to_string(ohs);
}
std::string oc_to_string(const UnorderedHandleSet& uhs)
{
	return uhs_to_string(uhs);
}
std::string oc_to_string(const HandleMap& hm)
{
	return hmap_to_string(hm);
}
std::string oc_to_string(const HandleMultimap& hmm)
{
	return hmultimap_to_string(hmm);
}
std::string oc_to_string(const HandleMapSeq& hms)
{
	return hmaps_to_string(hms);
}
std::string oc_to_string(const HandlePairSeq& hps)
{
	return hps_to_string(hps);
}
std::string oc_to_string(Type type)
{
	return atomtype_to_string(type);
}
std::string oc_to_string(const AtomPtr& aptr)
{
	return aptr_to_string(aptr);
}
std::string oc_to_string(const LinkPtr& lptr)
{
	return lptr_to_string(lptr);
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

} // ~namespace std
