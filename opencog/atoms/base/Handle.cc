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
#include <opencog/atoms/base/Link.h>

namespace opencog {

const Handle Handle::UNDEFINED;
const AtomPtr Handle::NULL_POINTER;

ContentHash Handle::value(void) const
{
    if (get()) return get()->get_hash();
    return INVALID_HASH;
}

// ===================================================
// Atom comparison.

bool content_eq(const Handle& lh, const Handle& rh) noexcept
{
    if (lh == rh) return true;
    if (nullptr == lh or nullptr == rh) return false;
    if (lh->get_hash() != rh->get_hash()) return false;

    return *((AtomPtr) lh) == *((AtomPtr) rh);
}

bool Handle::atoms_less(const Atom* a, const Atom* b)
{
    if (a == b) return false;
    if (nullptr == a) return true;
    if (nullptr == b) return false;

    // Pointer compare
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

std::size_t hash_value(Handle const& h)
{
	if (nullptr == h) return 0;
	return h->get_hash();
}

int Handle::compare(const Handle& h1, const Handle& h2)
{
	if (h1.get() == h2.get()) return 0;
	if (h1.get() == nullptr) return -1;
	if (h2.get() == nullptr) return +1;
	if (h1->operator<(*h2)) return -1;
	if (h2->operator<(*h1)) return +1;
	return 0;
}

bool Handle::operator<(const Handle& h) const noexcept
{
	if (get() == h.get()) return false;
	if (get() == nullptr) return true;
	if (h.get() == nullptr) return false;
	return get()->operator<(*h);
}

// The rest of this file is devoted to printing utilities used only
// during GDB debugging.  Thus, you won't find these anywhere in the
// code base. You may call that directly from gdb
// (opencog::h_to_string, etc), but very likely your version of GDB
// supports overloading and in this case you can simply configure GDB
// as follows
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string h_to_string(const Handle& h)
{
	if (h == nullptr)
		return "nullatom\n";
	else
		return h->to_string();
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
std::string hss_to_string(const HandleSeqSeq& hss)
{
	std::stringstream ss;
	ss << "size = " << hss.size() << std::endl;
	size_t i = 0;
	for (const HandleSeq& hs : hss) {
		ss << "atoms[" << i << "]:" << std::endl << hs_to_string(hs);
		i++;
	}
	return ss.str();
}
std::string ohs_to_string(const HandleSet& ohs)
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
std::string hmapset_to_string(const HandleMapSet& hms)
{
	std::stringstream ss;
	ss << "size = " << hms.size() << std::endl;
	unsigned i = 0;
	for (const HandleMap& hm : hms) {
		ss << "--- map[" << i << "] ---" << std::endl
		   << hmap_to_string(hm);
		++i;
	}
	return ss.str();
}
std::string hps_to_string(const HandlePairSeq& hps)
{
	std::stringstream ss;
	ss << "size = " << hps.size() << std::endl;
	size_t i = 0;
	for (const HandlePair& hp : hps) {
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
	return h_to_string(aptr->get_handle());
}
std::string lptr_to_string(const LinkPtr& lptr)
{
	return h_to_string(lptr->get_handle());
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
std::string oc_to_string(const HandleSeqSeq& hss)
{
	return hss_to_string(hss);
}
std::string oc_to_string(const HandleSet& ohs)
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
std::string oc_to_string(const HandleMapSet& hms)
{
	return hmapset_to_string(hms);
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
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::HandleSet)
GEN_HANDLE_CONTAINER_OSTREAM_OPERATOR(opencog::UnorderedHandleSet)

} // ~namespace std
