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

bool content_eq(const HandleSeq& lhs, const HandleSeq& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin(), rit = rhs.begin();
	for (; rit != rhs.end(); ++lit, ++rit)
		if (not content_eq(*lit, *rit))
			return false;
	return true;

}

bool content_eq(const HandleSet& lhs, const HandleSet& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin(), rit = rhs.begin();
	for (; rit != rhs.end(); ++lit, ++rit)
		if (not content_eq(*lit, *rit))
			return false;
	return true;
}

bool content_eq(const opencog::HandleSetSeq& lhs,
                const opencog::HandleSetSeq& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin(), rit = rhs.begin();
	for (; rit != rhs.end(); ++lit, ++rit)
		if (not content_eq(*lit, *rit))
			return false;
	return true;
}

bool content_contains(const opencog::HandleSeq& hs, const opencog::Handle& h)
{
	for (const Handle& o : hs)
		if (content_eq(h, o))
			return true;
	return false;
}

// The rest of this file is devoted to printing utilities used only
// during GDB debugging. You can configure GDB as follows
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects

std::string oc_to_string(const Handle& h, const std::string& indent)
{
	if (h == nullptr)
		return indent + "nullatom";
	else
		return h->to_string(indent);
}

std::string oc_to_string(const HandlePair& hp, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "first:" << std::endl
	   << oc_to_string(hp.first, indent + OC_TO_STRING_INDENT) << std::endl;
	ss << indent << "second:" << std::endl
	   << oc_to_string(hp.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

// Hack around the lack template use in Handle.h due to circular dependencies
#define GEN_ATOM_CONTAINER_OC_TO_STRING(T) \
std::string oc_to_string(const T& hs, const std::string& indent) { \
	std::stringstream ss; \
	ss << indent << "size = " << hs.size(); \
	size_t i = 0; \
	for (const opencog::Handle& h : hs) { \
		ss << std::endl << indent << "atom[" << i << "]:" << std::endl \
		   << oc_to_string(h, indent + OC_TO_STRING_INDENT); \
		i++; \
	} \
	return ss.str(); \
}
GEN_ATOM_CONTAINER_OC_TO_STRING(opencog::HandleSeq)

std::string oc_to_string(const HandleSeqSeq& hss, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hss.size();
	size_t i = 0;
	for (const HandleSeq& hs : hss) {
		ss << std::endl << indent << "atoms[" << i << "]:" << std::endl
		   << oc_to_string(hs, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

GEN_ATOM_CONTAINER_OC_TO_STRING(opencog::HandleSet)

std::string oc_to_string(const HandleSetSet& ohss, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << ohss.size();
	size_t i = 0;
	for (const HandleSet& ohs : ohss) {
		ss << std::endl << indent << "atoms[" << i << "]:" << std::endl
		   << oc_to_string(ohs, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleSetSeq& hss, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hss.size();
	size_t i = 0;
	for (const HandleSet& hs : hss) {
		ss << std::endl << indent << "atoms[" << i << "]:" << std::endl
		   << oc_to_string(hs, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

GEN_ATOM_CONTAINER_OC_TO_STRING(opencog::UnorderedHandleSet)

std::string oc_to_string(const HandleMap& hmap, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hmap.size();
	int i = 0;
	for (const auto& p : hmap) {
		ss << std::endl << indent << "key[" << i << "]:" << std::endl
		   << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "value[" << i << "]:" << std::endl
		   << oc_to_string(p.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const UnorderedHandleMap& hmap, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hmap.size();
	int i = 0;
	for (const auto& p : hmap) {
		ss << std::endl << indent << "key[" << i << "]:" << std::endl
		   << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "value[" << i << "]:" << std::endl
		   << oc_to_string(p.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleMap::value_type& hmv, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "key:" << std::endl
	   << oc_to_string(hmv.first, indent + OC_TO_STRING_INDENT)
	   << indent << "value:" << std::endl
	   << oc_to_string(hmv.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const HandleMultimap& hmultimap, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hmultimap.size();
	int i = 0;
	for (const auto& p : hmultimap) {
		ss << std::endl << indent << "key[" << i << "]:" << std::endl
		   << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "values[" << i << "]:" << std::endl
		   << oc_to_string(p.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleSeqMap& hsm, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hsm.size();
	int i = 0;
	for (const auto& p : hsm) {
		ss << std::endl << indent << "key[" << i << "]:" << std::endl
		   << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "values[" << i << "]:" << std::endl
		   << oc_to_string(p.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleMapSeq& hms, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hms.size();
	for (unsigned i = 0; i < hms.size(); i++)
		ss << std::endl << indent << "--- map[" << i << "] ---" << std::endl
		   << oc_to_string(hms[i], indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const HandleMapSeqSeq& hmss, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hmss.size();
	for (unsigned i = 0; i < hmss.size(); i++)
		ss << std::endl << indent << "--- maps[" << i << "] ---" << std::endl
		   << oc_to_string(hmss[i], indent + OC_TO_STRING_INDENT);
	return ss.str();	
}

std::string oc_to_string(const HandleMapSet& hms, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hms.size();
	unsigned i = 0;
	for (const HandleMap& hm : hms) {
		ss << std::endl << indent << "--- map[" << i << "] ---" << std::endl
		   << oc_to_string(hm, indent + OC_TO_STRING_INDENT);
		++i;
	}
	return ss.str();
}

std::string oc_to_string(const HandlePairSeq& hps, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hps.size();
	size_t i = 0;
	for (const HandlePair& hp : hps) {
		ss << std::endl << indent << "atom.first[" << i << "]:" << std::endl
		   << oc_to_string(hp.first, indent + OC_TO_STRING_INDENT);
		ss << indent << "atom.second[" << i << "]:" << std::endl
		   << oc_to_string(hp.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleCounter& hc, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hc.size();
	size_t i = 0;
	for (const auto& el : hc) {
		ss << std::endl << indent << "atom[" << i << "]:" << std::endl
		   << oc_to_string(el.first, indent + OC_TO_STRING_INDENT)
		   << indent << "num[" << i << "]:" << el.second << std::endl;
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const HandleUCounter& huc, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << huc.size();
	size_t i = 0;
	for (const auto& el : huc) {
		ss << std::endl << indent << "atom[" << i << "]:" << std::endl
		   << oc_to_string(el.first, indent + OC_TO_STRING_INDENT)
		   << indent << "num[" << i << "]:" << el.second << std::endl;
		i++;
	}
	return ss.str();
}

std::string oc_to_string(Type type, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << nameserver().getTypeName(type);
	return ss.str();
}

std::string oc_to_string(const TypeSet& types, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << types.size();
	if (not types.empty()) {
		ss << ", types =";
		for (Type t : types) {
			ss << " " << oc_to_string(t);
		}
	}
	return ss.str();
}

std::string oc_to_string(const AtomPtr& aptr, const std::string& indent)
{
	return oc_to_string(aptr->get_handle(), indent);
}

std::string oc_to_string(const LinkPtr& lptr, const std::string& indent)
{
	return oc_to_string(lptr->get_handle(), indent);
}

} // ~namespace opencog

namespace std {

ostream& operator<<(ostream& out, const opencog::HandleMap& hm)
{
	return out << oc_to_string(hm);
}
ostream& operator<<(ostream& out, const opencog::HandleSeq& hs)
{
	return out << oc_to_string(hs);
}
ostream& operator<<(ostream& out, const opencog::HandleSet& ohs)
{
	return out << oc_to_string(ohs);
}
ostream& operator<<(ostream& out, const opencog::UnorderedHandleMap& hm)
{
	return out << oc_to_string(hm);
}
ostream& operator<<(ostream& out, const opencog::UnorderedHandleSet& uhs)
{
	return out << oc_to_string(uhs);
}

} // ~namespace std
