/*
 * opencog/atomspace/ProtoAtom.h
 *
 * Copyright (C) 2015 Linas Vepstas
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
 */

#ifndef _OPENCOG_PROTO_ATOM_H
#define _OPENCOG_PROTO_ATOM_H

#include <memory>
#include <string>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * ProtoAtoms are the base class for the Atom shared pointer.
 */
class ProtoAtom
	: public std::enable_shared_from_this<ProtoAtom>
{
protected:
	// We store the type locally, to avoid the overhead of
	// turning getType into a virtual method.
	Type _type;

public:
	ProtoAtom(Type t) : _type(t) {}

	virtual ~ProtoAtom() {}

	inline Type get_type() const { return _type; }

	/** Basic predicate */
	bool is_type(Type t, bool subclass) const
	{
		Type at(get_type());
		if (not subclass) return t == at;
		return classserver().isA(at, t);
	}

	virtual bool is_atom() const { return false; }
	virtual bool is_node() const { return false; }
	virtual bool is_link() const { return false; }

	/**
	 * Returns a string representation of the proto-atom.
	 */
	virtual std::string to_string(const std::string& indent) const = 0;
	virtual std::string to_short_string(const std::string& indent) const
		{ return to_string(indent); }

	// Work around gdb's inability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 for more
	// explanation.
	std::string to_string() const { return to_string(""); }
	std::string to_short_string() const { return to_short_string(""); }

	/**
	 * Returns whether two proto-atoms are equal.
	 *
	 * @return true if the proto-atoms are equal, false otherwise.
	 */
	virtual bool operator==(const ProtoAtom&) const = 0;

	/**
	 * Returns whether two proto-atoms are different.
	 *
	 * @return true if the proto-atoms are different, false otherwise.
	 */
	bool operator!=(const ProtoAtom& other) const
	{ return not operator==(other); }
};

typedef std::shared_ptr<ProtoAtom> ProtoAtomPtr;
#if NOT_RIGHT_NOW
struct ProtoAtomPtr : public std::shared_ptr<ProtoAtom>
{
	ProtoAtomPtr(std::shared_ptr<ProtoAtom> pa) :
		std::shared_ptr<ProtoAtom>(pa) {}
	ProtoAtomPtr(AtomPtr a) :
		std::shared_ptr<ProtoAtom>(
			std::dynamic_pointer_cast<ProtoAtom>(a)) {}
	ProtoAtomPtr(Handle h) :
		std::shared_ptr<ProtoAtom>(
			std::dynamic_pointer_cast<ProtoAtom>(AtomPtr(h))) {}
	operator AtomPtr() const
		{ return AtomPtr(std::dynamic_pointer_cast<Atom>(*this)); }
	operator Handle() const
		{ return Handle(AtomPtr(*this)); }
};
#endif

typedef std::vector<ProtoAtomPtr> ProtomSeq;

/** @}*/
} // namespace opencog

// overload of operator<< to print ProtoAtoms
namespace std
{
    template<typename Out>
    Out& operator<<(Out& out, const opencog::ProtoAtomPtr& pa)
    {
        out << pa->to_string("");
        return out;
    }
} // ~namespace std

#endif // _OPENCOG_PROTO_ATOM_H
