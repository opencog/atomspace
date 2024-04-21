/*
 * opencog/atoms/base/Link.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2008-2010 OpenCog Foundation
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

#ifndef _OPENCOG_LINK_H
#define _OPENCOG_LINK_H

#include <functional>
#include <string>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Atoms in OpenCog are connected to each other by links. Each link
 * embodies a basic inter-atom relationship. Links do not necessarily
 * describe a binary relationship between two atoms. Links may describe
 * relationships between more than two atoms at once. Finally, links
 * describe relationships not only between nodes, but also higher-order
 * relationships between links, and between nodes and links.
 */
class Link : public Atom
{
private:
    void init();

protected:
    //! Array holding actual outgoing set of the link.
    //! Should not change during atom lifespan.
    HandleSeq _outgoing;

    virtual void install();
    virtual void remove();
    virtual ContentHash compute_hash() const;

public:
    /**
     * Constructor for this class.
     *
     * @param Link type.
     * @param Outgoing set, which is an array of the atom handles
     *        referenced by this link.
     * @param Link truthvalue.
     */
    Link(const HandleSeq&& oset, Type t=LINK)
        : Atom(t), _outgoing(std::move(oset))
    {
        init();
    }

    Link(Type t)
        : Atom(t)
    {
        init();
    }

	Link(Type t, const Handle& h)
        : Atom(t), _outgoing({h})
    {
        init();
    }

    Link(Type t, const Handle& ha, const Handle &hb)
        : Atom(t), _outgoing({ha, hb})
    {
        init();
    }

    Link(Type t, const Handle& ha, const Handle &hb, const Handle &hc)
        : Atom(t), _outgoing({ha, hb, hc})
    {
        init();
    }
    Link(Type t, const Handle& ha, const Handle &hb,
	      const Handle &hc, const Handle &hd)
        : Atom(t), _outgoing({ha, hb, hc, hd})
    {
        init();
    }

    Link(const Link&) = delete;
    Link& operator=(const Link&) = delete;

    /**
     * Destructor for this class.
     */
    ~Link();

    virtual bool is_node() const { return false; }
    virtual bool is_link() const { return true; }

    virtual Arity get_arity() const {
        return _outgoing.size();
    }

    virtual size_t size() const {
        return _outgoing.size();
    }

    /**
     * Returns a const reference to the array containing this
     * atom's outgoing set.
     *
     * @return A const reference to this atom's outgoing set.
     */
    virtual const HandleSeq& getOutgoingSet() const
    {
        return _outgoing;
    }

    /**
     * Returns a specific Handle in the outgoing set.
     *
     * @param The position of the handle in the array.
     * @return A specific handle in the outgoing set.
     */
    virtual Handle getOutgoingAtom(Arity pos) const
    {
        return _outgoing.at(pos);
    }

    virtual ValuePtr value_at_index(size_t idx) const
    {
        return ValueCast(getOutgoingAtom(idx));
    }

    //! Invoke the callback on each atom in the outgoing set of
    //! handle h, until till one of them returns true, in which case,
    //! the loop stops and returns true. Otherwise the callback is
    //! called on all outgoings and false is returned.
    template<class T>
    inline bool foreach_outgoing(bool (T::*cb)(const Handle&), T *data)
    {
        for (const Handle& out_h : _outgoing) {
            if ((data->*cb)(out_h)) return true;
        }
        return false;
    }

    /**
     * Returns a string representation of the link.
     *
     * @return A string representation of the link.
     */
    std::string to_string(const std::string& indent) const;

    /**
     * Returns a short string representation of the link.
     * Note that the TV is only represented by its
     * mean and count so if it is a compositeTV only
     * the primaryTV is printed.
     *
     * @return A short string representation of the link.
     */
    std::string to_short_string(const std::string& indent) const;

	// Work around gdb's incapability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 and
	// http://stackoverflow.com/questions/2973976 for more
	// explanation.
	using Atom::to_string;
	using Atom::to_short_string;
	
    /**
     * Perform a content-based compare of another atom to this one.
     * Return true if the content is the same for both atoms.
     * @param Atom to be tested.
     * @return true if content is equal, false otherwise.
     */
    virtual bool operator==(const Atom&) const;

    /**
     * Provides an ordering operator, based on the atom hash.
     * Performs a simple numeric comparison on the hashes of
     * this and the other atom. If the hashes are equal, then
     * it performs a content-based compare.
     *
     * @return true if this atom is less than the given one, false otherwise.
     */
    virtual bool operator<(const Atom&) const;
};

#define LINK_PTR_DECL(CNAME)  ATOM_PTR_DECL(CNAME)

LINK_PTR_DECL(Link);

template< class... Args >
Handle createLink( Args&&... args )
{
	Handle tmp(std::make_shared<Link>(std::forward<Args>(args) ...));
	return classserver().factory(tmp);
}

/** @}*/
} // namespace opencog

/// Overload std::less to perform a content-based compare of the
/// LinkPtr's. Otherwise, it seems to just use the address returned
/// by LinkPtr::get().
namespace std {
template<>
struct less<opencog::LinkPtr>
{
    bool operator()(const opencog::LinkPtr& la, const opencog::LinkPtr& lb) const
    {
        return la->operator<(*lb);
    }
};
}

#endif // _OPENCOG_LINK_H
