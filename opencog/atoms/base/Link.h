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

#include <string>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{
class AtomTable;
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
    friend class AtomTable;

private:
    void init(const HandleSeq&);
    void resort(void);

protected:
    //! Array holding actual outgoing set of the link.
    //! Should not change during atom lifespan.
    HandleSeq _outgoing;

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
    Link(const HandleSeq& oset, Type t=LINK)
        : Atom(t)
    {
        init(oset);
    }

    Link(Type t, const Handle& h)
        : Atom(t)
    {
        // reserve+assign is 2x faster than push_back()/emplace_back()
        HandleSeq oset(1);
        oset[0] = h;
        init(oset);
    }

    Link(Type t, const Handle& ha, const Handle &hb)
        : Atom(t)
    {
        // reserve+assign is 2x faster than push_back()/emplace_back()
        HandleSeq oset(2);
        oset[0] = ha;
        oset[1] = hb;
        init(oset);
    }

    Link(Type t, const Handle& ha, const Handle &hb, const Handle &hc)
        : Atom(t)
    {
        // reserve+assign is 2x faster than push_back()/emplace_back()
        HandleSeq oset(3);
        oset[0] = ha;
        oset[1] = hb;
        oset[2] = hc;
        init(oset);
    }
    Link(Type t, const Handle& ha, const Handle &hb,
	      const Handle &hc, const Handle &hd)
        : Atom(t)
    {
        // reserve+assign is 2x faster than push_back()/emplace_back()
        HandleSeq oset(4);
        oset[0] = ha;
        oset[1] = hb;
        oset[2] = hc;
        oset[3] = hd;
        init(oset);
    }

    /**
     * Copy constructor, does NOT copy atomspace membership,
     * or any of the values or truth values.
     */
    Link(const Link &l)
        : Atom(l.getType())
    {
        init(l.getOutgoingSet());
    }

    /**
     * Destructor for this class.
     */
    ~Link();

    virtual bool isNode() const { return false; }
    virtual bool isLink() const { return true; }

    virtual Arity getArity() const {
        return _outgoing.size();
    }

    virtual size_t size() const {
        size_t size = 1;
        for (const Handle&h : _outgoing)
            size += h->size();
        return size;
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
        // Checks for a valid position
        if (pos < _outgoing.size()) {
            return Handle(AtomCast(_outgoing[pos]));
        } else {
            throw RuntimeException(TRACE_INFO, "invalid outgoing set index %d", pos);
        }
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
    std::string toString(const std::string& indent) const;

    /**
     * Returns a short string representation of the link.
     * Note that the TV is only represented by its
     * mean and count so if it is a compositeTV only
     * the primaryTV is printed.
     *
     * @return A short string representation of the link.
     */
    std::string toShortString(const std::string& indent) const;

	// Work around gdb's incapability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 and
	// http://stackoverflow.com/questions/2973976 for more
	// explanation.
	using Atom::toString;
	using Atom::toShortString;
	
    /**
     * Perform a content-based compare of another atom to this one.
     * Return true if the content is the same for both atoms.
     * @param Atom to be tested.
     * @return true if content is equal, false otherwise.
     */
    virtual bool operator==(const Atom&) const;

    /**
     * Provides an ordering operator, based on the atom hash.
     * performs a simple numeric comparison on the hashes of
     * this and the other atom. If the hashes are equal, then
     * it performs a content-based compare.
     *
     * @return true if this atom is less than the given one, false otherwise.
     */
    virtual bool operator<(const Atom&) const;
};

static inline LinkPtr LinkCast(const Handle& h)
    { return std::dynamic_pointer_cast<Link>(AtomCast(h)); }
static inline LinkPtr LinkCast(const AtomPtr& a)
    { return std::dynamic_pointer_cast<Link>(a); }

// XXX temporary hack ...
#define createLink std::make_shared<Link>

/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_H
