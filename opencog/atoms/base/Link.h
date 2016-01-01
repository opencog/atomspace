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

//! arity of Links, represented as short integer (16 bits)
typedef unsigned short Arity;

/**
 * Nodes in OpenCog are connected to each other by links. Each link embodies
 * one of the basic inter-node relationships. Links do not necessarily
 * describe a binary relationship between two entities. Links may describe
 * relationships between more than two entities at once. Finally, links
 * describe relationships not only between nodes, but also higher-order
 * relationships between links, and between nodes and links.
 */
class Link : public Atom
{
    friend class AtomTable;

private:
    void init(const HandleSeq&);
    void resort(void);

    Link(const Link &l) : Atom(0)
    { OC_ASSERT(false, "Link: bad use of copy ctor"); }

protected:

    //! Array holding actual outgoing set of the link.
    //! Should not change during atom lifespan.
    HandleSeq _outgoing;

public:
    /**
     * Constructor for this class.
     *
     * @param Link type.
     * @param Outgoing set, which is an array of the atom handles
     *        referenced by this link.
     * @param Link truthvalue, which will be cloned before being
     *        stored in this Link.
     */
    Link(Type t, const HandleSeq& oset,
         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
         AttentionValuePtr av = AttentionValue::DEFAULT_AV())
        : Atom(t, tv, av)
    {
        init(oset);
    }

    Link(Type t, const Handle& h,
         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
         AttentionValuePtr av = AttentionValue::DEFAULT_AV())
        : Atom(t, tv, av)
    {
        HandleSeq oset;
        oset.emplace_back(h);
        init(oset);
    }

    Link(Type t, const Handle& ha, const Handle &hb,
         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
         AttentionValuePtr av = AttentionValue::DEFAULT_AV())
        : Atom(t, tv, av)
    {
        HandleSeq oset;
        oset.emplace_back(ha);
        oset.emplace_back(hb);
        init(oset);
    }

    Link(Type t, const Handle& ha, const Handle &hb, const Handle &hc,
         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
         AttentionValuePtr av = AttentionValue::DEFAULT_AV())
        : Atom(t, tv, av)
    {
        HandleSeq oset;
        oset.emplace_back(ha);
        oset.emplace_back(hb);
        oset.emplace_back(hc);
        init(oset);
    }
    Link(Type t, const Handle& ha, const Handle &hb,
	      const Handle &hc, const Handle &hd,
         TruthValuePtr tv = TruthValue::DEFAULT_TV(),
         AttentionValuePtr av = AttentionValue::DEFAULT_AV())
        : Atom(t, tv, av)
    {
        HandleSeq oset;
        oset.emplace_back(ha);
        oset.emplace_back(hb);
        oset.emplace_back(hc);
        oset.emplace_back(hd);
        init(oset);
    }

    /**
     * Copy constructor, does NOT copy atom table membership!
     * Cannot be const, because the get() functions can't be,
     * because thread-safe locking required in the gets. */
    Link(Link &l)
        : Atom(l.getType(),
               ({ TruthValuePtr tv(l.getTruthValue());
                  tv->isDefinedTV() ? tv : tv->clone(); }),
               ({ AttentionValuePtr av(l.getAttentionValue());
                  av->isDefaultAV() ? av : av->clone(); }))
    {
        init(l.getOutgoingSet());
    }

    /**
     * Destructor for this class.
     */
    ~Link();

    inline Arity getArity() const {
        return _outgoing.size();
    }

    /**
     * Returns a const reference to the array containing this
     * atom's outgoing set.
     *
     * @return A const reference to this atom's outgoing set.
     */
    inline const HandleSeq& getOutgoingSet() const
    {
        return _outgoing;
    }
    /**
     * Returns a specific Handle in the outgoing set.
     *
     * @param The position of the handle in the array.
     * @return A specific handle in the outgoing set.
     */
    inline Handle getOutgoingAtom(Arity pos) const
    {
        // Checks for a valid position
        if (pos < getArity()) {
            return _outgoing[pos];
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
        for (const Handle& out_h : getOutgoingSet()) {
            if ((data->*cb)(out_h)) return true;
        }
        return false;
    }

    /**
     * Returns a string representation of the link.
     *
     * @return A string representation of the link.
     */
    std::string toString(const std::string& indent);

    /**
     * Returns a short string representation of the link.
     * Note that the TV is only represented by its
     * mean and count so if it is a compositeTV only
     * the primaryTV is printed.
     *
     * @return A short string representation of the link.
     */
    std::string toShortString(const std::string& indent);

	// Work around gdb's incapability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 and
	// http://stackoverflow.com/questions/2973976 for more
	// explanation.
	using Atom::toString;
	using Atom::toShortString;
	
    /**
     * Returns whether a given atom is equal to the current link.
     * @param Atom to be tested.
     * @return true if they are equal, false otherwise.
     */
    virtual bool operator==(const Atom&) const;

    /**
     * Returns whether a given atom is different from the current link.
     * @param Atom to be tested.
     * @return true if they are different, false otherwise.
     */
    virtual bool operator!=(const Atom&) const;
};

static inline LinkPtr LinkCast(const Handle& h)
    { AtomPtr a(h); return std::dynamic_pointer_cast<Link>(a); }
static inline LinkPtr LinkCast(const AtomPtr& a)
    { return std::dynamic_pointer_cast<Link>(a); }

// XXX temporary hack ...
#define createLink std::make_shared<Link>

/** @}*/
} // namespace opencog

#endif // _OPENCOG_LINK_H
