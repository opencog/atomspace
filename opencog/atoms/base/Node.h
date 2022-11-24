/*
 * opencog/atoms/base/Node.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
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

#ifndef _OPENCOG_NODE_H
#define _OPENCOG_NODE_H

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ClassServer.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This is a subclass of Atom. It represents the most basic kind of
 * pattern known to the OpenCog system.
 */
class Node : public Atom
{
protected:
    // properties
    std::string _name;
    void init();

    virtual ContentHash compute_hash() const;

public:
    /**
     * Constructor for this class.
     *
     * @param Node type
     * @param Node name A reference to a std::string with the name of
     *                  the node.  Use empty string for unnamed node.
     */
    Node(Type t, const std::string s)
        : Atom(t), _name(std::move(s))
    {
        init();
    }

    Node(const Node&) = delete;
    Node& operator=(const Node&) = delete;

    virtual bool is_node() const { return true; }
    virtual bool is_link() const { return false; }

    /**
     * Gets the name of the node.
     *
     * @return The name of the node.
     */
    virtual const std::string& get_name() const { return _name; }

    virtual size_t size() const { return 1; }
    virtual ValuePtr value_at_index(size_t idx) const {
        return ValueCast(get_handle());
    }

    /**
     * Returns a string representation of the node.
     *
     * @return A string representation of the node.
     */
    std::string to_string(const std::string& indent) const;
    std::string to_short_string(const std::string& indent) const;
    std::string to_string_esc(void) const;

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

#define NODE_PTR_DECL(CNAME) ATOM_PTR_DECL(CNAME)
NODE_PTR_DECL(Node)

template< class... Args >
Handle createNode( Args&&... args )
{
   Handle tmp(std::make_shared<Node>(std::forward<Args>(args) ...));
   return classserver().factory(tmp);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_NODE_H
