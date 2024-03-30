/*
 * opencog/atomspace/AtomSpace.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009, 2013, 2021 Linas Vepstas
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

#include <string>
#include <iostream>
#include <fstream>
#include <list>

#include <stdlib.h>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/ValueFactory.h>

#include "AtomSpace.h"

using namespace opencog;

// ====================================================================

// An extremely primitive permissions system.
void AtomSpace::set_read_only(void)
{
    _read_only = true;
}

void AtomSpace::set_read_write(void)
{
    _read_only = false;
}

bool AtomSpace::content_compare(const AtomSpace& space_first,
                                const AtomSpace& space_second,
                                bool check_values,
                                bool emit_diagnostics)
{
    // Compare sizes
    if (space_first.get_size() != space_second.get_size())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - size " <<
                    space_first.get_size() << " != size " <<
                    space_second.get_size() << std::endl;
        return false;
    }

    // Compare node count
    if (space_first.get_num_nodes() != space_second.get_num_nodes())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - node count " <<
                    space_first.get_num_nodes() << " != node count " <<
                    space_second.get_num_nodes() << std::endl;
        return false;
    }

    // Compare link count
    if (space_first.get_num_links() != space_second.get_num_links())
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - link count " <<
                    space_first.get_num_links() << " != link count " <<
                    space_second.get_num_links() << std::endl;
        return false;
    }

    // If we get this far, we need to compare each individual atom.

    // Get the atoms in each atomspace.
    HandleSeq atomsInFirstSpace, atomsInSecondSpace;
    space_first.get_handles_by_type(atomsInFirstSpace, ATOM, true);
    space_second.get_handles_by_type(atomsInSecondSpace, ATOM, true);

    // Uncheck each atom in the second atomspace.
    for (auto atom : atomsInSecondSpace)
        atom->setUnchecked();

    // Loop to see if each atom in the first has a match in the second.
    for (auto atom_first : atomsInFirstSpace)
    {
        Handle atom_second = space_second.get_atom(atom_first);

        if( false)
        {
        Handle atom_second;
        if (atom_first->is_node())
        {
            atom_second = space_second.get_node(atom_first->get_type(),
                        std::string(atom_first->get_name()));
        }
        else if (atom_first->is_link())
        {
            atom_second =  space_second.get_link(atom_first->get_type(),
                        HandleSeq(atom_first->getOutgoingSet()));
        }
        else
        {
             throw opencog::RuntimeException(TRACE_INFO,
                 "AtomSpace::compare_atomspaces - atom not Node or Link");
        }
        }

        // If the atoms don't match because one of them is null.
        if ((atom_first and not atom_second) or
            (atom_second and not atom_first))
        {
            if (emit_diagnostics)
            {
                if (atom_first)
                    std::cout << "compare_atomspaces - first atom " <<
                            atom_first->to_string() << " != NULL " <<
                            std::endl;
                if (atom_second)
                    std::cout << "compare_atomspaces - first atom " <<
                            "NULL != second atom " <<
                            atom_second->to_string() << std::endl;
            }
            return false;
        }

        // If the atoms don't match... Compare the atoms not the pointers
        // which is the default if we just use Handle operator ==.
        if (*((AtomPtr) atom_first) != *((AtomPtr) atom_second))
        {
            if (emit_diagnostics)
                std::cout << "compare_atomspaces - first atom " <<
                        atom_first->to_string() << " != second atom " <<
                        atom_second->to_string() << std::endl;
            return false;
        }

        // Check the values...
        // TODO: this should probably be moved to a method on class Atom.
        if (check_values)
        {
            HandleSet keys_first = atom_first->getKeys();
            HandleSet keys_second = atom_second->getKeys();
            if (keys_first.size() != keys_second.size())
            {
                if (emit_diagnostics)
                    std::cout << "compare_atomspaces - first keys size "
                              << keys_first.size() << " != second keys size "
                              << keys_second.size() << " for "
                              << atom_first->to_short_string() << std::endl;
                return false;
            }

            if (keys_first != keys_second)
            {
                if (emit_diagnostics)
                    std::cout << "compare_atomspaces - key set mistmatch for "
                              << atom_first->to_short_string() << std::endl;
                return false;
            }

            for (const Handle& key: keys_first)
            {
                ValuePtr value_first = atom_first->getValue(key);
                ValuePtr value_second = atom_second->getValue(key);
                if (*value_first != *value_second)
                {
                    if (emit_diagnostics)
                        std::cout << "compare_atomspaces - first value "
                            << value_first->to_string() << " != second value "
                            << value_second->to_string() << " for "
                            << atom_first->to_short_string() << std::endl;
                    return false;
                }
            }
        }

        // Set the check for the second atom.
        atom_second->setChecked();
    }

    // Make sure each atom in the second atomspace has been checked.
    bool all_checked = true;
    for (auto atom : atomsInSecondSpace)
    {
        if (!atom->isChecked())
        {
            if (emit_diagnostics)
                std::cout << "compare_atomspaces - unchecked space atom " <<
                        atom->to_string() << std::endl;
            all_checked = false;
        }
    }
    if (!all_checked)
        return false;

    // If we get this far, then the spaces are equal.
    return true;
}

// Name-compare only. No content-compare.
bool AtomSpace::operator==(const Atom& other) const
{
    // If other points to this, then have equality.
    if (this == &other) return true;

    if (ATOM_SPACE != other.get_type()) return false;

    // Compare the AtomSpace names, only. This will make them behave
    // like Nodes, when being inserted or removed. We can't do a
    // content compare, because it's too slow for insertion/removal.
    return get_name() == other.get_name();
}

bool AtomSpace::operator<(const Atom& other) const
{
    // If other points to this, then have equality.
    if (this == &other) return false;

    if (ATOM_SPACE != other.get_type()) return false;
    AtomSpace* asp = (AtomSpace*) &other;
    return _uuid  < (asp->_uuid);
}

ContentHash AtomSpace::compute_hash() const
{
	if (_name.empty()) return _uuid;
	ContentHash hsh = std::hash<std::string>()(get_name());

	// Nodes will never have the MSB set.
	ContentHash mask = ~(((ContentHash) 1ULL) << (8*sizeof(ContentHash) - 1));
	hsh &= mask;
	return hsh;
}

// ====================================================================
// Provide all of the virtual methods on the base class.

const std::string& AtomSpace::get_name() const
{
	return _name;
}

// In order to restore complex AtomSpace DAG's from storage, we need
// to be able to set their names, to match what is in storage. So the
// name is settable.
void AtomSpace::set_name(const std::string& newna)
{
	_name = newna;
	_content_hash = compute_hash();
}

Handle AtomSpace::getOutgoingAtom(Arity n) const
{
	if (_outgoing.size() <= n) return Handle::UNDEFINED;
	return _outgoing[n];
}

ValuePtr AtomSpace::value_at_index(size_t idx) const
{
	return ValueCast(getOutgoingAtom(idx));
}

void AtomSpace::setAtomSpace(AtomSpace* as)
{
	// No-op. AtomSpaces cannot be "owned" by other AtomSpaces.
	// Why? Well, right now, allowing this seems like an awkward
	// thing to do. It's not clear how to think about this correctly.
	// So we'll just pre-emptively disallow it.
}

// ====================================================================
// XXX FIXME -- The recursive design of the depth() routine below makes
// it into a bottleneck, when the stack of AtomSpaces exceeds a few
// hundred. In particular, the recursion is on the C stack, and I don't
// believe the compiler has optimized them to be tail-recursive. (If
// they are tail-recursive, I guess that's OK, eh?)
// At this time, the only user of this code appears to be UniqueLink.cc
// It is NOT used by Rocks.

int AtomSpace::depth(const AtomSpace* as) const
{
    if (nullptr == as) return -1;
    if (as == this) return 0;

    for (const AtomSpacePtr& base : _environ)
    {
        int d = base->depth(as);
        if (0 < d) return d+1;
    }
    return -1;
}

int AtomSpace::depth(const Handle& atom) const
{
    if (nullptr == atom) return -1;
    AtomSpace* as = atom->getAtomSpace();
    if (as == this) return 0;

    for (const AtomSpacePtr& base : _environ)
    {
        int d = base->depth(as);
        if (0 < d) return d+1;
    }
    return -1;
}

bool AtomSpace::in_environ(const AtomSpace* as) const
{
    if (nullptr == as) return false;
    if (as == this) return true;
    const std::vector<AtomSpacePtr>* env = &_environ;
    while (true)
    {
        size_t evs = env->size();
        if (0 == evs) return false;
        if (1 < evs) break;
        if (as == (*env)[0].get()) return true;
        env = &((*env)[0]->_environ);
    }
    for (const AtomSpacePtr& base : *env)
    {
        if (base->in_environ(as)) return true;
    }
    return false;
}

bool AtomSpace::in_environ(const Handle& atom) const
{
    if (nullptr == atom) return false;
    AtomSpace* as = atom->getAtomSpace();
    if (as == this) return true;
    for (const AtomSpacePtr& base : _environ)
    {
        if (base->in_environ(as)) return true;
    }
    return false;
}

// ====================================================================

Handle AtomSpace::add_atom(const Handle& h)
{
    // Cannot add atoms to a read-only atomspace. But if it's already
    // in the atomspace, return it.
    if (_read_only) return get_atom(h);

    // If it is a DeleteLink, then the addition will fail. Deal with it.
    // If its a GrantLink, addition might require extra care.
    Handle rh;
    try {
        rh = add(h);
    }
    catch (const DeleteException& ex) {
        // Hmmm. Need to notify the backing store
        // about the deleted atom. But how?
    }
    catch (const SilentException& ex) {
        // The SilentException is thrown by GrantLink, when the
        // user attempts grants in non-base Frames. We want to
        // disallow hiding of grants, so we end up here.
        return lookupHide(h, false);  // Do not allow hiding!
    }
    return rh;
}

Handle AtomSpace::add_node(Type t, std::string&& name)
{
    // Cannot add atoms to a read-only atomspace. But if it's already
    // in the atomspace, return it.
    if (_read_only)
        return lookupHandle(createNode(t, std::move(name)));

    return add(createNode(t, std::move(name)));
}

Handle AtomSpace::get_node(Type t, std::string&& name) const
{
    return lookupHandle(createNode(t, std::move(name)));
}

Handle AtomSpace::add_link(Type t, HandleSeq&& outgoing)
{
    // Cannot add atoms to a read-only atomspace. But if it's already
    // in the atomspace, return it.
    if (_read_only)
        return lookupHandle(createLink(std::move(outgoing), t));

    // If it is a DeleteLink, then the addition will fail. Deal with it.
    // If its a GrantLink, addition might require extra care.
    Handle h(createLink(std::move(outgoing), t));
    try {
        return add(h);
    }
    catch (const DeleteException& ex) {
        // Hmmm. Need to notify the backing store
        // about the deleted atom. But how?
    }
    catch (const SilentException& ex) {
        // The SilentException is thrown by GrantLink, when the
        // user attempts grants in non-base Frames. We want to
        // disallow hiding of grants, so we end up here.
        return lookupHide(h, false);  // Do not allow hiding!
    }
    return Handle::UNDEFINED;
}

Handle AtomSpace::get_link(Type t, HandleSeq&& outgoing) const
{
    return lookupHandle(createLink(std::move(outgoing), t));
}

ValuePtr AtomSpace::add_atoms(const ValuePtr& vptr)
{
    if (nullptr == vptr) return vptr;

    Type t = vptr->get_type();
    if (nameserver().isA(t, ATOM))
    {
        Handle h = add_atom(HandleCast(vptr));
        if (h) return h; // Might be null if AtomSpace is read-only.
        return vptr;
    }

    if (nameserver().isA(t, LINK_VALUE))
    {
        std::vector<ValuePtr> vvec;
        for (const ValuePtr& v : LinkValueCast(vptr)->value())
           vvec.push_back(add_atoms(v));

        return valueserver().create(t, vvec);
    }
    return vptr;
}

// COW == Copy On Write
#define COWBOY_CODE(DO_STUFF)                                            \
    AtomSpace* has = h->getAtomSpace();                                  \
                                                                         \
    /* Hmm. It's kind-of a user-error, if they give us a naked atom.  */ \
    /* We could throw here, and force them to fix their code, or we   */ \
    /* can silently do what they wanted!? Which will probably expose  */ \
    /* other hard-to-debug bugs in the user's code ...                */ \
    /* if (nullptr == has)                                            */ \
    /*     throw opencog::RuntimeException(TRACE_INFO,                */ \
    /*            "Your atom is needs to be placed in an atomspace!") */ \
                                                                         \
    if (_read_only) {                                                    \
        throw opencog::RuntimeException(TRACE_INFO,                      \
             "Value not changed; AtomSpace is readonly");                \
        return Handle::UNDEFINED;                                        \
    }                                                                    \
                                                                         \
    /* No copy needed. Safe to just update.                           */ \
    if (has == this) {                                                   \
        DO_STUFF(h);                                                     \
        return h;                                                        \
    }                                                                    \
                                                                         \
    /* If the atom is in a read-only atomspace (i.e. if the parent    */ \
    /* is read-only) and this atomspace is read-write, then make      */ \
    /* a copy of the atom, and then set the value.                    */ \
    /* If this is a COW space, then always copy, no matter what.      */ \
    if (nullptr == has or has->_read_only or _copy_on_write or           \
        not in_environ(has))                                             \
    {                                                                    \
        /* Copy the atom into this atomspace                          */ \
        Handle copy(add(h, true));                                       \
        DO_STUFF(copy);                                                  \
        return copy;                                                     \
    }                                                                    \
                                                                         \
    DO_STUFF(h);                                                         \
    return h;


// Copy-on-write for setting values.
Handle AtomSpace::set_value(const Handle& h,
                            const Handle& key,
                            const ValuePtr& value)
{
   #define SETV(atm) atm->setValue(key, value);
	COWBOY_CODE(SETV);
}

// Copy-on-write for setting truth values.
Handle AtomSpace::set_truthvalue(const Handle& h, const TruthValuePtr& tvp)
{
   #define SET_TV(atm) atm->setTruthValue(tvp);
	COWBOY_CODE(SET_TV);
}

// Copy-on-write for incrementing truth values.
// The increment is atomic i.e. thread-safe.
Handle AtomSpace::increment_countTV(const Handle& h, double cnt)
{
	#define INC_TV(atm) atm->incrementCountTV(cnt);
	COWBOY_CODE(INC_TV);
}

// The increment is atomic i.e. thread-safe.
Handle AtomSpace::increment_count(const Handle& h, const Handle& key,
                                  const std::vector<double>& count)
{
	#define INCR_CNT(atm) atm->incrementCount(key, count);
	COWBOY_CODE(INCR_CNT);
}

// The increment is atomic i.e. thread-safe.
Handle AtomSpace::increment_count(const Handle& h, const Handle& key,
                                  size_t ref, double count)
{
	#define INCR_LOC(atm) atm->incrementCount(key, ref, count);
	COWBOY_CODE(INCR_LOC);
}

std::string AtomSpace::to_string(void) const
{
	std::stringstream ss;
	ss << *this;
	return ss.str();
}

/// Pretty-print with proper indentation.
/// The idea is that it should be easier to understand the hierarchy.
/// For example:
///    (use-modules (opencog))
///    (define space1 (cog-atomspace))
///    (define space2 (cog-new-atomspace space1))
///    (define space3 (cog-new-atomspace space2))
///    (define space4 (cog-new-atomspace space3))
///    (define space5a (cog-new-atomspace space4))
///    (define space5b (cog-new-atomspace space4))
///    (define space5c (cog-new-atomspace space4))
///    (define space6 (cog-new-atomspace space5a space5b space5c))
///    space6
///
std::string AtomSpace::to_string(const std::string& indent) const
{
	std::string sexpr = indent + "(AtomSpace \"" + _name + "\"";
	size_t szo = _outgoing.size();
	if (0 == szo)
		return sexpr + ")\n";

	if (1 == szo)
		return sexpr + "\n   " + _outgoing[0]->to_string(indent) + ")";

	sexpr += "\n";
	std::string idmore = indent + "   ";
	for (const Handle& ho : _outgoing)
		sexpr += ho->to_string(idmore) + "\n";
	sexpr += ")\n";

	return sexpr;
}

std::string AtomSpace::to_short_string(const std::string& indent) const
{
	return indent + "(AtomSpace \"" + _name + "\")";
}

namespace std {

ostream& operator<<(ostream& out, const opencog::AtomSpace& as)
{
    HandleSeq hseq;
    as.get_root_set_by_type(hseq, opencog::ATOM, true);
    for (const opencog::Handle& h : hseq)
		  out << h->to_string() << std::endl;
    return out;
}

} // namespace std
