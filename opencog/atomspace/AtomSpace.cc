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
#include <opencog/atoms/parallel/TriggerLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/ValueFactory.h>

#include "AtomSpace.h"

using namespace opencog;

// ====================================================================

void AtomSpace::init(void)
{
    // Timestamp provides the primary ID, which the user might
    // over-write with a given name. Normally, the timestamp is
    // (almost) enough to provide a distinct, unique name...
    // expect in cases of extreme multi-threading, when multiple
    // thread can end up with the same timestamp, even at the
    // nanosecond level, dependong on the OS, scheduling, interrupts,
    // etc. So we further disambiguate (DAB) with a per-session counter.
    static std::atomic_uint64_t dabcnt(1);

    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    char buf[64];
    strftime(buf, sizeof(buf), "/%Y-%m-%d %H:%M:%S", gmtime(&ts.tv_sec));
    sprintf(buf + strlen(buf), ".%09ld/%ld", ts.tv_nsec, dabcnt.fetch_add(1));

    _name = buf;

    // Connect signal to find out about type additions
    addedTypeConnection =
        _nameserver.typeAddedSignal().connect(
            &AtomSpace::typeAdded, this);
}

/**
 * Transient atomspaces are intended for use as scratch spaces, to hold
 * temporary results during evaluation, pattern matching and inference.
 */
AtomSpace::AtomSpace(AtomSpace* parent, bool transient) :
    Frame(ATOM_SPACE),
    _read_only(false),
    _copy_on_write(transient),
    _nameserver(nameserver()),
    addedTypeConnection(0)
{
    if (parent) {
        // Set the COW flag by default, for any Atomspace that sits on
        // top of another one. This provides a "common-sense" behavior
        // that most users would expect.
        _copy_on_write = true;
        _environ.push_back(AtomSpaceCast(parent));
        _outgoing.push_back(HandleCast(parent));
    }
    init();
}

AtomSpace::AtomSpace(const AtomSpacePtr& parent) :
    Frame(ATOM_SPACE),
    _read_only(false),
    _copy_on_write(false),
    _nameserver(nameserver()),
    addedTypeConnection(0)
{
    if (nullptr != parent) {
        // Set the COW flag by default; it seems like a simpler
        // default than setting it to be write-through.
        _copy_on_write = true;
        _environ.push_back(parent);
        _outgoing.push_back(HandleCast(parent));
    }

    init();
}

AtomSpace::AtomSpace(const HandleSeq& bases) :
    Frame(ATOM_SPACE, bases),
    _read_only(false),
    _copy_on_write(false),
    _nameserver(nameserver()),
    addedTypeConnection(0)
{
    if (0 < bases.size()) _copy_on_write = true;
    init();
}

AtomSpace::~AtomSpace()
{
    _nameserver.typeAddedSignal().disconnect(addedTypeConnection);
    clear_all_atoms();
}

/// Set up a vector of pointers to the parent AtomSpaces.
void AtomSpace::install(void)
{
    if (0 < _environ.size())
    {
        Frame::install();
        return;
    }

    for (const Handle& base : _outgoing)
    {
        // Easy. Just copy cast.
        if (_nameserver.isA(base->get_type(), ATOM_SPACE))
        {
            _environ.push_back(AtomSpaceCast(base));
            continue;
        }

        // The provided Atom in the outgoing set might be
        // executable and so we need to execute it, and
        // hopefully get an AtomSpace back. The only tricky
        // part here is that we might have been called from
        // Sexpr::decode_atom(), and so `base` might not be
        // in any AtomSpace, yet(!) So we put it in this one.
        // Duhh. Just like everything else in the world.
        if (base->is_executable())
        {
            Handle bh(base);
            if (nullptr == base->getAtomSpace())
                bh = _atom_space->add_atom(base);

            ValuePtr vp(bh->execute(this));
            AtomSpacePtr as(AtomSpaceCast(vp));
            if (nullptr == as)
                throw RuntimeException(TRACE_INFO,
                    "AtomSpace - executable base not an AtomSpace! Got %s",
                     vp->to_string().c_str());
            _environ.push_back(as);
            continue;
        }

        throw RuntimeException(TRACE_INFO,
            "AtomSpace - bases must be AtomSpaces! Got %s",
             base->to_string().c_str());
    }

    // XXX FIXME: Frame::install loops over _outgoing but maybe it
    // should be looping over _environ, instead? Maybe we should copy
    // _environ to _outgoing?
    Frame::install();
}

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
    if (space_first.get_num_atoms_of_type(ATOM, true) !=
        space_second.get_num_atoms_of_type(ATOM, true))
    {
        if (emit_diagnostics)
            std::cout << "compare_atomspaces - size " <<
                    space_first.get_num_atoms_of_type(ATOM, true) <<
                    " != size " <<
                    space_second.get_num_atoms_of_type(ATOM, true) <<
                    std::endl;
        return false;
    }

    // Compare each individual atom.
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
        if (*atom_first != *atom_second)
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
                    std::cout << "compare_atomspaces - key set mismatch for "
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

// Imlementation is the same as Node::operator<()
bool AtomSpace::operator<(const Atom& other) const
{
    // If other points to this, then have equality.
    if (this == &other) return false;

    ContentHash cht = get_hash();
    ContentHash cho = other.get_hash();
    if (cht != cho) return cht < cho;

    // We get to here only if the hashes are equal. This is
    // extremely unlikely; it requires a 64-bit hash collision.
    // Compare the contents directly, for this case.
    if (get_type() != other.get_type())
        return get_type() < other.get_type();

    return get_name() < other.get_name();
}

ContentHash AtomSpace::compute_hash() const
{
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
    try {
        return add(h);
    }
    catch (const DeleteException& ex) { /* Do nothing */ }
    catch (const ValueReturnException& ex) {
        // Rethrow to pass results onwards (to scheme/python bindings)
        throw;
    }
    catch (const SilentException& ex) {
        // The SilentException is thrown by GrantLink, when the
        // user attempts grants in non-base Frames. We want to
        // disallow hiding of grants, so we end up here.
        return lookupHide(h, false);  // Do not allow hiding!
    }
    return Handle::UNDEFINED;
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
	// Skip R/O and COW checking if key is a message.
	if (h->usesMessage(key))
	{
		h->setValue(key, value);
		return h;
	}

	#define SETV(atm) atm->setValue(key, value);
	COWBOY_CODE(SETV);
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
