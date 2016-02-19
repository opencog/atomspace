/*
 * opencog/atomspace/AtomTable.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013-2015 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
 *            Welter Silva <welter@vettalabs.com>
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

#include "AtomTable.h"

#include <iterator>
#include <set>

#include <stdlib.h>
#include <boost/bind.hpp>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/DeleteLink.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/core/StateLink.h>
#include <opencog/atoms/core/TypedAtomLink.h>
#include <opencog/atoms/core/UniqueLink.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/ImplicationLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/functional.h>
#include <opencog/util/Logger.h>

//#define DPRINTF printf
//#define tableId (0) // Hack around some DPRINTF statements that want an old tableID member variable
#define DPRINTF(...)

using namespace opencog;

std::recursive_mutex AtomTable::_mtx;

AtomTable::AtomTable(AtomTable* parent, AtomSpace* holder, bool transient)
    : _index_queue(this, &AtomTable::put_atom_into_index, transient?0:4)
{
    _as = holder;
    _environ = parent;
    _uuid = TLB::reserve_extent(1);
    _size = 0;
    size_t ntypes = classserver().getNumberOfClasses();
    _size_by_type.resize(ntypes);

    // Set resolver before doing anything else, such as getting
    // the atom-added signals.  Just in case some other thread
    // is busy adding types while we are being created.
    Handle::set_resolver(this);

    // Connect signal to find out about type additions
    addedTypeConnection =
        classserver().addTypeSignal().connect(
            boost::bind(&AtomTable::typeAdded, this, _1));
}

AtomTable::~AtomTable()
{
    // Disconnect signals. Only then clear the resolver.
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    addedTypeConnection.disconnect();
    Handle::clear_resolver(this);

    // No one who shall look at these atoms shall ever again
    // find a reference to this atomtable.
    UUID undef = Handle::INVALID_UUID;
    for (auto pr : _atom_set) {
        pr.second->_atomTable = NULL;
        pr.second->_uuid = undef;
        // Aiee ... We added this link to every incoming set;
        // thus, it is our responsibility to remove it as well.
        // This is a stinky design, but I see no other way,
        // because it seems that we can't do this in the Atom
        // destructor (which is where this should be happening).
        LinkPtr lll(LinkCast(pr.second));
        if (lll) {
            for (AtomPtr a : lll->_outgoing) {
                a->remove_atom(lll);
            }
        }
    }
}

AtomTable& AtomTable::operator=(const AtomTable& other)
{
    throw opencog::RuntimeException(TRACE_INFO,
            "AtomTable - Cannot copy an object of this class");
}

AtomTable::AtomTable(const AtomTable& other)
    :_index_queue(this, &AtomTable::put_atom_into_index)
{
    throw opencog::RuntimeException(TRACE_INFO,
            "AtomTable - Cannot copy an object of this class");
}

Handle AtomTable::getHandle(Type t, std::string name) const
{
    // Special types need validation
    try {
        if (NUMBER_NODE == t) {
            name = NumberNode::validate(name);
        } else if (TYPE_NODE == t) {
            TypeNode::validate(name);
        }
    }
    catch (...) { return Handle::UNDEFINED; }

    std::lock_guard<std::recursive_mutex> lck(_mtx);
    Atom* atom = nodeIndex.getAtom(t, name);
    if (atom) return atom->getHandle();
    if (_environ and NULL == atom)
        return _environ->getHandle(t, name);
    return Handle::UNDEFINED;
}

Handle AtomTable::getHandle(Type t, const HandleSeq &seq) const
{
    // Make sure all the atoms in the outgoing set are resolved :-)
    HandleSeq resolved_seq;
    for (Handle ho : seq) {
        resolved_seq.emplace_back(getHandle(ho));
    }

    // Aiieee! unordered link!
    if (classserver().isA(t, UNORDERED_LINK)) {
        // Caution: this comparison function MUST BE EXACTLY THE SAME
        // as the one in Link.cc, used for sorting unordered links.
        // Changing this without changing the other one will break things!
        std::sort(resolved_seq.begin(), resolved_seq.end(), handle_less());
    }

    std::lock_guard<std::recursive_mutex> lck(_mtx);
    Handle h(linkIndex.getHandle(t, resolved_seq));
    if (_environ and nullptr == h)
        return _environ->getHandle(t, resolved_seq);
    return h;
}

/// Find an equivalent atom that is exactly the same as the arg. If
/// such an atom is in the table, it is returned, else the return
/// is the bad handle.
Handle AtomTable::getHandle(const AtomPtr& a) const
{
    if (nullptr == a) return Handle::UNDEFINED;

    if (in_environ(a))
        return a->getHandle();

    if (a->isNode())
        return getHandle(a->getType(), a->getName());
    else if (a->isLink())
        return getHandle(a->getType(), a->getOutgoingSet());

    return Handle::UNDEFINED;
}

// If we have a uuid but no atom pointer, find the atom pointer.
Handle AtomTable::getHandle(UUID uuid) const
{
    // Read-lock for the _atom_set.
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    auto hit = _atom_set.find(uuid);
    if (hit != _atom_set.end())
        return hit->second;
    return Handle::UNDEFINED;
}

/// Return true if the atom is in this atomtable, or in the
/// environment for this atomtable.
bool AtomTable::in_environ(const AtomPtr& atom) const
{
    if (nullptr == atom) return false;
    AtomTable* atab = atom->getAtomTable();
    const AtomTable* env = this;
    while (env) {
        if (atab == env) return true;
        env = env->_environ;
    }
    return false;
}

// Experimental C++ atom types support code
// Try to cast, if possible.
AtomPtr AtomTable::do_factory(Type atom_type, AtomPtr atom)
{
    // Nodes of various kinds -----------
    if (NUMBER_NODE == atom_type) {
        if (nullptr == NumberNodeCast(atom))
            return createNumberNode(*NodeCast(atom));
    } else if (TYPE_NODE == atom_type) {
        if (nullptr == TypeNodeCast(atom))
            return createTypeNode(*NodeCast(atom));

    // Links of various kinds -----------
    } else if (BIND_LINK == atom_type) {
        if (nullptr == BindLinkCast(atom))
            return createBindLink(*LinkCast(atom));
    } else if (PATTERN_LINK == atom_type) {
        if (nullptr == PatternLinkCast(atom))
            return createPatternLink(*LinkCast(atom));
    } else if (DEFINE_LINK == atom_type) {
        if (nullptr == DefineLinkCast(atom))
            return createDefineLink(*LinkCast(atom));
/*
    XXX FIXME: cannot do this, due to a circular shared library
    dependency between python and itself: python depends on
    ExecutionOutputLink, and ExecutionOutputLink depends on python.
    Boo.  I tried fixing this, but it is hard, somehow.

*/
    } else if (EVALUATION_LINK == atom_type) {
/*
        if (nullptr == EvaluationLinkCast(atom))
            return createEvaluationLink(*LinkCast(atom));
*/
    } else if (EXECUTION_OUTPUT_LINK == atom_type) {
/*
        if (nullptr == ExecutionOutputLinkCast(atom))
            return createExecutionOutputLink(*LinkCast(atom));
*/
    } else if (GET_LINK == atom_type) {
        if (nullptr == PatternLinkCast(atom))
            return createPatternLink(*LinkCast(atom));
    } else if (PUT_LINK == atom_type) {
        if (nullptr == PutLinkCast(atom))
            return createPutLink(*LinkCast(atom));
    } else if (SATISFACTION_LINK == atom_type) {
        if (nullptr == PatternLinkCast(atom))
            return createPatternLink(*LinkCast(atom));
    } else if (TYPED_ATOM_LINK == atom_type) {
        if (nullptr == TypedAtomLinkCast(atom))
            return createTypedAtomLink(*LinkCast(atom));
    } else if (UNIQUE_LINK == atom_type) {
        if (nullptr == UniqueLinkCast(atom))
            return createUniqueLink(*LinkCast(atom));
    } else if (VARIABLE_LIST == atom_type) {
        if (nullptr == VariableListCast(atom))
            return createVariableList(*LinkCast(atom));
    } else if (LAMBDA_LINK == atom_type) {
        if (nullptr == LambdaLinkCast(atom))
            return createLambdaLink(*LinkCast(atom));
    } else if (classserver().isA(atom_type, IMPLICATION_LINK)) {
        if (nullptr == ImplicationLinkCast(atom))
            return createImplicationLink(*LinkCast(atom));
    } else if (classserver().isA(atom_type, FUNCTION_LINK)) {
/* More circular-dependency heart-ache
        if (nullptr == FunctionLinkCast(atom))
            return FunctionLink::factory(LinkCast(atom));
*/
    } else if (classserver().isA(atom_type, SCOPE_LINK)) {
        // isA because we want to force alpha-conversion.
        if (nullptr == ScopeLinkCast(atom))
            return createScopeLink(*LinkCast(atom));
    }

    // Very special handling for DeleteLink's
    else if (DELETE_LINK == atom_type) {
        DeleteLinkPtr delp(DeleteLinkCast(atom));
        // If it can be cast, then its not an open term.
        if (nullptr != delp)
            return delp;

        // Trying to create a closed-term DeleteLink will throw.
        // This is a sign that we need to remove stuff.
        try {
            delp = createDeleteLink(*LinkCast(atom));
        }
        catch (...) {
            LinkPtr lp(LinkCast(atom));
            for (Handle ho : lp->getOutgoingSet()) {
                this->extract(ho);
            }
            return Handle();
        }
        return delp;
    }

    // Very special handling for StateLink's
    else if (STATE_LINK == atom_type) {
        StateLinkPtr slp(StateLinkCast(atom));
        if (NULL == slp)
            slp = createStateLink(*LinkCast(atom));

        // Compare to the current state
        Handle old_state(slp->get_other());
        while (nullptr != old_state) {
            this->extract(old_state, true);
            old_state = slp->get_other();
        }
        return slp;
    }
    return atom;
}

// create a clone
static AtomPtr do_clone_factory(Type atom_type, AtomPtr atom)
{
    // Nodes of various kinds -----------
    if (NUMBER_NODE == atom_type)
        return createNumberNode(*NodeCast(atom));
    if (TYPE_NODE == atom_type)
        return createTypeNode(*NodeCast(atom));
    if (classserver().isA(atom_type, NODE))
        return createNode(*NodeCast(atom));

    // Links of various kinds -----------
    if (BIND_LINK == atom_type)
        return createBindLink(*LinkCast(atom));
    if (PATTERN_LINK == atom_type)
        return createPatternLink(*LinkCast(atom));
    if (DEFINE_LINK == atom_type)
        return createDefineLink(*LinkCast(atom));
/*
    XXX FIXME: cannot do this, due to a circular shared library
    dependency between python and itself: python depends on
    ExecutionOutputLink, and ExecutionOutputLink depends on python.
    Boo.  I tried fixing this, but it is hard, somehow.
*/
    if (EVALUATION_LINK == atom_type)
        // return createEvaluationLink(*LinkCast(atom));
        return createLink(*LinkCast(atom));
    if (EXECUTION_OUTPUT_LINK == atom_type)
        //return createExecutionOutputLink(*LinkCast(atom));
        return createLink(*LinkCast(atom));
    if (GET_LINK == atom_type)
        return createPatternLink(*LinkCast(atom));
    if (PUT_LINK == atom_type)
        return createPutLink(*LinkCast(atom));
    if (SATISFACTION_LINK == atom_type)
        return createPatternLink(*LinkCast(atom));
    if (STATE_LINK == atom_type)
        return createStateLink(*LinkCast(atom));
    if (TYPED_ATOM_LINK == atom_type)
        return createTypedAtomLink(*LinkCast(atom));
    if (UNIQUE_LINK == atom_type)
        return createUniqueLink(*LinkCast(atom));
    if (VARIABLE_LIST == atom_type)
        return createVariableList(*LinkCast(atom));
    if (LAMBDA_LINK == atom_type)
        return createLambdaLink(*LinkCast(atom));
    if (classserver().isA(atom_type, IMPLICATION_LINK))
        return createImplicationLink(*LinkCast(atom));
    if (classserver().isA(atom_type, FUNCTION_LINK))
        // XXX FIXME more circular-dependency heart-ache
        // return FunctionLink::factory(LinkCast(atom));
        return createLink(*LinkCast(atom));

    // isA because we want to force alpha-conversion.
    if (classserver().isA(atom_type, SCOPE_LINK))
        return createScopeLink(*LinkCast(atom));

    if (classserver().isA(atom_type, LINK))
        return createLink(*LinkCast(atom));

    throw RuntimeException(TRACE_INFO,
          "AtomTable - failed factory call!");
}

AtomPtr AtomTable::factory(Type atom_type, AtomPtr atom)
{
	AtomPtr clone(do_factory(atom_type, atom));
	if (nullptr == clone) return clone;
	clone->_uuid = atom->_uuid;
	return clone;
}

/// The purpose of the clone factory is to create a private, unique
/// copy of the atom, so as to avoid accidental, unintentional
/// sharing with others. In particular, the atom that we are given
/// may already exist in some other atomspace; we want our own private
/// copy, in that case.
AtomPtr AtomTable::clone_factory(Type atom_type, AtomPtr atom)
{
	AtomPtr clone(do_clone_factory(atom_type, atom));
	// Copy the UUID ONLY if the atom does not belong to some other
	// atomspace. This is the situation that applies to atoms being
	// delivered to us from the backing store: the UUID is set, but
	// they are othrwise "fresh" atoms.
	if (NULL == atom->getAtomTable())
		clone->_uuid = atom->_uuid;
	return clone;
}

#if 0
static void prt_diag(AtomPtr atom, size_t i, size_t arity, const HandleSeq& ogs)
{
    Logger::Level save = logger().getBackTraceLevel();
    logger().setBackTraceLevel(Logger::Level::NONE);
    logger().error() << "AtomTable - Insert link with "
               "invalid outgoing members";
    logger().error() << "Failing index i=" << i
                    << " and arity=" << arity;
    logger().error() << "Failing outset is this:";
    for (unsigned int fk=0; fk<arity; fk++)
        logger().error() << "outset i=" << fk
                        << " uuid=" << ogs[fk].value();

    logger().error() << "link is " << atom->toString();
    logger().flush();
    logger().setBackTraceLevel(save);
}
#endif

Handle AtomTable::add(AtomPtr atom, bool async)
{
    // Can be null, if its a PseudoAtom
    if (nullptr == atom) return Handle::UNDEFINED;

    // Is the atom already in this table, or one of its environments?
    if (in_environ(atom))
        return atom->getHandle();

    // Lock before checking to see if this kind of atom can already
    // be found in the atomspace.  We need to lock here, to avoid two
    // different threads from trying to add exactly the same atom.
    std::unique_lock<std::recursive_mutex> lck(_mtx);

    // Check again, under the lock this time.
    if (in_environ(atom))
        return atom->getHandle();

    // We expect to be given a valid atom...
    if (nullptr == atom)
        throw RuntimeException(TRACE_INFO,
            "AtomTable - Cannot insert null atom! ");

    // Factory implements experimental C++ atom types support code
    AtomPtr orig(atom);
    Type atom_type = atom->getType();
    atom = factory(atom_type, atom);

    // Certain DeleteLinks can never be added!
    if (nullptr == atom) return Handle();

    // Is the equivalent of this atom already in the table?
    // If so, then return the existing atom.  (Note that this 'existing'
    // atom might be in another atomspace, or might not be in any
    // atomspace yet.)
    Handle hexist(getHandle(atom));
    if (hexist) return hexist;

    // If this atom is in some other atomspace or not in any atomspace,
    // then we need to clone it. We cannot insert it into this atomtable
    // as-is.  (We already know that its not in this atomspace, or its
    // environ.)
    LinkPtr lll(LinkCast(atom));
    if (lll) {
        // Well, if the link was in some other atomspace, then
        // the outgoing set will probably be too. (It might not
        // be if the other atomspace is a child of this one).
        // So we recursively clone that too.
        HandleSeq closet;
        for (const Handle& h : lll->getOutgoingSet()) {
            // operator->() will be null if its a ProtoAtom that is
            // not an atom.
            if (nullptr == h.operator->()) return Handle::UNDEFINED;
            closet.emplace_back(add(h, async));
        }
        // Preserve the UUID! This is needed for assigning the UUID
        // correctly when fetching from backing store. But do this
        // if the atom is actually coming from backing store (i.e.
        // does not yet belong to any table).
        UUID save = atom->_uuid;
        if (NULL != atom->getAtomTable()) save = (UUID) -1;
        atom = createLink(atom_type, closet,
                          atom->getTruthValue(),
                          atom->getAttentionValue());
        atom = clone_factory(atom_type, atom);
        atom->_uuid = save;
    }

    // Clone, if we haven't done so already. We MUST maintain our own
    // private copy of the atom, else crazy things go wrong.
    if (atom == orig)
        atom = clone_factory(atom_type, atom);

    // Sometimes one inserts an atom that was previously deleted.
    // In this case, the removal flag might still be set. Clear it.
    atom->unsetRemovalFlag();

    // Check for bad outgoing set members; fix them up if needed.
    // "bad" here means outgoing set members that have UUID's but
    // no pointers to actual atoms.  We want to have the actual atoms,
    // because later steps need the pointers to do stuff, in particular,
    // to make sure the child atoms are in an atomtable, too.
    lll = LinkCast(atom);
    if (lll) {
        const HandleSeq& ogs(lll->getOutgoingSet());
        size_t arity = ogs.size();

        // First, make sure that every member of the outgoing set has
        // a valid atom pointer. We need this, cause we need to call
        // methods on those atoms.
        bool need_copy = false;
        for (size_t i = 0; i < arity; i++) {
            // The outgoing set must consist entirely of atoms that
            // are either in this atomtable, or its environment.
            if (not in_environ(ogs[i])) need_copy = true;
        }

        if (need_copy) {
            atom = clone_factory(atom_type, atom);
        }

        // llc not lll, in case a copy was made.
        LinkPtr llc(LinkCast(atom));
        for (size_t i = 0; i < arity; i++) {

            // Make sure all children have correct incoming sets
            Handle ho(llc->_outgoing[i]);
            if (not in_environ(ho)) {
                ho->remove_atom(llc);
                llc->_outgoing[i] = add(ho, async);
            }
            // Build the incoming set of outgoing atom h.
            llc->_outgoing[i]->insert_atom(llc);
        }

        // OK, so if the above fixed up the outgoing set, and
        // this is an unordered link, then we have to fix it up
        // and put it back into the default sort order. That's
        // because the default sort order uses UUID's, which have
        // now changed.
        if (classserver().isA(llc->getType(), UNORDERED_LINK)) {
            llc->resort();
        }
    }

    // Its possible that the atom already has a UUID assigned,
    // e.g. if it was fetched from persistent storage; this
    // was done to preserve handle consistency.
    if (atom->_uuid == Handle::INVALID_UUID) {
       // Atom doesn't yet have a valid uuid assigned to it. Ask the TLB
       // to issue a valid uuid.  And then memorize it.
       TLB::addAtom(atom);
    } else {
       TLB::reserve_upto(atom->_uuid);
    }
    Handle h(atom->getHandle());
    _size++;
    _size_by_type[atom->_type] ++;
    _atom_set.insert({atom->_uuid, h});

    atom->keep_incoming_set();
    atom->setAtomTable(this);

    if (not async)
        put_atom_into_index(atom);

    // We can now unlock, since we are done.
    lck.unlock();

    // Update the indexes asynchronously
    if (async)
        _index_queue.enqueue(atom);

    DPRINTF("Atom added: %ld => %s\n", atom->_uuid, atom->toString().c_str());
    return h;
}

void AtomTable::put_atom_into_index(AtomPtr& atom)
{
    std::unique_lock<std::recursive_mutex> lck(_mtx);
    Atom* pat = atom.operator->();
    nodeIndex.insertAtom(pat);
    linkIndex.insertAtom(atom);
    typeIndex.insertAtom(pat);
    importanceIndex.insertAtom(pat);

    // We can now unlock, since we are done. In particular, the signals
    // need to run unlocked, since they may result in more atom table
    // additions.
    lck.unlock();

    // Now that we are completely done, emit the added signal.
    // Don't emit signal until after the indexes are updated!
    _addAtomSignal(atom->getHandle());
}

void AtomTable::barrier()
{
    _index_queue.flush_queue();
}

size_t AtomTable::getSize() const
{
    return _size;
}

size_t AtomTable::getNumNodes() const
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    return nodeIndex.size();
}

size_t AtomTable::getNumLinks() const
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    return linkIndex.size();
}

size_t AtomTable::getNumAtomsOfType(Type type, bool subclass) const
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);

    size_t result = _size_by_type[type];
    if (subclass)
    {
        // Also count subclasses of this type, if need be.
        Type ntypes = _size_by_type.size();
        for (Type t = ATOM; t<ntypes; t++)
        {
            if (t != type and classserver().isA(type, t))
                result += _size_by_type[t];
        }
    }

    if (_environ)
        result += _environ->getNumAtomsOfType(type, subclass);

    return result;
}

Handle AtomTable::getRandom(RandGen *rng) const
{
    size_t x = rng->randint(getSize());

    Handle randy(Handle::UNDEFINED);

    // XXX TODO it would be considerably more efficient to go into the
    // the type index, and decrement x by the size of the index for
    // each type.  This would speed up the algo by about 100 (by about
    // the number of types that are in use...).
    foreachHandleByType(
        [&](Handle h)->void {
            if (0 == x) randy = h;
            x--;
        },
        ATOM, true);
    return randy;
}

AtomPtrSet AtomTable::extract(Handle& handle, bool recursive)
{
    AtomPtrSet result;

    // Make sure the atom is fully resolved before we go about
    // deleting it.
    handle = getHandle(handle);
    AtomPtr atom(handle);
    if (nullptr == atom or atom->isMarkedForRemoval()) return result;

    // Perhaps the atom is not in any table? Or at least, not in this
    // atom table? Its a user-error if the user is trying to extract
    // atoms that are not in this atomspace, but we're going to be
    // silent about this error -- it seems pointless to throw.
    AtomTable* other = atom->getAtomTable();
    if (other != this)
    {
        if (not in_environ(handle)) return result;
        return other->extract(handle, recursive);
    }

    // Lock before fetching the incoming set. Since getting the
    // incoming set also grabs a lock, we need this mutex to be
    // recursive. We need to lock here to avoid confusion if multiple
    // threads are trying to delete the same atom.
    std::unique_lock<std::recursive_mutex> lck(_mtx);

    if (atom->isMarkedForRemoval()) return result;
    atom->markForRemoval();

    // If recursive-flag is set, also extract all the links in the atom's
    // incoming set
    if (recursive) {
        // We need to make a copy of the incoming set because the
        // recursive call will trash the incoming set when the atom
        // is removed.
        IncomingSet is(handle->getIncomingSet());

        IncomingSet::iterator is_it = is.begin();
        IncomingSet::iterator is_end = is.end();
        for (; is_it != is_end; ++is_it)
        {
            Handle his(*is_it);
            DPRINTF("[AtomTable::extract] incoming set: %s",
                 (his) ? his->toString().c_str() : "INVALID HANDLE");

            // Something is seriously screwed up if the incoming set
            // is not in this atomtable, and its not a child of this
            // atom table.  So flag that as an error; it will assert
            // a few dozen lines later, below.
            AtomTable* other = his->getAtomTable();
            if (other and other != this and not other->in_environ(handle)) {
                logger().warn() << "AtomTable::extract() internal error, "
                                << "non-DAG membership.";
            }
            if (not his->isMarkedForRemoval()) {
                DPRINTF("[AtomTable::extract] marked for removal is false");
                if (other) {
                    AtomPtrSet ex = other->extract(his, true);
                    result.insert(ex.begin(), ex.end());
                }
            }
        }
    }

    // The check is done twice: the call to getIncomingSetSize() can
    // return a non-zero value if the incoming set has weak pointers to
    // deleted atoms. Thus, a second check is made for strong pointers,
    // since getIncomingSet() converts weak to strong.
    if (0 < handle->getIncomingSetSize())
    {
        IncomingSet iset(handle->getIncomingSet());
        if (0 < iset.size())
        {
            if (not recursive)
            {
                // User asked for a non-recursive remove, and the
                // atom is still referenced. So, do nothing.
                handle->unsetRemovalFlag();
                return result;
            }

            // Check for an invalid condition that should not occur. See:
            // https://github.com/opencog/opencog/commit/a08534afb4ef7f7e188e677cb322b72956afbd8f#commitcomment-5842682
            size_t ilen = iset.size();
            for (size_t i=0; i<ilen; i++)
            {
                // Its OK if the atom being extracted is in a link
                // that is not currently in any atom space, or if that
                // link is in a child subspace, in which case, we
                // extract from the child.
                //
                // A bit of a race can happen: when the unlock is
                // done below, to send the removed signal, another
                // thread can sneak in and get to here, if it is
                // deleting a different atom with an overlapping incoming
                // set.  Since the incoming set hasn't yet been updated
                // (that happens after re-acquiring the lock),
                // it will look like the incoming set has not yet been
                // fully cleared.  Well, it hasn't been, but as long as
                // we are marked for removal, things should end up OK.
                //
                // XXX this might not be exactly thread-safe, if
                // other atomspaces are involved...
                if (iset[i]->getAtomTable() != NULL and
                    (not iset[i]->getAtomTable()->in_environ(handle) or
                     not iset[i]->isMarkedForRemoval()))
                {
                    Logger::Level lev = logger().get_backtrace_level();
                    logger().set_backtrace_level(Logger::ERROR);
                    logger().warn() << "AtomTable::extract() internal error";
                    logger().warn() << "Non-empty incoming set of size "
                                    << ilen << " First trouble at " << i;
                    logger().warn() << "This atomtable=" << ((void*) this)
                                    << " other atomtale=" << ((void*) iset[i]->getAtomTable())
                                    << " in_environ=" << iset[i]->getAtomTable()->in_environ(handle);
                    logger().warn() << "This atom: " << handle->toString();
                    for (size_t j=0; j<ilen; j++) {
                        logger().warn() << "Atom j=" << j << " " << iset[j]->toString();
                        logger().warn() << "Marked: " << iset[j]->isMarkedForRemoval()
                                        << " Table: " << ((void*) iset[j]->getAtomTable());
                    }
                    logger().set_backtrace_level(lev);
                    atom->unsetRemovalFlag();
                    throw RuntimeException(TRACE_INFO,
                        "Internal Error: Cannot extract an atom with "
                        "a non-empty incoming set!");
                }
            }
        }
    }

    // Issue the atom removal signal *BEFORE* the atom is actually
    // removed.  This is needed so that certain subsystems, e.g. the
    // Agent system activity table, can correctly manage the atom;
    // it needs info that gets blanked out during removal.
    lck.unlock();
    _removeAtomSignal(atom);
    lck.lock();

    // Decrements the size of the table
    _size--;
    _size_by_type[atom->_type] --;
    _atom_set.erase(atom->_uuid);

    Atom* pat = atom.operator->();
    nodeIndex.removeAtom(pat);
    linkIndex.removeAtom(atom);
    typeIndex.removeAtom(pat);
    LinkPtr lll(LinkCast(atom));
    if (lll) {
        for (AtomPtr a : lll->_outgoing) {
            a->remove_atom(lll);
        }
    }
    importanceIndex.removeAtom(pat);

    // XXX Setting the atom table causes AVChanged signals to be emitted.
    // We should really do this unlocked, but I'm too lazy to fix, and
    // am hoping no one will notice. This will probably need to be fixed
    // someday.
    atom->setAtomTable(NULL);

    result.insert(atom);
    return result;
}

// This is the resize callback, when a new type is dynamically added.
void AtomTable::typeAdded(Type t)
{
    std::lock_guard<std::recursive_mutex> lck(_mtx);
    //resize all Type-based indexes
    size_t new_size = classserver().getNumberOfClasses();
    _size_by_type.resize(new_size);
    nodeIndex.resize();
    linkIndex.resize();
    typeIndex.resize();
}

