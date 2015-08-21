
/*
 * FUNCTION:
 * Persistent Atom storage, ZeroMQ-backed e.g. Neo4j.
 *
 * Copyright (c) 2015 Hendy Irawan <ceefour666@gmail.com>
 *
 * LICENSE:
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
#ifdef HAVE_ZMQ

#include <stdlib.h>
#include <unistd.h>

#include <chrono>
#include <memory>
#include <thread>

#include <opencog/util/oc_assert.h>
#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/CountTruthValue.h>
#include <opencog/atomspace/IndefiniteTruthValue.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
#include <opencog/atomspace/ProbabilisticTruthValue.h>
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atomspace/TLB.h>
#include <opencog/atomspace/TruthValue.h>

#include "ZMQStorage.h"

using namespace opencog;

ZMQStorage::ZMQStorage() {

}

ZMQStorage::~ZMQStorage() {

}

bool ZMQStorage::connected(void) {
	return false;
}

void ZMQStorage::reserve() {

}

void ZMQStorage::store(const AtomTable &table) {

}

void ZMQStorage::load(AtomTable &table) {

}

/**
 * Retreive the entire incoming set of the indicated atom.
 */
std::vector<Handle> ZMQStorage::getIncomingSet(Handle h)
{

}

/**
 * Fetch Node from database, with the indicated type and name.
 * If there is no such node, NULL is returned.
 * More properly speaking, the point of this routine is really
 * to fetch the associated TruthValue for this node.
 *
 * This method does *not* register the atom with any atomtable/atomspace
 * However, it does register with the TLB, as the SQL uuids and the
 * TLB Handles must be kept in sync, or all hell breaks loose.
 */
NodePtr ZMQStorage::getNode(Type t, const char * str)
{
	return NULL;
}

/**
 * Fetch Link from database, with the indicated type and outgoing set.
 * If there is no such link, NULL is returned.
 * More properly speaking, the point of this routine is really
 * to fetch the associated TruthValue for this link.
 *
 * This method does *not* register the atom with any atomtable/atomspace
 * However, it does register with the TLB, as the SQL uuids and the
 * TLB Handles must be kept in sync, or all hell breaks loose.
 */
LinkPtr ZMQStorage::getLink(Type t, const std::vector<Handle>&oset)
{
	return NULL;
}

/**
 * Create a new atom, retrieved from storage
 *
 * This method does *not* register the atom with any atomtable/atomspace
 * However, it does register with the TLB, as the SQL uuids and the
 * TLB Handles must be kept in sync, or all hell breaks loose.
 */
AtomPtr ZMQStorage::getAtom(Handle h)
{
	return NULL;
}

void ZMQStorage::flushStoreQueue()
{

}


#endif /* HAVE_ZMQ_STORAGE */
/* ============================= END OF FILE ================= */
