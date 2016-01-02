/*
 * FUNCTION:
 * Persistent Atom storage, ZeroMQ-backed e.g. Neo4j.
 *
 * Copyright (C) 2008-2015 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Erwin Joosten, Hendy Irawan <ceefour666@gmail.com>
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

#include <stdlib.h>
#include <unistd.h>

#include <chrono>
#include <memory>
#include <thread>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/TruthValue.h>

#include <opencog/persist/zmq/atomspace/ZMQClient.h>

using namespace opencog;

ZMQClient::ZMQClient(string networkAddress) {
	zmqContext = new zmq::context_t(1);
    zmqClientSocket = new zmq::socket_t(*zmqContext, ZMQ_REQ);
    fprintf(stderr, "ZeroMQ connecting to %s\n", networkAddress.c_str());
    zmqClientSocket->connect(networkAddress.c_str());
}

ZMQClient::~ZMQClient() {
	delete zmqClientSocket;
	delete zmqContext;
}

bool ZMQClient::connected(void) {
	return zmqClientSocket->connected();
}

void ZMQClient::sendMessage(ZMQRequestMessage& requestMessage,
        ZMQReplyMessage& replyMessage)
{
    // Send request to server
    fprintf(stderr, "%s. Request... %s\n", connected() ? "Connected" : "NOT connected", requestMessage.DebugString().c_str());
    string strRequest = requestMessage.SerializeAsString();
    zmq::message_t request(strRequest.size());
    memcpy((void *) request.data (), strRequest.c_str(),
            strRequest.size()); //TODO use copyless init from data
    zmqClientSocket->send(request);

    // Wait for reply
    zmq::message_t reply;
    zmqClientSocket->recv(&reply);
    replyMessage.ParseFromArray(reply.data(), reply.size());
    fprintf(stderr, "Reply received... %s\n", replyMessage.DebugString().c_str());
}

void ZMQClient::reserve() {

}

/**
 * In Java: org.opencog.atomspace.zmq.ZmqBackingStore#storeAtomsAsync
 */
void ZMQClient::storeAtom(const AtomPtr& atomPtr, bool synchronous) {

	fprintf(stderr, "Storing atom %lu ...", atomPtr->getHandle().value());

    ZMQRequestMessage req;
    ZMQReplyMessage rep;

    req.set_function(ZMQstoreAtoms);
    ZMQAtomMessage *atomMsg = req.add_atom();
    atomMsg->set_handle(atomPtr->getHandle().value());
    atomMsg->set_type(atomPtr->getType());

//    ZMQTruthValueMessage *tvMsg = atomMsg->mutable_truthvalue();
//    ZMQSingleTruthValueMessage *stvMsg = tvMsg->add_singletruthvalue();
//    stvMsg->set_truthvaluetype(ZMQTruthValueTypeSimple);
//    stvMsg->set_mean(atomPtr->getTruthValue()->getMean());
//    stvMsg->set_confidence(atomPtr->getTruthValue()->getConfidence());
//    stvMsg->set_count(atomPtr->getTruthValue()->getCount());
//    atomMsg->set_allocated_truthvalue(tvMsg);

    LinkPtr linkPtr = dynamic_pointer_cast<Link>(atomPtr);
    if (linkPtr != NULL) {
    	atomMsg->set_atomtype(ZMQAtomTypeLink);
    	for (unsigned int i = 0; i < linkPtr->getOutgoingSet().size(); i++) {
    		atomMsg->add_outgoing(linkPtr->getOutgoingSet()[i].value());
    	}
    } else {
    	NodePtr nodePtr = dynamic_pointer_cast<Node>(atomPtr);
    	atomMsg->set_atomtype(ZMQAtomTypeNode);
    	atomMsg->set_name(nodePtr->getName());
    }

    sendMessage(req, rep);
}

/* ================================================================ */
/**
 * Store the single, indicated atom.
 * Store its truth values too.
 * The store is performed synchnously (in the calling thread).
 */
void ZMQClient::storeSingleAtom(AtomPtr atom)
{
	storeAtom(atom, true);
}

bool ZMQClient::store_cb(AtomPtr atom)
{
	storeSingleAtom(atom);
	store_count ++;
	if (store_count%1 == 0)
	{
		fprintf(stderr, "\tStored %lu atoms.\n", (unsigned long) store_count);
	}
	return false;
}

void ZMQClient::store(const AtomTable &table) {
	store_count = 0;
	table.foreachHandleByType(
	    [&](Handle h)->void { store_cb(h); }, ATOM, true);
}

void ZMQClient::loadType(AtomTable &, Type)
{
}

void ZMQClient::load(AtomTable &table) {

}

/**
 * Retrieve the entire incoming set of the indicated atom.
 */
std::vector<Handle> ZMQClient::getIncomingSet(Handle h)
{
	// TODO: implement
	std::vector<Handle> handles;
	return handles;
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
NodePtr ZMQClient::getNode(Type t, const char * str)
{
    ZMQRequestMessage req;
    ZMQReplyMessage rep;

    req.set_function(ZMQgetAtoms);
    ZMQAtomFetch *fetch1 = req.add_fetch();
    fetch1->set_kind(ZMQAtomFetchKind::NODE);
    fetch1->set_type(t);
    fetch1->set_name(str);
    sendMessage(req, rep);

    ZMQAtomMessage atomMsg = rep.atom(0);
    if (atomMsg.atomtype() == ZMQAtomTypeNode) {
        NodePtr nodePtr = dynamic_pointer_cast<Node>(ProtocolBufferSerializer::deserialize(atomMsg));
        return nodePtr;
    } else {
    	return NULL;
    }
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
LinkPtr ZMQClient::getLink(Type t, const std::vector<Handle>&oset)
{
    ZMQRequestMessage req;
    ZMQReplyMessage rep;

    req.set_function(ZMQgetAtoms);
    ZMQAtomFetch *fetch1 = req.add_fetch();
    fetch1->set_kind(ZMQAtomFetchKind::LINK);
    fetch1->set_type(t);
    for (unsigned int i = 0; i < oset.size(); i++) {
    	fetch1->add_outgoing(oset[i].value());
    }
    sendMessage(req, rep);

    ZMQAtomMessage atomMsg = rep.atom(0);
    if (atomMsg.atomtype() == ZMQAtomTypeLink) {
        LinkPtr linkPtr = dynamic_pointer_cast<Link>(ProtocolBufferSerializer::deserialize(atomMsg));
        return linkPtr;
    } else {
    	return NULL;
    }
}

/**
 * Create a new atom, retrieved from storage
 *
 * This method does *not* register the atom with any atomtable/atomspace
 * However, it does register with the TLB, as the SQL uuids and the
 * TLB Handles must be kept in sync, or all hell breaks loose.
 */
AtomPtr ZMQClient::getAtom(UUID uuid)
{
    ZMQRequestMessage req;
    ZMQReplyMessage rep;

    req.set_function(ZMQgetAtoms);
    ZMQAtomFetch *fetch1 = req.add_fetch();
    fetch1->set_kind(ZMQAtomFetchKind::UUID);
    fetch1->set_handle(uuid);
    sendMessage(req, rep);

    ZMQAtomMessage atomMsg = rep.atom(0);
    if (atomMsg.atomtype() != ZMQAtomTypeNotFound) {
        AtomPtr atomPtr = ProtocolBufferSerializer::deserialize(atomMsg);
        return atomPtr;
    } else {
    	return NULL;
    }
}

void ZMQClient::flushStoreQueue()
{

}

/* ============================= END OF FILE ================= */
