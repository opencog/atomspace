
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

#include <opencog/persist/zmq/atomspace/ZMQClient.h>

using namespace opencog;

ZMQClient::ZMQClient(string networkAddress) {
	zmqContext = new zmq::context_t(1);
    zmqClientSocket = new zmq::socket_t(*zmqContext, ZMQ_REQ);
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
    string strRequest = requestMessage.SerializeAsString();
    zmq::message_t request(strRequest.size());
    memcpy((void *) request.data (), strRequest.c_str(),
            strRequest.size()); //TODO use copyless init from data
    zmqClientSocket->send(request);

    // Wait for reply
    zmq::message_t reply;
    zmqClientSocket->recv(&reply);
    replyMessage.ParseFromArray(reply.data(), reply.size());
}

void ZMQClient::reserve() {

}

void ZMQClient::store(const AtomTable &table) {

}

void ZMQClient::load(AtomTable &table) {

}

/**
 * Retrieve the entire incoming set of the indicated atom.
 */
std::vector<Handle> ZMQClient::getIncomingSet(Handle h)
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

    return NULL;
    //Atom* atom = ProtocolBufferSerializer::deserialize(rep.atom());
    //return AtomPtr(atom);
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
	return NULL;
}

/**
 * Create a new atom, retrieved from storage
 *
 * This method does *not* register the atom with any atomtable/atomspace
 * However, it does register with the TLB, as the SQL uuids and the
 * TLB Handles must be kept in sync, or all hell breaks loose.
 */
AtomPtr ZMQClient::getAtom(Handle &h)
{
	return NULL;
}

void ZMQClient::flushStoreQueue()
{

}


#endif /* HAVE_ZMQ_STORAGE */
/* ============================= END OF FILE ================= */
