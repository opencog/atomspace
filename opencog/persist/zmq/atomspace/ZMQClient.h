/*
 * FUNCTION:
 * Base class for ZeroMQ-backed persistent storage.
 *
 * HISTORY:
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

#ifndef _OPENCOG_PERSISTENT_ZMQ_STORAGE_H
#define _OPENCOG_PERSISTENT_ZMQ_STORAGE_H

#include <atomic>
#include <mutex>
#include <set>
#include <thread>
#include <vector>
#include <string>

#include <zmq.hpp>
#include <opencog/util/async_method_caller.h>
#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/types.h>

#include "ZMQMessages.pb.h"
#include "ProtocolBufferSerializer.h"

using namespace std;

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class ZMQClient
{
	private:
		zmq::context_t *zmqContext;
		zmq::socket_t *zmqClientSocket;

	protected:
		void sendMessage(ZMQRequestMessage& requestMessage,
		        ZMQReplyMessage& replyMessage);
	public:
		ZMQClient(string networkAddress="tcp://127.0.0.1:5555"); //"ipc:///tmp/AtomSpaceZMQ.ipc"
		~ZMQClient();

		bool connected(void); // connection to DB is alive

		// Store atoms to DB
//		void storeSingleAtom(AtomPtr);
		void storeAtom(AtomPtr, bool synchronous = false);
		void flushStoreQueue();

		// Fetch atoms from DB
//		bool atomExists(Handle);
		AtomPtr getAtom(Handle &h);
		std::vector<Handle> getIncomingSet(Handle);
		NodePtr getNode(Type, const char *);
		NodePtr getNode(const Node &n)
		{
			return getNode(n.getType(), n.getName().c_str());
		}
		LinkPtr getLink(Type, const std::vector<Handle>&);
		LinkPtr getLink(const Link &l)
		{
			return getLink(l.getType(), l.getOutgoingSet());
		}

		// Large-scale loads and saves
		void loadType(AtomTable &, Type); // Load *all* atoms of type
		void load(AtomTable &); // Load entire contents of DB
		void store(const AtomTable &); // Store entire contents of AtomTable
		void reserve(void);     // reserve range of UUID's

};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PERSISTENT_ZMQ_STORAGE_H
