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
#include <string>
#include <thread>
#include <vector>

#include <zmq.hpp>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/types.h>

#include "opencog/persist/zmq/atomspace/ZMQMessages.pb.h"
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
		int store_count = 0;

	protected:
		void sendMessage(ZMQRequestMessage& requestMessage,
		        ZMQReplyMessage& replyMessage);
		void storeSingleAtom(AtomPtr atom);
		bool store_cb(AtomPtr atom);

	public:
		ZMQClient(string networkAddress = "tcp://127.0.0.1:5555"); //"ipc:///tmp/AtomSpaceZMQ.ipc"
		~ZMQClient();

		bool connected(void); // connection to DB is alive

		// Store atoms to DB
//		void storeSingleAtom(AtomPtr);
		void storeAtom(const AtomPtr& atomPtr, bool synchronous = false);
		void flushStoreQueue();

		// Fetch atoms from DB
		AtomPtr getAtom(UUID);
		Handle getNode(Type, const char *);
		Handle getLink(Type, const HandleSeq&);

		// Large-scale loads and saves
		void loadType(AtomTable &, Type); // Load *all* atoms of type
		void load(AtomTable &); // Load entire contents of DB
		void getIncomingSet(AtomTable&, const Handle&);
		void getIncomingByType(AtomTable&, const Handle&, Type);
		void store(const AtomTable &); // Store entire contents of AtomTable
		void reserve(void);     // reserve range of UUID's

};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PERSISTENT_ZMQ_STORAGE_H
