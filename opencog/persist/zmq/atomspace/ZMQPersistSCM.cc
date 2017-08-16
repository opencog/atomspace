/*
 * opencog/persist/zmq/atomspace/ZMQPersistSCM.cc
 *
 * Copyright (c) 2008-2015 by OpenCog Foundation
 * Written by Hendy Irawan <ceefour666@gmail.com>
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
#ifdef HAVE_GUILE

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/persist/zmq/atomspace/ZMQClient.h>

#include "ZMQPersistSCM.h"

using namespace opencog;

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class ZMQBackingStore : public BackingStore
{
	private:
		ZMQClient *_store;
	public:
		ZMQBackingStore();
		void set_store(ZMQClient *);

		virtual Handle getNode(Type, const char *) const;
		virtual Handle getLink(Type, const HandleSeq&) const;
		virtual AtomPtr getAtom(UUID) const;
		virtual void storeAtom(const Handle&);
		virtual void removeAtom(const Handle&, bool);
		virtual void loadType(AtomTable&, Type);
		virtual void getIncomingSet(AtomTable&, const Handle&);
		virtual void getIncomingByType(AtomTable&, const Handle&, Type);
		virtual void getValuations(AtomTable&, const Handle&, bool);
		virtual void barrier();
};

/** @}*/
}  // namespace

ZMQBackingStore::ZMQBackingStore()
{
	_store = NULL;
}

void ZMQBackingStore::set_store(ZMQClient *as)
{
	_store = as;
}

Handle ZMQBackingStore::getNode(Type t, const char *name) const
{
	return _store->getNode(t, name);
}

Handle ZMQBackingStore::getLink(Type t, const HandleSeq& hseq) const
{
	return _store->getLink(t, hseq);
}

AtomPtr ZMQBackingStore::getAtom(UUID uuid) const
{
	return _store->getAtom(uuid);
}

void ZMQBackingStore::getIncomingSet(AtomTable& table, const Handle& h)
{
	_store->getIncomingSet(table, h);
}

void ZMQBackingStore::getIncomingByType(AtomTable& table, const Handle& h, Type t)
{
	_store->getIncomingByType(table, h, t);
}

void ZMQBackingStore::getValuations(AtomTable& table, const Handle& key, bool get_all)
{
	_store->getValuations(table, key, get_all);
}

void ZMQBackingStore::storeAtom(const Handle& h)
{
	_store->storeAtom(h);
}

void ZMQBackingStore::removeAtom(const Handle& h, bool recursive)
{
	_store->removeAtom(h, recursive);
}

void ZMQBackingStore::loadType(AtomTable& at, Type t)
{
	_store->loadType(at, t);
}

void ZMQBackingStore::barrier()
{
	_store->flushStoreQueue();
}

// ==================================================================

ZMQPersistSCM::ZMQPersistSCM(AtomSpace *as)
{
	_as = as;
	_store = NULL;
	_backing = new ZMQBackingStore();

	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	scm_with_guile(init_in_guile, this);
}

void* ZMQPersistSCM::init_in_guile(void* self)
{
	scm_c_define_module("opencog persist-zmq", init_in_module, self);
	scm_c_use_module("opencog persist-zmq");
	return NULL;
}

void ZMQPersistSCM::init_in_module(void* data)
{
   ZMQPersistSCM* self = (ZMQPersistSCM*) data;
   self->init();
}

void ZMQPersistSCM::init(void)
{
	define_scheme_primitive("zmq-open", &ZMQPersistSCM::do_open, this, "persist-zmq");
	define_scheme_primitive("zmq-close", &ZMQPersistSCM::do_close, this, "persist-zmq");
	define_scheme_primitive("zmq-load", &ZMQPersistSCM::do_load, this, "persist-zmq");
	define_scheme_primitive("zmq-store", &ZMQPersistSCM::do_store, this, "persist-zmq");
}

ZMQPersistSCM::~ZMQPersistSCM()
{
	delete _backing;
}

void ZMQPersistSCM::do_open(const std::string& networkAddress)
{
	_store = new ZMQClient(networkAddress);
	if (!_store)
		throw RuntimeException(TRACE_INFO,
			"zmq-open: Error: Unable to open ZeroMQ-based persistence");

	if (!_store->connected())
	{
		delete _store;
		_store = NULL;
		throw RuntimeException(TRACE_INFO,
			"zmq-open: Error: Unable to connect to ZeroMQ-based persistence");
	}

	// reserve() is critical here, to reserve UUID range.
	_store->reserve();
	_backing->set_store(_store);
	AtomSpace *as = _as;
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-open");
	// as->registerBackingStore(_backing); // TODO: register
}

void ZMQPersistSCM::do_close(void)
{
	if (_store == NULL)
		throw RuntimeException(TRACE_INFO,
			 "zmq-close: Error: Database not open");

	AtomSpace *as = _as;
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-close");
	// as->unregisterBackingStore(_backing); // TODO: unregister

	_backing->set_store(NULL);
	delete _store;
	_store = NULL;
}

void ZMQPersistSCM::do_load(void)
{
	if (_store == NULL)
		throw RuntimeException(TRACE_INFO,
			"zmq-load: Error: Database not open");

	AtomSpace *as = _as;
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-load");
	// XXX TODO: this should probably be done in a separate thread.
	 _store->load(const_cast<AtomTable&>(as->get_atomtable()));
}

void ZMQPersistSCM::do_store(void)
{
	if (_store == NULL)
		throw RuntimeException(TRACE_INFO,
			"zmq-store: Error: Database not open");

	AtomSpace *as = _as;
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-store");
	// XXX TODO This should really be started in a new thread ...
	_store->store(const_cast<AtomTable&>(as->get_atomtable()));
}

void opencog_persist_zmq_init(void)
{
   static ZMQPersistSCM patty(NULL);
}

#endif // HAVE_GUILE
