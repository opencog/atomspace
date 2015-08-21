/*
 * opencog/persist/zmq/atomspace/ZMQPersistSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Hendy Irawan <ceefour666@gmail.com>
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>
#include <opencog/guile/SchemePrimitive.h>

#include "ZMQPersistSCM.h"
#include "ZMQStorage.h"

using namespace opencog;

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class ZMQBackingStore : public BackingStore
{
	private:
		ZMQStorage *_store;
	public:
		ZMQBackingStore();
		void set_store(ZMQStorage *);

		virtual NodePtr getNode(Type, const char *) const;
		virtual LinkPtr getLink(Type, const HandleSeq&) const;
		virtual AtomPtr getAtom(Handle) const;
		virtual HandleSeq getIncomingSet(Handle) const;
		virtual void storeAtom(Handle);
		virtual void loadType(AtomTable&, Type);
		virtual void barrier();
};

/** @}*/
}  // namespace

ZMQBackingStore::ZMQBackingStore()
{
	_store = NULL;
}

void ZMQBackingStore::set_store(ZMQStorage *as)
{
	_store = as;
}

NodePtr ZMQBackingStore::getNode(Type t, const char *name) const
{
	return _store->getNode(t, name);
}

LinkPtr ZMQBackingStore::getLink(Type t, const std::vector<Handle>& oset) const
{
	return _store->getLink(t, oset);
}

AtomPtr ZMQBackingStore::getAtom(Handle h) const
{
	return _store->getAtom(h);
}

HandleSeq ZMQBackingStore::getIncomingSet(Handle h) const
{
	return _store->getIncomingSet(h);
}

void ZMQBackingStore::storeAtom(Handle h)
{
	_store->storeAtom(h);
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

#ifdef HAVE_GUILE
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	scm_with_guile(init_in_guile, this);
#endif
}

void* ZMQPersistSCM::init_in_guile(void* self)
{
#ifdef HAVE_GUILE
	scm_c_define_module("opencog persist-zmq", init_in_module, self);
	scm_c_use_module("opencog persist-zmq");
#endif
	return NULL;
}

void ZMQPersistSCM::init_in_module(void* data)
{
   ZMQPersistSCM* self = (ZMQPersistSCM*) data;
   self->init();
}

void ZMQPersistSCM::init(void)
{
#ifdef HAVE_GUILE
	define_scheme_primitive("zmq-open", &ZMQPersistSCM::do_open, this, "persist-zmq");
	define_scheme_primitive("zmq-close", &ZMQPersistSCM::do_close, this, "persist-zmq");
	define_scheme_primitive("zmq-load", &ZMQPersistSCM::do_load, this, "persist-zmq");
	define_scheme_primitive("zmq-store", &ZMQPersistSCM::do_store, this, "persist-zmq");
#endif
}

ZMQPersistSCM::~ZMQPersistSCM()
{
	delete _backing;
}

void ZMQPersistSCM::do_open(const std::string& dbname,
                         const std::string& username,
                         const std::string& auth)
{
	_store = new ZMQStorage(/*dbname, username, auth*/);
	if (!_store)
		throw RuntimeException(TRACE_INFO,
			"zmq-open: Error: Unable to open the database");

	if (!_store->connected())
	{
		delete _store;
		_store = NULL;
		throw RuntimeException(TRACE_INFO,
			"zmq-open: Error: Unable to connect to the database");
	}

	// reserve() is critical here, to reserve UUID range.
	_store->reserve();
	_backing->set_store(_store);
	AtomSpace *as = _as;
#ifdef HAVE_GUILE
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-open");
#endif
	// as->registerBackingStore(_backing); // TODO: register
}

void ZMQPersistSCM::do_close(void)
{
	if (_store == NULL)
		throw RuntimeException(TRACE_INFO,
			 "zmq-close: Error: Database not open");

	AtomSpace *as = _as;
#ifdef HAVE_GUILE
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-close");
#endif
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
#ifdef HAVE_GUILE
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-load");
#endif
	// XXX TODO: this should probably be done in a separate thread.
	 _store->load(const_cast<AtomTable&>(as->get_atomtable()));
}

void ZMQPersistSCM::do_store(void)
{
	if (_store == NULL)
		throw RuntimeException(TRACE_INFO,
			"zmq-store: Error: Database not open");

	AtomSpace *as = _as;
#ifdef HAVE_GUILE
	if (NULL == as)
		as = SchemeSmob::ss_get_env_as("zmq-store");
#endif
	// XXX TODO This should really be started in a new thread ...
	_store->store(const_cast<AtomTable&>(as->get_atomtable()));
}

void opencog_persist_zmq_init(void)
{
   static ZMQPersistSCM patty(NULL);
}
