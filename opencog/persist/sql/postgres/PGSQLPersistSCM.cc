/*
 * opencog/persist/sql/PGSQLPersistSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Linas Vepstas <linasvepstas@gmail.com>
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
#include <opencog/persist/sql/SQLBackingStore.h>

#include "PGAtomStorage.h"
#include "PGSQLPersistSCM.h"

using namespace opencog;

#ifndef DEBUG_SQL_STATEMENTS
#define DEBUG_SQL_STATEMENTS 0
#endif

PGSQLPersistSCM::PGSQLPersistSCM(AtomSpace *as)
{
    _as = as;
    _store = NULL;
    _backing = new SQLBackingStore();

    static bool is_init = false;
    if (is_init) return;
    is_init = true;
    scm_with_guile(init_in_guile, this);
}

void* PGSQLPersistSCM::init_in_guile(void* self)
{
    scm_c_define_module("opencog persist-pgsql", init_in_module, self);
    scm_c_use_module("opencog persist-pgsql");
    return NULL;
}

void PGSQLPersistSCM::init_in_module(void* data)
{
   PGSQLPersistSCM* self = (PGSQLPersistSCM*) data;
   self->init();
}

void PGSQLPersistSCM::init(void)
{
    define_scheme_primitive("pgsql-open", &PGSQLPersistSCM::do_open, this, "persist-pgsql");
    define_scheme_primitive("pgsql-close", &PGSQLPersistSCM::do_close, this, "persist-pgsql");
    define_scheme_primitive("pgsql-load", &PGSQLPersistSCM::do_load, this, "persist-pgsql");
    define_scheme_primitive("pgsql-store", &PGSQLPersistSCM::do_store, this, "persist-pgsql");
}

PGSQLPersistSCM::~PGSQLPersistSCM()
{
    delete _backing;
}

void PGSQLPersistSCM::do_open(const std::string& dbname,
                         const std::string& username,
                         const std::string& auth)
{
    _store = new PGAtomStorage(dbname, username, auth);
    if (!_store)
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to open the database");

    if (!_store->connected())
    {
        delete _store;
        _store = NULL;
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to connect to the database");
    }

    _backing->set_store(_store);
    AtomSpace *as = _as;
    if (NULL == as)
        as = SchemeSmob::ss_get_env_as("sql-open");

#if DEBUG_SQL_STATEMENTS
    _store->setVerbose();
    _store->setPrintStatements();
    _store->setDontStoreEdges();
#endif
    _backing->registerWith(as);
    TLB::set_resolver(&as->get_atomtable());
}

void PGSQLPersistSCM::do_close(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
             "sql-close: Error: Database not open");

    AtomSpace *as = _as;
    if (NULL == as)
        as = SchemeSmob::ss_get_env_as("sql-close");
    _backing->unregisterWith(as);
    TLB::clear_resolver(&as->get_atomtable());
    _backing->set_store(NULL);
    delete _store;
    _store = NULL;
}

void PGSQLPersistSCM::do_load(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
            "sql-load: Error: Database not open");

    AtomSpace *as = _as;
    if (NULL == as)
        as = SchemeSmob::ss_get_env_as("sql-load");
    // XXX TODO: this should probably be done in a separate thread.
    _store->loadAtomSpace(as);
}

void PGSQLPersistSCM::do_store(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
            "sql-store: Error: Database not open");

    AtomSpace *as = _as;
    if (NULL == as)
        as = SchemeSmob::ss_get_env_as("sql-store");
    // XXX TODO This should really be started in a new thread ...
    _store->storeAtomSpace(as);
}

void PGSQLPersistSCM::enable_testing_mode()
{
    _store->enable_testing_mode();
}

void PGSQLPersistSCM::disable_testing_mode()
{
    _store->disable_testing_mode();
}

void opencog_persist_pgsql_init(void)
{
   static PGSQLPersistSCM patty(NULL);
}

#endif // HAVE_GUILE
