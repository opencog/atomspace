/*
 * opencog/persist/sql/SQLPersistSCM.cc
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

#include "SQLAtomStorage.h"
#include "SQLPersistSCM.h"

using namespace opencog;


// =================================================================

SQLPersistSCM::SQLPersistSCM(AtomSpace *as)
{
    _as = as;
    _backing = nullptr;

    static bool is_init = false;
    if (is_init) return;
    is_init = true;
    scm_with_guile(init_in_guile, this);
}

void* SQLPersistSCM::init_in_guile(void* self)
{
    scm_c_define_module("opencog persist-sql", init_in_module, self);
    scm_c_use_module("opencog persist-sql");
    return NULL;
}

void SQLPersistSCM::init_in_module(void* data)
{
   SQLPersistSCM* self = (SQLPersistSCM*) data;
   self->init();
}

void SQLPersistSCM::init(void)
{
    define_scheme_primitive("sql-open", &SQLPersistSCM::do_open, this, "persist-sql");
    define_scheme_primitive("sql-close", &SQLPersistSCM::do_close, this, "persist-sql");
    define_scheme_primitive("sql-load", &SQLPersistSCM::do_load, this, "persist-sql");
    define_scheme_primitive("sql-store", &SQLPersistSCM::do_store, this, "persist-sql");
    define_scheme_primitive("sql-stats", &SQLPersistSCM::do_stats, this, "persist-sql");
    define_scheme_primitive("sql-clear-cache", &SQLPersistSCM::do_clear_cache, this, "persist-sql");
    define_scheme_primitive("sql-clear-stats", &SQLPersistSCM::do_clear_stats, this, "persist-sql");
    define_scheme_primitive("sql-set-hilo-watermarks!", &SQLPersistSCM::do_set_hilo, this, "persist-sql");
    define_scheme_primitive("sql-set-stall-writers!", &SQLPersistSCM::do_set_stall, this, "persist-sql");
}

SQLPersistSCM::~SQLPersistSCM()
{
    if (_backing) delete _backing;
}

void SQLPersistSCM::do_open(const std::string& uri)
{
    // Unconditionally use the current atomspace, until the next close.
    AtomSpace *as = SchemeSmob::ss_get_env_as("sql-open");
    if (nullptr != as) _as = as;

    if (nullptr == _as)
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: No atomspace specified!");

    // Allow only one connection at a time.
    if (_as->isAttachedToBackingStore())
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: Atomspace already connected to a storage backend!");
    if (_backing)
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: Atomspace already connected to a storage backend!");

    // Use the postgres driver.
    SQLAtomStorage *store = new SQLAtomStorage(uri);
    if (!store)
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to open the database");

    if (!store->connected())
    {
        delete store;
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to connect to the database");
    }

    _backing = store;
    _backing->registerWith(_as);
}

void SQLPersistSCM::do_close(void)
{
    if (nullptr == _backing)
        throw RuntimeException(TRACE_INFO,
             "sql-close: Error: Database not open");

    // The destructor might run for a while before its done; it will
    // be emptying the pending store queues, which might take a while.
    // So unhook the atomspace first -- this will prevent new writes
    // from accidentally being queued. (It will also drain the queues)
    // Only then actually call the dtor.
    //
    // We should probably be doing this under a lock, to prevent
    // two racing threads that are both trying to close the
    // connection. But who would be crazy enough to want to do that?
    _backing->unregisterWith(_as);

    delete _backing;
    _backing = nullptr;
}

void SQLPersistSCM::do_load(void)
{
    if (nullptr == _backing)
        throw RuntimeException(TRACE_INFO,
            "sql-load: Error: Database not open");

    _backing->loadAtomSpace(_as);
}


void SQLPersistSCM::do_store(void)
{
    if (nullptr == _backing)
        throw RuntimeException(TRACE_INFO,
            "sql-store: Error: Database not open");

    _backing->storeAtomSpace(_as);
}

void SQLPersistSCM::do_stats(void)
{
    if (nullptr == _backing) {
        printf("sql-stats: Database not open\n");
        return;
    }

    if (NULL == _as)
        printf("sql-stats: AtomSpace not set\n");

    AtomSpace* as = SchemeSmob::ss_get_env_as("sql-stats");
    printf("sql-stats: Atomspace holds %lu atoms\n", as->get_size());

    _backing->print_stats();
}

void SQLPersistSCM::do_clear_cache(void)
{
    if (nullptr == _backing) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _backing->clear_cache();
}

void SQLPersistSCM::do_clear_stats(void)
{
    if (nullptr == _backing) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _backing->clear_stats();
}

void SQLPersistSCM::do_set_hilo(int hi, int lo)
{
    if (nullptr == _backing) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _backing->set_hilo_watermarks(hi, lo);
}

void SQLPersistSCM::do_set_stall(bool stall)
{
    if (nullptr == _backing) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _backing->set_stall_writers(stall);
}

void opencog_persist_sql_init(void)
{
    static SQLPersistSCM patty(NULL);
}
#endif // HAVE_GUILE
