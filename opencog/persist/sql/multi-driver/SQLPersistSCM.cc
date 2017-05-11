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
#include <opencog/persist/sql/SQLBackingStore.h>

#include "SQLAtomStorage.h"
#include "SQLPersistSCM.h"

using namespace opencog;


// =================================================================

SQLPersistSCM::SQLPersistSCM(AtomSpace *as)
{
    _as = as;
    _store = NULL;
    _backing = new SQLBackingStore();

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
}

SQLPersistSCM::~SQLPersistSCM()
{
    delete _backing;
}

void SQLPersistSCM::do_open(const std::string& uri)
{
    // Unconditionally use the current atomspace, until the next close.
    AtomSpace *as = SchemeSmob::ss_get_env_as("sql-open");
    if (nullptr != as) _as = as;

    if (nullptr == _as)
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: No atomspace specified!");

    // Use the postgres driver.
    _store = new SQLAtomStorage(uri);
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
    _backing->registerWith(_as);
}

void SQLPersistSCM::do_close(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
             "sql-close: Error: Database not open");

    _backing->unregisterWith(_as);
    _backing->set_store(NULL);
    delete _store;
    _store = NULL;
}

void SQLPersistSCM::do_load(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
            "sql-load: Error: Database not open");

    _store->loadAtomSpace(_as);
}


void SQLPersistSCM::do_store(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
            "sql-store: Error: Database not open");

    _store->storeAtomSpace(_as);
}

void SQLPersistSCM::do_stats(void)
{
    if (_store == NULL) {
        printf("sql-stats: Database not open\n");
        return;
    }

    if (NULL == _as)
        printf("sql-stats: AtomSpace not set\n");

    AtomSpace* as = SchemeSmob::ss_get_env_as("sql-stats");
    printf("sql-stats: Atomspace holds %lu atoms\n", as->get_size());

    _store->print_stats();
}

void SQLPersistSCM::do_clear_cache(void)
{
    if (_store == NULL) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _store->clear_cache();
}

void SQLPersistSCM::do_clear_stats(void)
{
    if (_store == NULL) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _store->clear_stats();
}

void opencog_persist_sql_init(void)
{
    static SQLPersistSCM patty(NULL);
}
#endif // HAVE_GUILE
