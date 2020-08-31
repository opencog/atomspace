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
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/api/PersistSCM.h>
#include <opencog/guile/SchemePrimitive.h>

#include "SQLAtomStorage.h"
#include "SQLPersistSCM.h"

using namespace opencog;


// =================================================================

SQLPersistSCM::SQLPersistSCM(AtomSpace *as)
{
    _as = as;

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
    define_scheme_primitive("sql-create", &SQLPersistSCM::do_create, this, "persist-sql");
    define_scheme_primitive("sql-open", &SQLPersistSCM::do_open, this, "persist-sql");
    define_scheme_primitive("sql-close", &SQLPersistSCM::do_close, this, "persist-sql");
    define_scheme_primitive("sql-stats", &SQLPersistSCM::do_stats, this, "persist-sql");
    define_scheme_primitive("sql-clear-cache", &SQLPersistSCM::do_clear_cache, this, "persist-sql");
    define_scheme_primitive("sql-clear-stats", &SQLPersistSCM::do_clear_stats, this, "persist-sql");
    define_scheme_primitive("sql-set-hilo-watermarks!", &SQLPersistSCM::do_set_hilo, this, "persist-sql");
    define_scheme_primitive("sql-set-stall-writers!", &SQLPersistSCM::do_set_stall, this, "persist-sql");
}

SQLPersistSCM::~SQLPersistSCM()
{
	_storage = nullptr;
}

void SQLPersistSCM::do_create(const std::string& uri)
{
    SQLAtomStorage* store = new SQLAtomStorage(uri);
    store->create_database();
    delete store;
}

void SQLPersistSCM::do_open(const std::string& uri)
{
    if (_storage)
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: Already connected to a database!");

    // Unconditionally use the current atomspace, until the next close.
    AtomSpace *as = SchemeSmob::ss_get_env_as("sql-open");
    if (nullptr != as) _as = as;

    if (nullptr == _as)
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: Can't find the atomspace!");

    // Adding the postgres node to the atomspace will fail on read-only
    // atomspaces.
    if (_as->get_read_only())
        throw RuntimeException(TRACE_INFO,
             "sql-open: Error: AtomSpace is read-only!");

    Handle hsn = _as->add_node(POSTGRES_STORAGE_NODE, std::string(uri));
    _storage = PostgresStorageNodeCast(hsn);
    _storage->open();
    if (!_storage->connected())
    {
        _as->exract_atom(hsn);
        _storage = nullptr;
        throw RuntimeException(TRACE_INFO,
            "sql-open: Error: Unable to connect to the database");
    }

    PersistSCM::set_connection(_storage);
}

void SQLPersistSCM::do_close(void)
{
    if (nullptr == _storage)
        throw RuntimeException(TRACE_INFO,
             "sql-close: Error: Database not open");

    // The destructor might run for a while before its done; it will
    // be emptying the pending store queues, which might take a while.
    // So unhook the atomspace first -- this will prevent new writes
    // from accidentally being queued. (It will also drain the queues)
    // Only then actually call the dtor.
    _as->extract_atom(HandleCast(_storage));
    _storage = nullptr;
}

void SQLPersistSCM::do_stats(void)
{
    if (nullptr == _storage) {
        printf("sql-stats: Database not open\n");
        return;
    }

    printf("sql-stats: Atomspace holds %lu atoms\n", _as->get_size());
    _storage->print_stats();
}

void SQLPersistSCM::do_clear_cache(void)
{
    if (nullptr == _storage) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _storage->clear_cache();
}

void SQLPersistSCM::do_clear_stats(void)
{
    if (nullptr == _storage) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _storage->clear_stats();
}

void SQLPersistSCM::do_set_hilo(int hi, int lo)
{
    if (nullptr == _storage) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _storage->set_hilo_watermarks(hi, lo);
}

void SQLPersistSCM::do_set_stall(bool stall)
{
    if (nullptr == _storage) {
        printf("sql-stats: Database not open\n");
        return;
    }

    _storage->set_stall_writers(stall);
}

void opencog_persist_sql_init(void)
{
    static SQLPersistSCM patty(NULL);
}
#endif // HAVE_GUILE
