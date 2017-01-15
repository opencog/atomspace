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

#include "ODBCAtomStorage.h"
#include "SQLPersistSCM.h"

using namespace opencog;


// =================================================================

SQLPersistSCM::SQLPersistSCM(AtomSpace *as)
{
    _as = as;
    _store = NULL;
    _backing = new SQLBackingStore();

    // XXX FIXME Huge hack alert.
    // As of 2013, no one uses this thing, except for NLP processing.
    // Since I'm too lazy to find an elegant solution right now, I'm
    // just going to hack this in.  Fix this someday.
    //
    // Anyway, what the below does is to ignore these certain types,
    // when they are to be fetched from the backing store.  This can
    // speed up document processing, since we know that word instances
    // and documents and sentences will not be stored in the database.
    // Thus, we don't even try to fetch these.

#define NLP_HACK 0
#if NLP_HACK
    _backing->_ignored_types.insert(VARIABLE_NODE);
    _backing->_ignored_types.insert(TYPE_NODE);
    _backing->_ignored_types.insert(TYPED_VARIABLE_LINK);
    _backing->_ignored_types.insert(BIND_LINK);

    _backing->_ignored_types.insert(DOCUMENT_NODE);
    _backing->_ignored_types.insert(SENTENCE_NODE);
    _backing->_ignored_types.insert(PARSE_NODE);
    _backing->_ignored_types.insert(PARSE_LINK);
    _backing->_ignored_types.insert(WORD_INSTANCE_NODE);
    _backing->_ignored_types.insert(WORD_INSTANCE_LINK);
#endif // NLP_HACK

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
#ifdef STORAGE_DEBUG
    define_scheme_primitive("sql-stats", &SQLPersistSCM::do_stats, this, "persist-sql");
#endif
}

SQLPersistSCM::~SQLPersistSCM()
{
    delete _backing;
}

void SQLPersistSCM::do_open(const std::string& dbname,
                         const std::string& username,
                         const std::string& auth)
{
    _store = new ODBCAtomStorage(dbname, username, auth);
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
    _backing->registerWith(as);
}

void SQLPersistSCM::do_close(void)
{
    if (_store == NULL)
        throw RuntimeException(TRACE_INFO,
             "sql-close: Error: Database not open");

    AtomSpace *as = _as;
    if (NULL == as)
        as = SchemeSmob::ss_get_env_as("sql-close");
    _backing->unregisterWith(as);
    _backing->set_store(NULL);
    delete _store;
    _store = NULL;
}

void SQLPersistSCM::do_load(void)
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


void SQLPersistSCM::do_store(void)
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

#ifdef STORAGE_DEBUG
void SQLPersistSCM::do_stats(void)
{
    if (_store == NULL)
        printf("sql-stats: Database not open\n");

    if (NULL == _as)
        printf("sql-stats: AtomSpace not set\n");

    size_t extra = 0;
    size_t noh = 0;
    size_t remap = 0;
    AtomSpace* as = SchemeSmob::ss_get_env_as("sql-stats");

    printf("sql-stats: Atomspace holds %lu atoms\n", as->get_size());
    printf("sql-stats: tlbuf holds %lu atoms\n", _store->_tlbuf.size());

    UUID mad = _store->_tlbuf.getMaxUUID();
    for (UUID uuid = 1; uuid < mad; uuid++)
    {
        Handle h = _store->_tlbuf.getAtom(uuid);
        if (nullptr == h) { noh++; continue; }

        Handle hr = as->get_atom(h);
        if (nullptr == hr) { extra++; continue; }

        if (hr != h) { remap++; }
    }

    double frac = 100.0 * extra / ((double) _store->_tlbuf.size());
    printf("sql-stats: tlbuf holds %lu atoms not in atomspace (%f pct)\n",
           extra, frac);

    frac = 100.0 * remap / ((double) _store->_tlbuf.size());
    printf("sql-stats: tlbuf holds %lu unremapped atoms (%f pct)\n",
           remap, frac);

    frac = 100.0 * noh / ((double) mad);
    printf("sql-stats: %lu of %lu uuids unused (%f pct)\n",
           noh, mad, frac);
}
#endif

void opencog_persist_sql_init(void)
{
    static SQLPersistSCM patty(NULL);
}
#endif // HAVE_GUILE
