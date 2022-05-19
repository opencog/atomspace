/*
 * opencog/persist/sql/SQLPersistSCM.h
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

#ifndef _OPENCOG_SQL_PERSIST_SCM_H
#define _OPENCOG_SQL_PERSIST_SCM_H

#ifdef HAVE_GUILE

#include <string>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/persist/sql/multi-driver/SQLAtomStorage.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class SQLPersistSCM
{
private:
    static void* init_in_guile(void*);
    static void init_in_module(void*);
    void init(void);

    PostgresStorageNodePtr _storage;
    AtomSpace *_as;

public:
    SQLPersistSCM(AtomSpace*);
    ~SQLPersistSCM();

    void do_create(const std::string&);
    void do_open(const std::string&);
    void do_close(void);

    void do_stats(void);
    void do_clear_cache(void);
    void do_clear_stats(void);

    void do_set_hilo(int, int);
    void do_set_stall(bool);

}; // class

/** @}*/
}  // namespace

extern "C" {
void opencog_persist_sql_init(void);
};
#endif // HAVE_GUILE

#endif // _OPENCOG_SQL_PERSIST_SCM_H
