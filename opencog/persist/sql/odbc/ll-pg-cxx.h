/*
 * FUNCTION:
 * Postgres driver -- low-level API for postgres
 *
 * HISTORY:
 * Copyright (c) 2017 Linas Vepstas
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

#ifndef _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H
#define _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H

#ifdef HAVE_PGSQL_STORAGE

#include <stack>
#include <string>

#include "llapi.h"

/** \addtogroup grp_persist
 *  @{
 */

class PGRecordSet;

class PGConnection : public LLConnection
{
    friend class PGRecordSet;
    private:
        PGRecordSet *get_record_set(void);

    public:
        PGConnection(const char * dbname,
                       const char * username,
                       const char * authentication);
        ~PGConnection();

        LLRecordSet *exec(const char *);
        void extract_error(const char *);
};

class PGRecordSet : public LLRecordSet
{
    friend class PGConnection;
    private:
        void alloc_and_bind_cols(int ncols);
        PGRecordSet(PGConnection *);
        ~PGRecordSet();

        void get_column_labels(void);

    public:
        // rewind the cursor to the start
        void rewind(void);

        int fetch_row(void); // return non-zero value if there's another row.

        // call this, instead of the destructor,
        // when done with this instance.
        void release(void);
};

/** @}*/

#endif /* HAVE_PGSQL_STORAGE */
#endif // _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H
