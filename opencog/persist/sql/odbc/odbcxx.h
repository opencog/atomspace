/*
 * FUNCTION:
 * ODBC driver -- developed/tested with both iODBC http://www.iodbc.org
 * and with unixODBC
 *
 * HISTORY:
 * Copyright (c) 2002,2008 Linas Vepstas <linas@linas.org>
 * created by Linas Vepstas  March 2002
 * ported to C++ March 2008
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

#ifndef _OPENCOG_PERSISTENT_ODBC_DRIVER_H
#define _OPENCOG_PERSISTENT_ODBC_DRIVER_H

#include <stack>
#include <string>

#include <sql.h>
#include <sqlext.h>

#include "llapi.h"

/** \addtogroup grp_persist
 *  @{
 */

class ODBCRecordSet;

class ODBCConnection : public LLConnection
{
    friend class ODBCRecordSet;
    private:
        SQLHENV sql_henv;
        SQLHDBC sql_hdbc;

        ODBCRecordSet *get_record_set(void);

    public:
        ODBCConnection(const char * dbname,
                       const char * username,
                       const char * authentication);
        ~ODBCConnection();

        LLRecordSet *exec(const char *);
        void extract_error(const char *);
};

class ODBCRecordSet : public LLRecordSet
{
    friend class ODBCConnection;
    private:
        ODBCConnection *conn;
        SQLHSTMT sql_hstmt;

        void alloc_and_bind_cols(int ncols);
        ODBCRecordSet(ODBCConnection *);
        ~ODBCRecordSet();

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

#endif // _OPENCOG_PERSISTENT_ODBC_DRIVER_H
