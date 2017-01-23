/*
 * FUNCTION:
 * Low-Level SQL driver -- generic, simple low-level SQL api.
 *
 * Threading:
 * ----------
 * This class is thread-enabled but not thread-safe. Two threads should
 * not try to use one instance of this class at the same time. Each
 * thread should construct it's own instance of this class. This class
 * uses no globals.
 *
 * HISTORY:
 * Copyright (c) 2002,2008 Linas Vepstas <linas@linas.org>
 * created by Linas Vepstas  March 2002
 * ported to C++ March 2008
 * made generic January 2017
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

#ifdef HAVE_SQL_STORAGE

#include <stack>
#include <string>

#include <stdio.h>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/platform.h>

#include "llapi.h"

#define PERR(...) \
    throw opencog::RuntimeException(TRACE_INFO, __VA_ARGS__);

/* =========================================================== */

LLConnection::LLConnection(const char * _dbname,
                           const char * _username,
                           const char * _authentication)
{
    is_connected = false;

    if (NULL == _dbname)
    {
        PERR("No DB specified");
        return;
    }

    dbname = _dbname;
    username = _username;
}

/* =========================================================== */

LLConnection::~LLConnection()
{
    while (!free_pool.empty())
    {
        LLRecordSet *rs = free_pool.top();
        delete rs;
        free_pool.pop();
    }
}

bool LLConnection::connected (void) const
{
    return is_connected;
}

/* =========================================================== */

// This kind of static alloc is kind of crazy and bizarre, it was
// driven by the need for odbc too be given mem locations in which
// to scribble it's results. Ugly, but it works, with some kind of
// reasonable efficiency, for now.
//
// XXX we could improve the postgres efficiency by just mallocing
// these for odbc, and storing pointers for postgres. XXX TODO.

#define DEFAULT_COLUMN_NAME_SIZE 121
#define DEFAULT_VARCHAR_SIZE 4040

void
LLRecordSet::alloc_and_bind_cols(int new_ncols)
{
    if (new_ncols <= arrsize) return;

    int i;
    if (column_labels)
    {
        for (i=0; i<arrsize; i++)
        {
            if (column_labels[i])
            {
                delete[] column_labels[i];
            }
        }
        delete[] column_labels;
    }
    if (column_datatype) delete[] column_datatype;

    if (values)
    {
        for (i=0; i<arrsize; i++)
        {
            if (values[i])
            {
                delete[] values[i];
            }
        }
        delete[] values;
    }
    if (vsizes) delete[] vsizes;

    column_labels = new char*[new_ncols];
    column_datatype = new int[new_ncols];
    values = new char*[new_ncols];
    vsizes = new int[new_ncols];

    /* intialize */
    for (i = 0; i<new_ncols; i++)
    {
        column_labels[i] = new char[DEFAULT_COLUMN_NAME_SIZE];
        column_labels[i][0] = 0;
        column_datatype[i] = 0;
        values[i] = new char[DEFAULT_VARCHAR_SIZE];
        vsizes[i] = DEFAULT_VARCHAR_SIZE;
        values[i][0] = 0;
    }

    arrsize = new_ncols;
}

/* =========================================================== */
/* pseudo-private routine */


LLRecordSet::LLRecordSet(LLConnection *_conn)
{
    conn = _conn;
    ncols = -1;
    arrsize = 0;
    column_labels = NULL;
    column_datatype = NULL;
    values = NULL;
    vsizes = NULL;
}

/* =========================================================== */

void
LLRecordSet::release(void)
{
    conn->free_pool.push(this);
}

/* =========================================================== */

LLRecordSet::~LLRecordSet()
{
    conn = NULL;

    for (int i=0; i<arrsize; i++)
    {
        delete[] column_labels[i];
        delete[] values[i];
    }
    delete[] column_labels;
    column_labels = NULL;

    delete[] column_datatype;
    column_datatype = NULL;

    delete[] values;
    values = NULL;

    delete[] vsizes;
    vsizes = NULL;
}

/* =========================================================== */

int
LLRecordSet::get_col_by_name (const char * fieldname)
{
    /* lookup the column number based on the column name */
    int i;
    for (i=0; i<ncols; i++)
    {
        if (!strcasecmp (fieldname, column_labels[i])) return i;
    }

    /* oops. Try removing the table name if possible */
    const char * fp = strrchr (fieldname, '.');
    if (!fp) return -1;
    fp ++;

    for (i=0; i<ncols; i++)
    {
        if (!strcasecmp (fp, column_labels[i])) return i;
    }

    return -1;
}

const char *
LLRecordSet::get_value(const char * fieldname)
{
    /* If number of columns is negative, then we haven't
     * gotten any results back yet.  Start by getting the
     * column labels.
     */
    if (0 > ncols)
    {
        get_column_labels();
    }

    int column = get_col_by_name (fieldname);
    if (0 > column) return NULL;

    // LEAVE ("(rs=%p, fieldname=%s) {val=\'%s\'}", rs, fieldname,  rs->values[column]);
    return values[column];
}

int
LLRecordSet::get_column_count()
{
    /* If number of columns is negative, then we haven't
     * gotten any results back yet.  Start by getting the
     * column labels which will set the column count.
     */
    if (0 > ncols)
        get_column_labels();

    return ncols;
}

const char *
LLRecordSet::get_column_value(int column)
{
    /* Make sure we have columns and this column is in range. */
    if (column >= get_column_count()) return NULL;

    return values[column];
}

/* =========================================================== */

#ifdef UNIT_TEST_EXAMPLE

class Ola
{
    public:
        LLRecordSet *rs;
        bool column_cb(const char *colname, const char * colvalue)
        {
            printf ("%s = %s\n", colname, colvalue);
            return false;
        }
        bool row_cb(void)
        {
            printf ("---- New row found ----\n");
            rs->foreach_column(&Ola::column_cb, this);
            return false;
        }
};

int main ()
{
    LLConnection *conn;
    conn = new LLConnection("opencog", "linas", NULL);

    LLRecordSet *rs;

// #define MAKE_SOME_DATA
#ifdef MAKE_SOME_DATA
    rs = conn->exec(
        "INSERT INTO Atoms VALUES (3,1,0.5, 0.5, 'umm');");
#endif

    // One way of doing things
    rs = conn->exec("SELECT * FROM Atoms;");
    while (rs->fetch_row())
    {
        const char * n = rs->get_value("name");
        printf ("found column with value %s\n", n);
    }
    rs->release();

    // Another way of doing things
    Ola *ola = new Ola();
    rs = conn->exec("SELECT * FROM Atoms;");
    while (rs->fetch_row())
    {
        printf("--- method 2 has row:\n");
        rs->foreach_column(&Ola::column_cb, ola);
    }
    rs->release();

    // A third way of doing things
    ola->rs = rs;
    rs = conn->exec("SELECT * FROM Atoms;");
    rs->foreach_row(&Ola::row_cb, ola);
    rs->release();

    delete conn;

    return 0;
}

#endif /* UNIT_TEST_EXAMPLE */

#endif /* HAVE_SQL_STORAGE */
/* ============================= END OF FILE ================= */

