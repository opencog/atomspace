/*
 * FUNCTION:
 * ODBC driver -- developed/tested with unixodbc http://www.unixodbc.org
 *
 * ODBC is basically brain-damaged, and so is this driver.
 *
 * Do not use this driver, unless you have some backwards-compat issues
 * that you have to work around.
 *
 * ODBC has several problems:
 * 1) It forces you to guess how many columns there are in your reply,
 *    which we don't know a-priori.
 * 2) VARCHAR is difficult (impossible ??) to support correctly!
 * 3) The use of the question mark as a special character, even when
 *    you have nothing bound, nothing at all, giving this message:
 *    (34) The # of binded parameters < the # of parameter markers;
 *    Wow! Aside from the bad English, this is a stunningly bad idea!
 * 4) It's slowww. Terrible performance. The native bindings are 3x
 *    faster!
 * 5) It's verbose. Notice how the native postgres driver can
 *    accomplish the same thing in less than half the number of lines
 *    of code!
 *
 * Blame it on SQLBindCol(), which is a terrible idea.  @#$%^ Microsoft.
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

#ifdef HAVE_ODBC_STORAGE

#include <stack>
#include <string>

#include <sql.h>
#include <sqlext.h>
#include <stdio.h>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/platform.h>

#include "odbcxx.h"

#define PERR(...) \
    throw opencog::RuntimeException(TRACE_INFO, __VA_ARGS__);

/* =========================================================== */

#define PRINT_SQLERR(HTYPE,HAN)                                 \
{                                                               \
    char sql_stat[10];                                          \
    SQLSMALLINT msglen;                                         \
    SQLINTEGER err;                                             \
    char msg[200];                                              \
                                                                \
    SQLGetDiagRec(HTYPE, HAN, 1, (SQLCHAR *) sql_stat,          \
                  &err, (SQLCHAR*) msg, sizeof(msg), &msglen);  \
    opencog::logger().warn("ODBC Driver: (%ld) %s\n", (long int) err, msg);  \
}

/* =========================================================== */

ODBCConnection::ODBCConnection(const char * uri)
{
    SQLRETURN rc;

    is_connected = false;
    need_qmark_escape = true; // worst-case assumption

    sql_hdbc = NULL;
    sql_henv = NULL;

    // We expect odbc://user:password/database
    if (0 != strncmp("odbc://", uri, 7))
    {
        PERR("Invalid URI specified: %s", uri);
        return;
    }
    uri += 7;   // skip past odbc://
    char* curi = strdup(uri);
    char* username = curi;
    char* authentication;
    char* p = strchr(curi, ':');
    if (p)
    {
        *p = 0;
        authentication = ++p;
    }
    else
    {
        // Point at the null byte at the end of the string.
        authentication = curi+strlen(curi);
        p = curi;
    }
    p = strchr(p, '/');
    if (!p)
    {
        free(curi);
        PERR("Invalid URI specified: %s", uri);
    }
    *p = 0;
    char *dbname = ++p;

    /* Allocate environment handle */
    rc = SQLAllocEnv(&sql_henv);
    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        free(curi);
        PERR("Can't SQLAllocEnv, rc=%d", rc);
        return;
    }

    /* Set the ODBC version */
    rc = SQLSetEnvAttr(sql_henv, SQL_ATTR_ODBC_VERSION,
                       (void*)SQL_OV_ODBC3, 0);
    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        free(curi);
        PRINT_SQLERR (SQL_HANDLE_ENV, sql_henv);
        SQLFreeHandle(SQL_HANDLE_ENV, sql_henv);
        sql_henv = NULL;
        PERR("Can't SQLSetEnv, rc=%d", rc);
        return;
    }

    /* Allocate the connection handle */
    rc = SQLAllocConnect(sql_henv, &sql_hdbc);
    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        free(curi);
        PRINT_SQLERR (SQL_HANDLE_ENV, sql_henv);
        SQLFreeHandle(SQL_HANDLE_ENV, sql_henv);
        sql_henv = NULL;
        PERR ("Can't SQLAllocConnect handle rc=%d", rc);
        return;
    }

    /* set the timeout to 5 seconds ?? hack alert fixme */
    // SQLSetConnectAttr(sql_hdbc, SQL_LOGIN_TIMEOUT, (SQLPOINTER *)5, 0);

    rc = SQLConnect(sql_hdbc,
                    (SQLCHAR*) dbname, SQL_NTS,
                    (SQLCHAR*) username, SQL_NTS,
                    (SQLCHAR*) authentication, SQL_NTS);

    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        free(curi);
        PRINT_SQLERR (SQL_HANDLE_DBC, sql_hdbc);
        SQLFreeHandle(SQL_HANDLE_DBC, sql_hdbc);
        SQLFreeHandle(SQL_HANDLE_ENV, sql_henv);
        sql_henv = NULL;
        sql_hdbc = NULL;
        PERR ("Can't perform SQLConnect rc=%d", rc);
        return;
    }

    is_connected = true;
    free(curi);

#define DRBUFSZ 120
    char buf[DRBUFSZ];
    rc = SQLGetInfo(sql_hdbc, SQL_DRIVER_ODBC_VER, buf, DRBUFSZ, NULL);
    if ((SQL_SUCCESS == rc) or (SQL_SUCCESS_WITH_INFO == rc))
    {
        // I'm not sure when question-mark escaping got fixed,
        // and I'm not sure if it got fixed in the postgres driver
        // or in the UnixODBC shims.  I do know that it works for
        // unixODBC version "03.51" and fails for "03.00". So make
        // the worst-case assumption that it fails for any version
        // except "03.51".
        need_qmark_escape = true;
        if (0 == strcmp(buf, "03.51")) need_qmark_escape = false;

        if (need_qmark_escape)
            opencog::logger().warn(
               "ODBC Driver: Version %s needs question-mark escaping\n"
               "\tThis WILL garble atom names with question-marks in them!\n"
               "\tIt will replace question-marks by &#63;\n",
               buf);
    }
}

/* =========================================================== */

ODBCConnection::~ODBCConnection()
{
    if (sql_hdbc)
    {
        SQLDisconnect(sql_hdbc);
        SQLFreeHandle(SQL_HANDLE_DBC, sql_hdbc);
        sql_hdbc = NULL;
    }

    if (sql_henv)
    {
        SQLFreeHandle(SQL_HANDLE_ENV, sql_henv);
        sql_henv = NULL;
    }
}

/* =========================================================== */
#define DEFAULT_NUM_COLS 50

ODBCRecordSet * ODBCConnection::get_record_set(void)
{
    ODBCRecordSet *rs;
    if (!free_pool.empty())
    {
        LLRecordSet* llrs = free_pool.top();
        rs = dynamic_cast<ODBCRecordSet*>(llrs);
        free_pool.pop();
        rs->ncols = -1;
    }
    else
    {
        rs = new ODBCRecordSet(this);
    }

    rs->alloc_and_bind_cols(DEFAULT_NUM_COLS);

    return rs;
}

/* =========================================================== */

void ODBCConnection::extract_error(const char *msg)
{
    SQLRETURN ret;
    SQLINTEGER i = 0;
    SQLINTEGER native;
    SQLCHAR state[7];
    SQLCHAR text[256];
    SQLSMALLINT len;
    do
    {
        ret = SQLGetDiagRec(SQL_HANDLE_ENV, sql_henv, ++i,
               state, &native, text, sizeof(text), &len);
        if (SQL_SUCCEEDED(ret))
            opencog::logger().warn("ODBC Driver: %s : %d : %d : %s\n",
                                    state, i, native, text);
    } while (ret == SQL_SUCCESS);
}

/* =========================================================== */

LLRecordSet *
ODBCConnection::exec(const char * buff, bool trial)
{
    if (!is_connected) return NULL;
    ODBCRecordSet* rs = get_record_set();
    SQLRETURN rc;

    /* ODBC treats the appearance of the question-mark character ? as
     * something special -- for binding columns, even if you are NOT
     * actually binding columns. If it shows up in the query, it will
     * result in the error message
     * (34) The # of binded parameters < the # of parameter markers;
     * Therefore we scan for and html-escape all question marks.
     *
     * XXX FIXME This changes the name of the atom in the database,
     * itself.  This is fine, if we only ever store or update the
     * truth value on the atom, but breaks it if we try to read the
     * atom.  We could try to convert &#63; back into a question-mark,
     * when we read the atom name, but then, in that case, how can we
     * know that the atom name didn't originally have &#63; in it?
     *
     * Basically, its a really crappy situation, and the only real
     * solution is to simply not use the old/broken drivers, or not
     * use ODBC at all. Since this is not always possible, we have
     * to, uhh, pretend we can make lemonade out of the lemons.
     */
    if (need_qmark_escape and strchr(buff, '?'))
    {
        int cnt = 0;
        const char* p = buff;
        while ((p = strchr(p, '?'))) { ++p; cnt++; }
        char *escape = (char *) malloc(strlen(buff) + 4*cnt + 1);
        char *d = escape;
        const char *b = buff;
        while ((p = strchr(b, '?'))) {
            size_t len = p-b;
            memcpy(d, b, len);
            d += len;
            memcpy(d, "&#63;", 5);
            d += 5;
            ++p;
            b = p;
        }
        strcpy(d, b);
        rc = SQLExecDirect(rs->sql_hstmt, (SQLCHAR *)escape, SQL_NTS);
        free(escape);
    }
    else
    {
        rc = SQLExecDirect(rs->sql_hstmt, (SQLCHAR *)buff, SQL_NTS);
    }

    /* If query returned no data, its not an error:
     * its simply "no data", that's all.
     */
    if (SQL_NO_DATA == rc)
    {
        rs->release();
        return NULL;
    }

    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        PRINT_SQLERR (SQL_HANDLE_STMT, rs->sql_hstmt);
        rs->release();
        opencog::logger().warn("ODCB Driver: Query was: %s\n", buff);
        extract_error("exec");
        PERR ("Can't perform query rc=%d ", rc);
        return NULL;
    }

    /* Use numbr of columns to indicate that the query hasn't
     * given results yet. */
    rs->ncols = -1;
    return rs;
}

/* =========================================================== */

#define DEFAULT_COLUMN_NAME_SIZE 121
#define DEFAULT_VARCHAR_SIZE 4040

void
ODBCRecordSet::alloc_and_bind_cols(int new_ncols)
{
    int i;

    // This static alloc of column names and values is crazy and
    // bizarre, but it is driven by the fact that ODBC must be given
    // mem locations in which to scribble it's results. Which is
    // a nutty design, given that we don't know sizes of the columns
    // in advance. Only Microsoft could invent something this nasty.

    if (new_ncols > arrsize)
    {
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

        /* initialize */
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

    ODBCConnection* oconn = dynamic_cast<ODBCConnection*>(conn);
    SQLRETURN rc = SQLAllocStmt (oconn->sql_hdbc, &sql_hstmt);
    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        PRINT_SQLERR (SQL_HANDLE_STMT, sql_hstmt);
        PERR("Can't allocate statement handle, rc=%d", rc);
        /* oops memory leak */
        return;
    }

    // IMPORTANT! MUST NOT BE ON STACK!! Else stack corruption will result.
    // The ODBC driver really wants to write a return value here!
    static SQLLEN bogus;

    /* Initialize the newly realloc'ed entries */
    for (i=0; i<new_ncols; i++)
    {
        rc = SQLBindCol(sql_hstmt, i+1, SQL_C_CHAR,
            values[i], vsizes[i], &bogus);
        if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
        {
            PRINT_SQLERR (SQL_HANDLE_STMT, sql_hstmt);
            PERR ("Can't bind col=%d rc=%d", i, rc);
            return;
        }
    }
}

/* =========================================================== */
/* pseudo-private routine */


ODBCRecordSet::ODBCRecordSet(ODBCConnection* _conn)
    : LLRecordSet(_conn)
{
    sql_hstmt = NULL;
}

/* =========================================================== */

void
ODBCRecordSet::release(void)
{
    ncols = -1;

    // Avoid accidental double-release
    if (NULL == sql_hstmt) return;

    // SQLFreeStmt(sql_hstmt, SQL_UNBIND);
    // SQLFreeStmt(sql_hstmt, SQL_CLOSE);
    SQLFreeHandle(SQL_HANDLE_STMT, sql_hstmt);
    sql_hstmt = NULL;

    LLRecordSet::release();
}

/* =========================================================== */

ODBCRecordSet::~ODBCRecordSet()
{
    for (int i=0; i<arrsize; i++)
    {
        delete[] column_labels[i];
        delete[] values[i];
    }
}

/* =========================================================== */

void
ODBCRecordSet::get_column_labels(void)
{
    SQLSMALLINT _ncols;
    SQLRETURN rc;
    int i;

    if (0 <= ncols) return;

    /* If number of columns is negative, then we haven't
     * gotten any results back yet.  Start by getting the
     * column labels.
     */

    rc = SQLNumResultCols(sql_hstmt, &_ncols);
    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        PRINT_SQLERR (SQL_HANDLE_STMT, sql_hstmt);
        PERR ("Can't get num columns rc=%d", rc);
        return;
    }

    if (_ncols > arrsize)
    {
        _ncols = arrsize;
        PERR( "screwed not enough columns !! ");
    }

    for (i=0; i<_ncols; i++)
    {
        char namebuff[300];
        SQLSMALLINT namelen;
        SQLULEN column_size;
        SQLSMALLINT datatype;
        SQLSMALLINT decimal_digits;
        SQLSMALLINT nullable;

        rc = SQLDescribeCol (sql_hstmt, i+1,
                  (SQLCHAR *) namebuff, 299, &namelen,
                  &datatype, &column_size, &decimal_digits, &nullable);
        if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
        {
            PRINT_SQLERR (SQL_HANDLE_STMT, sql_hstmt);
            PERR ("Can't describe col rc=%d", rc);
            return;
        }

        namebuff[namelen] = 0x0;
        // PINFO ("column %d has name\'%s\'", i, namebuff);

        strncpy(column_labels[i], namebuff, DEFAULT_COLUMN_NAME_SIZE);
        column_labels[i][DEFAULT_COLUMN_NAME_SIZE-1] = 0;
        column_datatype[i] = datatype;
    }

    ncols = _ncols;
}

/* =========================================================== */

bool
ODBCRecordSet::fetch_row(void)
{
    // Columns can have null values.  In this case, the ODBC shims
    // will neither set nor clear the value-strings. As a result,
    // some random value from a previous query will still be sitting
    // there, in the values, and get reported to the unlucky user.
    for (int i=0; i<ncols; i++) values[i][0] = 0;

    SQLRETURN rc = SQLFetch(sql_hstmt);

    /* no more data */
    if (SQL_NO_DATA == rc) return false;
    if (SQL_NULL_DATA == rc) return false;

    if ((SQL_SUCCESS != rc) and (SQL_SUCCESS_WITH_INFO != rc))
    {
        PRINT_SQLERR (SQL_HANDLE_STMT, sql_hstmt);
        PERR ("Can't fetch row rc=%d", rc);
        return false;
    }

    return true;
}

#endif /* HAVE_ODBC_STORAGE */
/* ============================= END OF FILE ================= */
