/*
 * FUNCTION:
 * Low-Level SQL database API. Super-Simple.
 *
 * HISTORY:
 * Copyright (c) 2002,2008 Linas Vepstas <linas@linas.org>
 * created by Linas Vepstas  March 2002
 * ported to C++ March 2008
 * Made generic, 2017
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

#ifndef _OPENCOG_PERSISTENT_LL_DRIVER_H
#define _OPENCOG_PERSISTENT_LL_DRIVER_H

#include <stack>
#include <string>

/** \addtogroup grp_persist
 *  @{
 */

class LLRecordSet;

class LLConnection
{
    friend class LLRecordSet;
    protected:
        bool is_connected;
        std::stack<LLRecordSet *> free_pool;

    public:
        LLConnection(void);
        virtual ~LLConnection();

        // No future version of this must ever throw!
        bool connected(void) const { return is_connected; }

        virtual LLRecordSet *exec(const char *, bool=false) = 0;
};

class LLRecordSet
{
    friend class LLConnection;
    protected:
        LLConnection *conn;

        int ncols;
        int arrsize;
        char **column_labels;
        int  *column_datatype;
        char **values;
        int  *vsizes;

        LLRecordSet(LLConnection *);
        virtual ~LLRecordSet();

        virtual void get_column_labels(void) = 0;
        int get_col_by_name (const char *);

    public:
        // return true if there's another row.
        virtual bool fetch_row(void) = 0;

        const char * get_value(const char * fieldname);
        int get_column_count();
        const char * get_column_value(int column);

        // call this, instead of the destructor,
        // when done with this instance.
        virtual void release(void);

        // Calls the callback once for each row.
        template<class T> bool
            foreach_row(bool (T::*cb)(void), T *data)
        {
            while (fetch_row())
            {
                bool rc = (data->*cb) ();
                if (rc) return rc;
            }
            return false;
        }

        // Calls the callback once for each column.
        template<class T> bool
            foreach_column(bool (T::*cb)(const char *, const char *), T *data)
        {
            int i;
            if (0 > ncols)
            {
                get_column_labels();
            }

            for (i=0; i<ncols; i++)
            {
                bool rc = (data->*cb) (column_labels[i], values[i]);
                if (rc) return rc;
            }
            return false;
        }
};

/**
 * Handy-dandy utility function: since SQL uses single-quotes
 * for delimiting strings, the strings themselves need to have
 * any single-quotes escaped, to avoid bad syntax.
 */
inline void escape_single_quotes(std::string &str)
{
    std::string::size_type pos = 0;
    pos = str.find ('\'', pos);
    while (pos != std::string::npos)
    {
        str.insert(pos, 1, '\'');
        pos += 2;
        pos = str.find('\'', pos);
    }
}

/** @}*/

#endif // _OPENCOG_PERSISTENT_LL_DRIVER_H
