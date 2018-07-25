/*
 * FUNCTION:
 * Postgres driver --
 *
 * Threading:
 * ----------
 * This class is thread-enabled but not thread-safe. Two threads should
 * not try to use one instance of this class at the same time. Each
 * thread should construct it's own instance of this class. This class
 * uses no globals.
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

#ifdef HAVE_PGSQL_STORAGE

#include <libpq-fe.h>

#include <opencog/util/exceptions.h>
#include <opencog/util/Logger.h>
#include <opencog/util/platform.h>

#include "ll-pg-cxx.h"

/* =========================================================== */

LLPGConnection::LLPGConnection(const char * uri)
{
	is_connected = false;

	_pgconn = PQconnectdb(uri);

	if (PQstatus(_pgconn) != CONNECTION_OK)
	{
		std::string msg = PQerrorMessage(_pgconn);
		PQfinish(_pgconn);
		throw opencog::RuntimeException(TRACE_INFO,
			"Cannot connect to database: %s", msg.c_str());
	}

	is_connected = true;
}

/* =========================================================== */

LLPGConnection::~LLPGConnection()
{
	PQfinish(_pgconn);
}

/* =========================================================== */
#define DEFAULT_NUM_COLS 20

LLPGRecordSet * LLPGConnection::get_record_set(void)
{
	LLPGRecordSet *rs;
	if (!free_pool.empty())
	{
		LLRecordSet* llrs = free_pool.top();
		rs = dynamic_cast<LLPGRecordSet*>(llrs);
		free_pool.pop();
		rs->ncols = -1;
	}
	else
	{
		rs = new LLPGRecordSet(this);
	}

	rs->setup_cols(DEFAULT_NUM_COLS);

	return rs;
}

/* =========================================================== */

LLRecordSet *
LLPGConnection::exec(const char * buff, bool trial_run)
{
	if (!is_connected) return NULL;

	LLPGRecordSet* rs = get_record_set();

	rs->_result = PQexec(_pgconn, buff);

	ExecStatusType rest = PQresultStatus(rs->_result);
	if (rest != PGRES_COMMAND_OK and
	    rest != PGRES_EMPTY_QUERY and
	    rest != PGRES_TUPLES_OK)
	{
		// Don't log trial-run failures. Just throw.
		if (trial_run and PGRES_FATAL_ERROR == rest)
		{
			rs->release();
			throw opencog::SilentException();
		}

		std::string msg;
		if (PQstatus(_pgconn) != CONNECTION_OK)
		{
			msg = "No connection to the database!";
		}
		else
		{
			msg = "PQresult message: ";
			msg += PQresultErrorMessage(rs->_result);
			msg += "\nPQ query was: ";
			msg += buff;
		}
		rs->release();

		opencog::logger().warn("%s", msg.c_str());

		throw opencog::RuntimeException(TRACE_INFO,
			"Failed to execute SQL command!\n%s", msg.c_str());
	}

	/* Use numbr of columns to indicate that the query hasn't
	 * given results yet. */
	rs->ncols = -1;
	return rs;
}

/* =========================================================== */

void
LLPGRecordSet::setup_cols(int new_ncols)
{
	if (new_ncols <= arrsize) return;

	if (column_labels) delete[] column_labels;
	column_labels = new char*[new_ncols];
	memset(column_labels, 0, new_ncols * sizeof(char*));

	if (values) delete[] values;
	values = new char*[new_ncols];
	memset(values, 0, new_ncols * sizeof(char*));

   arrsize = new_ncols;
}

/* =========================================================== */
/* pseudo-private routine */


LLPGRecordSet::LLPGRecordSet(LLPGConnection* _conn)
	: LLRecordSet(_conn)
{
	_result = nullptr;
	_nrows = -1;
	_curr_row = -1;
}

/* =========================================================== */

void
LLPGRecordSet::release(void)
{
	PQclear(_result);
	_result = nullptr;
	_nrows = -1;
	_curr_row = -1;
	ncols = -1;
	memset(column_labels, 0, arrsize * sizeof(char*));
	memset(values, 0, arrsize * sizeof(char*));
	LLRecordSet::release();
}

/* =========================================================== */

LLPGRecordSet::~LLPGRecordSet()
{
}

/* =========================================================== */

#define DEFAULT_COLUMN_NAME_SIZE 121

void
LLPGRecordSet::get_column_labels(void)
{
	if (0 <= ncols) return;

	/* If number of columns is negative, then we haven't
	 * gotten any results back yet.  Start by getting the
	 * column labels.
	 */

	ncols = PQnfields(_result);
	for (int i=0; i<ncols; i++)
	{
		column_labels[i] = PQfname(_result, i);
	}
}

/* =========================================================== */

#define DEFAULT_VARCHAR_SIZE 4040

bool
LLPGRecordSet::fetch_row(void)
{
	if (_nrows < 0)
	{
		_curr_row = 0;
		_nrows = PQntuples(_result);
	}
	if (_nrows <= _curr_row or _curr_row < 0) return false;

	if (ncols < 0) get_column_labels();

	for (int i=0; i< ncols; i++)
	{
		values[i] = PQgetvalue(_result, _curr_row, i);
	}
	_curr_row++;
	return true;
}

#endif /* HAVE_PGSQL_STORAGE */
/* ============================= END OF FILE ================= */
