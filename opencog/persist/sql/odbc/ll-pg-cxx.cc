/*
 * FUNCTION:
 * Potgres driver -- 
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

#define PERR(...) \
	throw opencog::RuntimeException(TRACE_INFO, __VA_ARGS__);

/* =========================================================== */

LLPGConnection::LLPGConnection(const char * _dbname,
							   const char * _username,
							   const char * _authentication)
	: LLConnection(_dbname, _username, _authentication)
{
	is_connected = false;

	if (NULL == _dbname)
	{
		PERR("No DB specified");
		return;
	}
	pgconn = PQconnectdb("dbname = postgres");

	if (PQstatus(pgconn) != CONNECTION_OK)
	{
		PQfinish(pgconn);
		PERR("Cannot conect to database");
	}

	// is_connected = true;
}

/* =========================================================== */

LLPGConnection::~LLPGConnection()
{
	PQfinish(pgconn);
}

/* =========================================================== */
#define DEFAULT_NUM_COLS 50

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

	rs->alloc_and_bind_cols(DEFAULT_NUM_COLS);

	return rs;
}

/* =========================================================== */

LLRecordSet *
LLPGConnection::exec(const char * buff)
{
	if (!is_connected) return NULL;

	LLPGRecordSet* rs = get_record_set();

	rs->_result = PQexec(pgconn, buff);

	if (PQresultStatus(rs->_result) != PGRES_COMMAND_OK)
	{
		rs->release();
		PERR("Failed to execute!");
	}

	/* Use numbr of columns to indicate that the query hasn't
	 * given results yet. */
	rs->ncols = -1;
	return rs;
}

/* =========================================================== */

void
LLPGRecordSet::alloc_and_bind_cols(int new_ncols)
{
	LLRecordSet::alloc_and_bind_cols(new_ncols);
}

/* =========================================================== */
/* pseudo-private routine */


LLPGRecordSet::LLPGRecordSet(LLPGConnection* _conn)
	: LLRecordSet(_conn)
{
}

/* =========================================================== */

void
LLPGRecordSet::release(void)
{
	PQclear(_result);
	LLRecordSet::release();
}

/* =========================================================== */

LLPGRecordSet::~LLPGRecordSet()
{
}

/* =========================================================== */

void
LLPGRecordSet::get_column_labels(void)
{
	if (0 <= ncols) return;

	/* If number of columns is negative, then we haven't
	 * gotten any results back yet.  Start by getting the
	 * column labels.
	 */
}

/* =========================================================== */


int
LLPGRecordSet::fetch_row(void)
{
	// Columns can have null values.  In this case, the PG shims
	// will neither set nor clear the value-strings. As a result,
	// some random value from a previous query will still be sitting
	// there, in the values, and get reported to the unlucky user.
	for (int i=0; i<ncols; i++) values[i][0] = 0;

	return 1;
}

#endif /* HAVE_PGSQL_STORAGE */
/* ============================= END OF FILE ================= */
