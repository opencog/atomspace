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

#include <libpq-fe.h>

#include "llapi.h"

/** \addtogroup grp_persist
 *  @{
 */

class LLPGRecordSet;

class LLPGConnection : public LLConnection
{
	friend class LLPGRecordSet;
	private:
		PGconn* _pgconn;
		LLPGRecordSet* get_record_set(void);

	public:
		LLPGConnection(const char * uri);
		~LLPGConnection();

		LLRecordSet *exec(const char *, bool);
};

class LLPGRecordSet : public LLRecordSet
{
	friend class LLPGConnection;
	private:
		PGresult* _result;
		int _nrows;
		int _curr_row;

		void setup_cols(int ncols);
		LLPGRecordSet(LLPGConnection *);
		~LLPGRecordSet();

		void get_column_labels(void);

	public:
		// return true if there's another row.
		bool fetch_row(void);

		// call this, instead of the destructor,
		// when done with this instance.
		void release(void);
};

/** @}*/

#endif /* HAVE_PGSQL_STORAGE */
#endif // _OPENCOG_PERSISTENT_POSTGRES_DRIVER_H
