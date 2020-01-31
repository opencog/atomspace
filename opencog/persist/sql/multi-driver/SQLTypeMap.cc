/*
 * SQLTypeMap.cc
 * Save and restore of the Atom Types.
 *
 * Copyright (c) 2008,2009,2013,2017 Linas Vepstas <linas@linas.org>
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
#include <stdlib.h>
#include <unistd.h>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/NameServer.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

using namespace opencog;

/* ================================================================ */
/**
 * Store the concordance of type names to type values.
 *
 * The concordance is used to match up the type id's stored in
 * the SQL database, against those currently in use in the current
 * version of the opencog server. The basic problem is that types
 * can be dynamic in OpenCog -- different versions will have
 * different types, and will assign different type numbers to some
 * given type name. To overcome this, the SQL database stores all
 * atoms according to the type *name* -- although, to save space, it
 * actually stored type ids; however, the SQL type-name-to-type-id
 * mapping can be completely different than the OpenCog type-name
 * to type-id mapping. Thus, tables to convert the one to the other
 * id are needed.
 *
 * Given an opencog type t, the storing_typemap[t] will contain the
 * sqlid for the named type. The storing_typemap[t] will *always*
 * contain a valid value.
 *
 * Given an SQL type sq, the loading_typemap[sq] will contain the
 * opencog type t for the named type, or NOTYPE if this version of
 * opencog does not have this kind of atom.
 *
 * The typemaps must be constructed before any saving or loading of
 * atoms can happen. The typemaps will be a superset (union) of the
 * types used by OpenCog, and stored in the SQL table.
 */
void SQLAtomStorage::setup_typemap(void)
{
	/* Only need to set up the typemap once. */
	if (type_map_was_loaded) return;

	/* Again, under the lock, so we don't race against ourselves. */
	std::lock_guard<std::mutex> lck(_typemap_mutex);
	if (type_map_was_loaded) return;

	// If we are here, we need to reconcile the types currently in
	// use, with a possibly pre-existing typemap. New types must be
	// stored.  So we start by loading a map from SQL (if its there).
	//
	// Be careful to initialize the typemap with invalid types,
	// in case there are (unexpected) holes in the map!
	for (int i=0; i< TYPEMAP_SZ; i++)
	{
		loading_typemap[i] = NOTYPE;
		storing_typemap[i] = -1;
		db_typename[i] = nullptr;
	}

	{
		Response rp(conn_pool);
		rp.exec("SELECT * FROM TypeCodes;");
		rp.store = this;
		rp.rs->foreach_row(&Response::type_cb, &rp);
	}

	unsigned int numberOfTypes = nameserver().getNumberOfClasses();
	for (Type t=0; t<numberOfTypes; t++)
	{
		int sqid = storing_typemap[t];
		/* If this typename is not yet known, record it */
		if (-1 == sqid)
		{
			if (not nameserver().isDefined(t)) continue;

			const char * tname = nameserver().getTypeName(t).c_str();

			// Let the sql id be the same as the current type number,
			// unless this sql number is already in use, in which case
			// we need to find another, unused one.  Its in use if we
			// have a string name associated to it.
			sqid = t;

			if ((db_typename[sqid] != nullptr) &&
				(loading_typemap[sqid] != t))
			{
				// Find some (any) unused type index to use in the
				// sql table. Use the lowest unused value that we
				// can find.
				for (sqid = 0; sqid<TYPEMAP_SZ; sqid++)
				{
					if (nullptr == db_typename[sqid]) break;
				}

				if (TYPEMAP_SZ <= sqid)
				{
					OC_ASSERT("Fatal Error: type table overflow!\n");
				}
			}
			set_typemap(sqid, tname);

#define BUFSZ 160
			char buff[BUFSZ];
			snprintf(buff, BUFSZ,
			         "INSERT INTO TypeCodes (type, typename) "
			         "VALUES (%d, \'%s\');",
			         sqid, tname);
			Response rp(conn_pool);
			rp.exec(buff);
		}
	}

	// Set this last!
	type_map_was_loaded = true;
}

void SQLAtomStorage::set_typemap(int dbval, const char * tname)
{
	Type realtype = nameserver().getType(tname);
	loading_typemap[dbval] = realtype;
	storing_typemap[realtype] = dbval;
	if (db_typename[dbval] != nullptr) free (db_typename[dbval]);
	db_typename[dbval] = strdup(tname);
}

/* ============================= END OF FILE ================= */
