/*
 * opencog/persist/csv/TableSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015, 2022 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_CSV_TABLE_SCM_H
#define _OPENCOG_CSV_TABLE_SCM_H

#include <opencog/guile/SchemeModule.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class TableSCM : public ModuleWrap
{
private:
	void init(void);

	void load_table(const Handle&, const std::string&);
public:
	TableSCM(void);
}; // class

/** @}*/
}  // namespace

extern "C" {
void opencog_csv_table_init(void);
};

#endif // _OPENCOG_CSV_TABLE_SCM_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include "table_read.h"

using namespace opencog;

TableSCM::TableSCM(void)
	: ModuleWrap("opencog csv-table")
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	module_init();
}

// Temporary(?) Hacky experimental API.  Subject to change.
void TableSCM::init(void)
{
	define_scheme_primitive("load-table",
	             &TableSCM::load_table, this, "csv-table");
}

// =====================================================================

void TableSCM::load_table(const Handle& h, const std::string& path)
{
	// const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("load-table");
	opencog::load_csv_table(h, path);
}

void opencog_csv_table_init(void)
{
	static TableSCM patty;
}
