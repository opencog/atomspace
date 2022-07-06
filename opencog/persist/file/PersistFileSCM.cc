/*
 * opencog/persist/file/PersistFileSCM.cc
 *
 * Copyright (c) 2008 by OpenCog Foundation
 * Copyright (c) 2008, 2009, 2013, 2015 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PERSIST_FILE_SCM_H
#define _OPENCOG_PERSIST_FILE_SCM_H

#include <opencog/guile/SchemeModule.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class PersistFileSCM : public ModuleWrap
{
private:
	void init(void);

	void load_file(const std::string&);
public:
	PersistFileSCM(void);
}; // class

/** @}*/
}  // namespace

extern "C" {
void opencog_persist_file_init(void);
};

#endif // _OPENCOG_PERSIST_FILE_SCM_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include "fast_load.h"

using namespace opencog;

PersistFileSCM::PersistFileSCM(void)
	: ModuleWrap("opencog persist-file")
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	module_init();
}

// This API is outdated, and is being kept for backwards-compat.
// Use the FileStorageNode to read and write files using the
// StorageNode API.

void PersistFileSCM::init(void)
{
	define_scheme_primitive("load-file",
	             &PersistFileSCM::load_file, this, "persist-file");
}

// =====================================================================

void PersistFileSCM::load_file(const std::string& path)
{
	const AtomSpacePtr& as = SchemeSmob::ss_get_env_as("load-file");
	opencog::load_file(path, *as.get());
}

void opencog_persist_file_init(void)
{
	static PersistFileSCM patty;
}
