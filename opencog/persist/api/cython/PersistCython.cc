/*
 * opencog/persist/api/cython/PersistCython.cc
 *
 * Copyright (c) 2022,2024 Linas Vepstas <linasvepstas@gmail.com>
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

#include <opencog/atomspace/AtomSpace.h>
#include "PersistCython.h"

using namespace opencog;

namespace opencog {

// XXX FIXME: except for the error messages, most of this code is
// mostly a cut-n-pate of what's in PersistSCM.cc

// =====================================================================

// South Texas Nuclear Project
#define GET_STNP \
	if (not nameserver().isA(hsn->get_type(), STORAGE_NODE)) { \
		throw RuntimeException(TRACE_INFO, \
			"Expecting StorageNode, got %s", hsn->to_short_string().c_str()); \
	} \
 \
	StorageNodePtr stnp = StorageNodeCast(hsn); \
 \
	/* The cast will fail, if the dynamic library that defines the type */ \
	/* isn't loaded. This is the user's job. They can do it by saying */ \
	/* from opencog.storage-foo import * */ \
	if (nullptr == stnp) { \
		if (hsn->get_type() == STORAGE_NODE) { \
			throw RuntimeException(TRACE_INFO, \
				"A StorageNode cannot be used directly; " \
				"only it's sub-types provide the needed implementation!"); \
		} \
		throw RuntimeException(TRACE_INFO, \
			"Not opened; please load module that defines %s\n" \
			"Like so: from opencog.storage-foo import *\n" \
			"where `foo` is the module providing the node.", \
			nameserver().getTypeName(hsn->get_type()).c_str()); \
	}

// =====================================================================
StorageNodePtr _sn;

void storage_open(const Handle& hsn)
{
	GET_STNP;

	if (stnp->connected())
		throw RuntimeException(TRACE_INFO,
			"StorageNode %s is already open!",
			hsn->to_short_string().c_str());

	// It can happen that _sn was deleted earlier. Clobber
	// the smart pointer so use count goes to zero, and the
	// StorageNode dtor runs (which then closes the connection.)
	if (_sn and nullptr == _sn->getAtomSpace()) _sn = nullptr;

	// Like above, but the same StorageNode in a different AtomSpace!?
	// if (_sn and *_sn == *stnp) _sn = nullptr;

	stnp->open();

	if (nullptr == _sn) _sn = stnp;
}

};

void cog_close(Handle hsn)
{
	GET_STNP;
	if (not stnp->connected())
		throw RuntimeException(TRACE_INFO,
			"StorageNode %s is not open!",
			hsn->to_short_string().c_str());

	stnp->close();

	if (stnp == _sn) _sn = nullptr;
}

// =================== END OF FILE ====================
