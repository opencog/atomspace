/*
 * opencog/persist/api/PersistSCM.cc
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

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/persist/api/StorageNode.h>

namespace opencog
{
class PersistSCM : public ModuleWrap
{
private:
	void init(void);
	static void direct_setvalue(Handle, Handle, ValuePtr);

public:
	PersistSCM(void);
}; // class

}  // namespace

extern "C" {
void opencog_persist_init(void);
};

using namespace opencog;

PersistSCM::PersistSCM(void)
	: ModuleWrap("opencog persist")
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	module_init();
}

void PersistSCM::init(void)
{
	// define_scheme_primitive(..., false); means that these functions
	// will NOT be `define-public` and just plain `define`. Thus,
	// accessible within the module, but not outside of it.
	define_scheme_primitive("direct-setvalue!",
	             &PersistSCM::direct_setvalue, "persist", false);
}

// =====================================================================

// Guile wrapper to set a value on an Atom directly, instead of going
// through the AtomSpace. By "set a value", we really mean "send a
// message to a StorageNode": this is specialized to work for
// StorageNodes only.
//
// This method is a historical artifact, and should be eventually
// eliminated; in most cases, the user can/should use cog-setvalue!
// function directly. However, there's a bug...
//
// The cog-setvalue! function calls AtomSpace::SetValue() which does
// some checking regarding frames and read-only AtomSpaces, and only
// then calls Atom::SetValue() as appropriate, possibly performing a
// copy-on-write (COW) of the Atom. And this is the right thing to do ...
// mostly. When using frames (AtomSpaces layered one on top another),
// an accidental COW of the StorageNode results in hard-to-debug errors,
// because the COW'ed StorageNode will have a different connection to
// the underlying Storage. And we don't want that.
//
// The correct long-term fix is to avoid/prevent the COW of Atoms,
// such as StorageNodes, that are used to send messages. But fixing
// this is hard, because the frame code is tangled and complicated.
// And low priority, because kind of no one uses frames, more or less.
// Except for the unit tests, which use them heavily.
//
// So, for backwards compat, for the scheme bindings only, we implement
// the direct setvalue below. The unit tests, and the framing API in
// general, probably needs some kind of redo to work nicely, easily,
// rapidly, correctly.
//
void PersistSCM::direct_setvalue(Handle hsn, Handle key, ValuePtr val)
{
	if (not nameserver().isA(hsn->get_type(), STORAGE_NODE)) {
		throw RuntimeException(TRACE_INFO,
			"Expecting StorageNode, got %s", hsn->to_short_string().c_str());
	}

	StorageNodePtr stnp = StorageNodeCast(hsn);

	/* The cast will fail, if the dynamic library that defines the type */
	/* isn't loaded. This is the user's job. They can do it by saying */
	/* (use-modules (opencog persist-foo)) */
	if (nullptr == stnp) {
		if (hsn->get_type() == STORAGE_NODE) {
			throw RuntimeException(TRACE_INFO,
				"A StorageNode cannot be used directly; "
				"only it's sub-types provide the needed implementation!");
		}
		throw RuntimeException(TRACE_INFO,
			"Not opened; please load module that defines %s\n"
			"Like so: (use-modules (opencog persist-foo))\n"
			"where `foo` is the module providing the node.",
			nameserver().getTypeName(hsn->get_type()).c_str());
	}

	stnp->setValue(key, val);
}

// =====================================================================

void opencog_persist_init(void)
{
	static PersistSCM patty;
}

// =================== END OF FILE ====================
