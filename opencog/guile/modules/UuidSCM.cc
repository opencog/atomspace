/*
 * UuidSCM.cc
 *
 * Copyright (c) 2020 Linas Vepstas
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

#include <opencog/guile/SchemeModule.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/atomspaceutils/TLB.h>

using namespace opencog;
namespace opencog {

/**
 * Wrapper for the UUID subsystem.
 */

class UuidSCM : public ModuleWrap
{
protected:
	virtual void init();

	UUID add_atom(Handle, UUID);
	Handle get_atom(UUID);
	UUID get_uuid(Handle);
	void remove_atom(Handle);
	void remove_uuid(UUID);

public:
	UuidSCM();
};


UUID UuidSCM::add_atom(Handle h, UUID uuid)
{
	return 0;
}

} /*end of namespace opencog*/

UuidSCM::UuidSCM() : ModuleWrap("opencog uuid") {}

/// This is called while (opencog uuid) is the current module.
/// Thus, all the definitions below happen in that module.
void UuidSCM::init(void)
{
	define_scheme_primitive("cog-assign-uuid",
		&UuidSCM::add_atom, this, "uuid");
}

extern "C" {
void opencog_uuid_init(void);
};

void opencog_uuid_init(void)
{
    static UuidSCM uuid_scm;
    uuid_scm.module_init();
}
