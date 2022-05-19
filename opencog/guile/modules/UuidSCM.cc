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
#include <opencog/guile/SchemeSmob.h>
#include <opencog/persist/tlb/TLB.h>

using namespace opencog;
namespace opencog {

/**
 * Wrapper for the UUID subsystem.
 */

class UuidSCM : public ModuleWrap
{
protected:
	TLB _tlb;
	virtual void init();

	UUID add_atom(Handle, UUID);
	Handle get_atom(UUID);
	void remove_atom(Handle);
	void remove_uuid(UUID);

public:
	UuidSCM();
};


UUID UuidSCM::add_atom(Handle h, UUID uuid)
{
	return _tlb.addAtom(h, uuid);
}

Handle UuidSCM::get_atom(UUID uuid)
{
	return _tlb.getAtom(uuid);
}

void UuidSCM::remove_atom(Handle h)
{
	_tlb.removeAtom(h);
}

void UuidSCM::remove_uuid(UUID uuid)
{
	_tlb.removeAtom(uuid);
}

} /*end of namespace opencog*/

UuidSCM::UuidSCM() : ModuleWrap("opencog uuid") {}

/// This is called while (opencog uuid) is the current module.
/// Thus, all the definitions below happen in that module.
void UuidSCM::init(void)
{
	// OK, this is a terrible hack, but basically, we grab whatever
	// atomspace there happens to be there, when this module is loaded,
	// and use that. This will fail utterly when there are multiple
	// atomspaces, and really, we should have nested hierarchical TLB
	// resolvers for nested atomspaces. But for now, no one uses this so
	// what the heck. I'm gonna punt. XXX FIXME.
	AtomSpace* as = SchemeSmob::ss_get_env_as("uuid");
	_tlb.set_resolver(as);

	define_scheme_primitive("cog-add-uuid",
		&UuidSCM::add_atom, this, "uuid");
	define_scheme_primitive("cog-lookup-uuid",
		&UuidSCM::get_atom, this, "uuid");
	define_scheme_primitive("cog-unassign-uuid",
		&UuidSCM::remove_atom, this, "uuid");
	define_scheme_primitive("cog-remove-uuid",
		&UuidSCM::remove_uuid, this, "uuid");
}

extern "C" {
void opencog_uuid_init(void);
};

void opencog_uuid_init(void)
{
	static UuidSCM uuid_scm;
	uuid_scm.module_init();
}
