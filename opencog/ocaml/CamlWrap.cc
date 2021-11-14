/*
 * opencog/ocaml/CamlWrap.cc
 *
 * OCaml wrappers for the AtomSpace -- core functions.
 *
 * Copyright (c) 2021 Linas Vepstas <linas@linas.org>
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

#include <caml/custom.h>
#include <caml/mlvalues.h>
#undef Atom

#include <opencog/atomspace/AtomSpace.h>

#include "CamlWrap.h"

using namespace opencog;

AtomSpacePtr asp = createAtomSpace();

static void finalize(value v)
{
printf("duude finalize called \n");
}

static struct custom_operations opstbl;

static __attribute__ ((constructor)) void init()
{
	opstbl.identifier = "OpenCog Value";
	opstbl.finalize = finalize;

	// XXX FIXME
	opstbl.compare = custom_compare_default;
	opstbl.hash = custom_hash_default;
	opstbl.serialize = custom_serialize_default;
	opstbl.deserialize = custom_deserialize_default;
	opstbl.compare_ext = custom_compare_ext_default;
	opstbl.fixed_length = custom_fixed_length_default;
}

value tag_to_value(const ValuePtr& pa)
{
	if (nullptr == pa) return Val_unit;

	// sizeof(ValuePtr) = 16
	value v = caml_alloc_custom(&opstbl, sizeof(ValuePtr), 1, 4000000);

	void* vd = Data_custom_val(v);
	memset(vd, 0, sizeof(ValuePtr));

	// Smart pointer increments!
	*((ValuePtr*) vd) = pa;
	return v;
}

CAMLprim value NewNode(value ostr)
{
	const char * str = String_val(ostr);

	return tag_to_value(asp->add_node(NODE, str));
}
