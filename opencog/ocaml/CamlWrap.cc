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

static struct caml_custom_table opstbl;

static void init()
{
	opstbl.identifier = "OpenCog Value";
	opstbl.finalize = finalize;

	// XXX FIXME
	opstbl.compare = custom_compare_default;
	opstbl.compare_ext = custom_compare_ext_default;
	opstbl.hash = custom_hash_default;
	opstbl.serialize = custom_serialize_default;
	opstbl.deserialize = custom_deserialize_default;
}

value tag_to_value(const ValuePtr& pa)
{
	if (nullptr == pa) return Val_unit;

	// sizeof(ValuePtr) = 16
	value v = caml_alloc_custom(&opstbl, sizeof(ValuePtr), 1, 4000000);

	// Use new so that the smart pointer increments!
	// ValuePtr* pap = new ValuePtr(pa);

	*((ValuePtr*) Data_custom_val(v)) = pa;
	return v;
}

CAMLprim value NewNode(value ostr)
{
	const char * str = String_val(ostr);

	Handle h = asp->add_node(NODE, str);

	return Val_unit;
}
