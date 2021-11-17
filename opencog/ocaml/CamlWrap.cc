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

#define CAML_NAME_SPACE
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#undef Atom

#include <opencog/atomspace/AtomSpace.h>

#include "CamlWrap.h"

using namespace opencog;

AtomSpacePtr asp = createAtomSpace();

static void finalize(value v)
{
	void* vd = Data_custom_val(v);

	// Force smart pointer decrement!
	*((ValuePtr*) vd) = nullptr;
}

static struct custom_operations opstbl;

static __attribute__ ((constructor)) void init()
{
	opstbl.identifier = "Atomese";
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

ValuePtr value_to_tag(value v)
{
	return *((ValuePtr*) Data_custom_val(v));
}

// Functions in the actual interface need to be declared 'CAMLprim'
// and need to use 'CAMLparam' and 'CAMLreturn'
CAMLprim value NewNode(value vname, Type t)
{
	CAMLparam1(vname);
	const char* name = String_val(vname);
	CAMLreturn(tag_to_value(asp->add_node(t, name)));
}

CAMLprim value NewLink(value vatomlist, Type t)
{
	CAMLparam1(vatomlist);
	HandleSeq oset;

	// vatomlist is a linked list. Walk it.
	CAMLlocal1(p);
	p = vatomlist;
	while (p != Val_unit)
	{
		Handle h(HandleCast(value_to_tag(Field(p, 0))));
		oset.emplace_back(h);
		p = Field(p, 1);
	}
	CAMLreturn(tag_to_value(asp->add_link(t, std::move(oset))));
}

CAMLprim void print_atomspace(void)
{
	CAMLparam0();
	printf("Atomspace size %lu contents:\n%s\n",
		asp->get_size(), asp->to_string().c_str());

	CAMLreturn0;
}

CAMLprim void print_atom(value vatom)
{
	CAMLparam1(vatom);
	printf("duuduue its an atoms yeah\n");

	CAMLreturn0;
}

CAMLprim void atom_pretty_printer(value outport, value vatom)
{
	CAMLparam2(outport, vatom);
	printf("duuduue portapotty yeah\n");

	CAMLreturn0;
}
