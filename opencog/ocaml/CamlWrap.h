/*
 * opencog/ocaml/CamlWrap.h
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

#ifndef _CAML_WRAP_H_
#define _CAML_WRAP_H_

#include <caml/mlvalues.h>
#undef Atom

#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/base/Handle.h>

using namespace opencog;

extern "C" {
CAMLprim value NewNode(value, Type);
CAMLprim value NewLink(value, Type);
CAMLprim void print_atomspace(void);
CAMLprim value atom_to_sexpr(value);
CAMLprim value atom_string_printer(value);
CAMLprim value execute(value);
CAMLprim value evaluate(value);
}

value tag_to_value(const ValuePtr& pa);
ValuePtr value_to_tag(value);


std::string oc_to_caml_str(const Handle& h,
                           const std::string& indent=empty_string);
std::string oc_to_caml_str(const ValuePtr& vp,
                           const std::string& indent=empty_string);

#endif // _CAML_WRAP_H_
