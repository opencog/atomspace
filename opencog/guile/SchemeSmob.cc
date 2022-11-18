/*
 * SchemeSmob.cc
 *
 * Scheme small objects (SMOBS) for the AtomSpace -- core functions.
 *
 * Copyright (c) 2008, 2013, 2014, 2015 Linas Vepstas <linas@linas.org>
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

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/value/Value.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/version.h>
#include "SchemePrimitive.h"
#include "SchemeSmob.h"

using namespace opencog;

/**
 * Just one scheme smob type is used to implement the interface.
 *
 * The cog_misc_tag is used to store all structures, such as atoms
 * and truth values. It is assumed that these structures are all
 * ephemeral (garbage-collected), including the Handles.  Note that
 * atoms in the atomspace have a concrete existence outside of the
 * scheme shell. By contrast, truth values created by the scheme
 * shell are garbage collected by the shell.
 *
 * The type of the "misc" structure is stored in the flag bits;
 * thus, handling is dispatched based on these flags.
 *
 * XXX TODO:
 * The cog_misc_tag should be replaced by a tag-per-class (i.e. we
 * should have a separate tag for handles, tv's, etc.) This would
 * simplify that code, and probably improve performance just a bit.
 */

scm_t_bits SchemeSmob::cog_misc_tag;
std::atomic_flag SchemeSmob::is_inited = ATOMIC_FLAG_INIT;
SCM SchemeSmob::_radix_ten;
SCM SchemeSmob::_alist;

void SchemeSmob::init()
{
	static volatile bool done_with_init = false;
	if (done_with_init) return;

	// Allow only one thread, ever to initialize. Hold off all other
	// threads until initialization is complete.  This could be done
	// with mutexes, but atomic test-n-set is easier and faster.
	if (is_inited.test_and_set())
	{
		while (not done_with_init) { usleep(1000); }
		return;
	}

	// If this assert triggers for you, e.g. because it is 24, then you
	// have an easy fix: change SCM_SMOB_VALUE_PTR_LOC to use
	// SCM_SMOB_OBJECT_1_LOC. If it is larger than 24 then you need a
	// more complex fix.
	static_assert (
		(sizeof(void*) == 8 and sizeof(ValuePtr) == 16) or
		(sizeof(void*) == 4 and sizeof(ValuePtr) == 8),
		"Unexpected ValuePtr size");

	init_smob_type();
	scm_c_define_module("opencog", module_init, NULL);
	scm_c_use_module("opencog");

	atomspace_fluid = scm_make_fluid();
	atomspace_fluid = scm_permanent_object(atomspace_fluid);
	_radix_ten = scm_from_int8(10);
	_alist = scm_from_utf8_symbol("alist");

	// Tell compiler to set flag dead-last, after above has executed.
	asm volatile("": : :"memory");
	done_with_init = true;
}

SchemeSmob::SchemeSmob()
{
	init();
}

void opencog_guile_init(void)
{
	SchemeSmob::init();
}

/* ============================================================== */

void SchemeSmob::init_smob_type(void)
{
	// A SMOB type for everything, including Handles/ValuePtrs.
	// cog_misc_tag = scm_make_smob_type ("opencog-misc", sizeof (ValuePtr));
	cog_misc_tag = scm_make_smob_type ("opencog-misc", sizeof (scm_t_bits));
	scm_set_smob_print (cog_misc_tag, print_misc);
	scm_set_smob_equalp (cog_misc_tag, equalp_misc);
	scm_set_smob_free (cog_misc_tag, free_misc);
}

/* ============================================================== */

SCM SchemeSmob::ss_version(void)
{
	return scm_from_utf8_string(ATOMSPACE_VERSION_STRING);
}

/* ============================================================== */

SCM SchemeSmob::equalp_misc(SCM a, SCM b)
{
	// If they're not something we know about, let scheme sort it out.
	// (Actually, this should never happen ...)
	if ((not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, a)) or
	    (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, b)))
		return scm_equal_p(a, b);

	// If the types don't match, they can't be equal.
	scm_t_bits ta = SCM_SMOB_FLAGS(a);
	scm_t_bits tb = SCM_SMOB_FLAGS(b);
	if (ta != tb)
		return SCM_BOOL_F;

	switch (ta)
	{
		default: // Should never happen.
		case 0:  // Should never happen.
			return SCM_BOOL_F;

		case COG_LOGGER:
		{
			Logger* al = (Logger *) SCM_SMOB_DATA(a);
			Logger* bl = (Logger *) SCM_SMOB_DATA(b);
			scm_remember_upto_here_1(a);
			scm_remember_upto_here_1(b);
			/* Just a simple pointer comparison */
			if (al == bl) return SCM_BOOL_T;
			return SCM_BOOL_F;
		}
		case COG_EXTEND:
		{
			// We compare pointers here, only.
			PrimitiveEnviron* av = (PrimitiveEnviron *) SCM_SMOB_DATA(a);
			PrimitiveEnviron* bv = (PrimitiveEnviron *) SCM_SMOB_DATA(b);
			scm_remember_upto_here_1(a);
			scm_remember_upto_here_1(b);
			if (av == bv) return SCM_BOOL_T;
			return SCM_BOOL_F;
		}
		case COG_PROTOM:
		{
			ValuePtr* av = SCM_SMOB_VALUE_PTR_LOC(a);
			ValuePtr* bv = SCM_SMOB_VALUE_PTR_LOC(b);
			scm_remember_upto_here_1(a);
			scm_remember_upto_here_1(b);
			if (av == bv) return SCM_BOOL_T;
			if (av->get() == bv->get()) return SCM_BOOL_T;
			if (av->get() == nullptr or bv->get() == nullptr) return SCM_BOOL_F;

			// Do not perform content compare! Why? Because the atomspace
			// already does this for us.  If we have two different pointers
			// but their contents compare OK, they must be in different
			// atomspaces, having different TV's and Values on them. They
			// are NOT the same atom! NO to content-compare for atoms!
			if ((*av)->is_atom()) return SCM_BOOL_F;

			if (**av == **bv) return SCM_BOOL_T; // content-compare!
			return SCM_BOOL_F;
		}
	}
}

/* ============================================================== */

/// Return true if s is the scm representation of an Atom or Value
bool SchemeSmob::scm_is_protom(SCM s)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s))
		return false;

	return COG_PROTOM == SCM_SMOB_FLAGS(s);
}

/* ============================================================== */

[[ noreturn ]] void SchemeSmob::throw_exception(const std::exception& ex,
                                                const char *func,
                                                SCM args)
{
	const char * msg = ex.what();
	if (msg and msg[0] != 0)
	{
		// We do NOT log normal opencog exceptions -- the user presumably
		// knows what they are doing. We DO log other exceptions, since
		// they are almost surely due to internal errors.
		if (nullptr == dynamic_cast<const StandardException*>(&ex))
		{
			logger().error("Guile caught C++ exception: %s", msg);
		}

		// Hmmm. scm_throw() is supposed to be able to take a list;
		// however, it gets an "Error while printing exception" if
		// we do actually pass a list. So hack all messages into a
		// string.
		SCM sargs = scm_open_output_string();
		scm_display(args, sargs);
		SCM sout = scm_get_output_string(sargs);
		char * v = scm_to_utf8_string(sout);

		std::string ma = msg;
		ma += "\nFunction args:\n";
		ma += v;
		free(v);

		// scm_misc_error(fe->get_name(), msg, SCM_EOL);
		scm_throw(
			scm_from_utf8_symbol("C++-EXCEPTION"),
			scm_cons(
				scm_from_utf8_string(func),
				scm_cons(
					scm_from_utf8_string(ma.c_str()), SCM_EOL)));

		// Hmm. scm_throw never returns.
	}
	else
	{
		logger().error("Guile caught unknown C++ exception");
		// scm_misc_error(fe->get_name(), "unknown C++ exception", SCM_EOL);
		scm_error_scm(
			scm_from_utf8_symbol("C++ exception"),
			scm_from_utf8_string(func),
			scm_from_utf8_string("unknown C++ exception"),
			args,
			SCM_EOL);
		// Hmm. scm_error never returns.
	}

	// The scm functions never return, so this throw can never occur.
	throw "Impossible, this can't happen";
}

/* ============================================================== */

/// Define the (opencog) module.
//
// This is implemented somewhat awkwardly, in that *scm files are
// being loaded here.  This is due to a design tangle where C++ code
// needs to call the scheme evaluator; each evaluator uses its own
// atomspace in each thread (stored in a fluid); thus, the C++ code
// must link to the module definition, which is thus incomplete without
// the load of the *.scm files.
//
void SchemeSmob::module_init(void*)
{
	// The portion of (opencog) done in C++
	register_procs();
	
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/core_types.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/core-docs.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/utilities.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/atom-cache.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/apply.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/tv.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/types.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/file-utils.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/debug-trace.scm"));
}

#if defined(HAVE_GUILE2) || defined(HAVE_GUILE3)
 #define C(X) ((scm_t_subr) X)
#else
 #define C(X) ((SCM (*) ()) X)
#endif

void SchemeSmob::register_procs()
{
	register_proc("cog-version",           0, 0, 0, C(ss_version));
	register_proc("cog-set-server-mode!",  1, 0, 0, C(ss_set_server_mode));

	register_proc("cog-new-value",         1, 0, 1, C(ss_new_value));
	register_proc("cog-new-atom",          1, 0, 1, C(ss_new_atom));
	register_proc("cog-new-ast",           1, 0, 1, C(ss_new_ast));
	register_proc("cog-new-node",          2, 0, 1, C(ss_new_node));
	register_proc("cog-new-link",          1, 0, 1, C(ss_new_link));
	register_proc("cog-atom",              1, 0, 1, C(ss_atom));
	register_proc("cog-node",              2, 0, 1, C(ss_node));
	register_proc("cog-link",              1, 0, 1, C(ss_link));
	register_proc("cog-extract!",          1, 0, 1, C(ss_extract));
	register_proc("cog-extract-recursive!",1, 0, 1, C(ss_extract_recursive));

	register_proc("cog-value?",            1, 0, 0, C(ss_value_p));
	register_proc("cog-atom?",             1, 0, 0, C(ss_atom_p));
	register_proc("cog-node?",             1, 0, 0, C(ss_node_p));
	register_proc("cog-link?",             1, 0, 0, C(ss_link_p));

	// hash-value of the atom
	register_proc("cog-handle",            1, 0, 0, C(ss_handle));
	register_proc("cog-atom-less?",        2, 0, 0, C(ss_atom_less_p));
	register_proc("cog-equal?",            2, 0, 0, C(ss_equal_p));

	// Value API
	register_proc("cog-value->list",       1, 0, 0, C(ss_value_to_list));
	register_proc("cog-value-ref",         2, 1, 0, C(ss_value_ref));

	// Generic property setter on atoms
	register_proc("cog-set-value!",        3, 0, 0, C(ss_set_value));
	register_proc("cog-set-values!",       2, 0, 0, C(ss_set_values));
	register_proc("cog-set-value-ref!",    4, 0, 0, C(ss_set_value_ref));

	// Value property setters on atoms
	register_proc("cog-set-tv!",           2, 0, 0, C(ss_set_tv));
	register_proc("cog-inc-count!",        2, 0, 0, C(ss_inc_count));
	register_proc("cog-inc-value!",        4, 0, 0, C(ss_inc_value));
	register_proc("cog-update-value!",     3, 0, 0, C(ss_update_value));

	// Property getters on atoms
	register_proc("cog-name",              1, 0, 0, C(ss_name));
	register_proc("cog-number",            1, 0, 0, C(ss_number));
	register_proc("cog-type",              1, 0, 0, C(ss_type));
	register_proc("cog-arity",             1, 0, 0, C(ss_arity));
	register_proc("cog-incoming-set",      1, 1, 0, C(ss_incoming_set));
	register_proc("cog-incoming-size",     1, 1, 0, C(ss_incoming_size));
	register_proc("cog-incoming-by-type",  2, 1, 0, C(ss_incoming_by_type));
	register_proc("cog-incoming-size-by-type", 2, 1, 0, C(ss_incoming_size_by_type));
	register_proc("cog-outgoing-set",      1, 0, 0, C(ss_outgoing_set));
	register_proc("cog-outgoing-by-type",  2, 0, 0, C(ss_outgoing_by_type));
	register_proc("cog-outgoing-atom",     2, 0, 0, C(ss_outgoing_atom));
	register_proc("cog-keys",              1, 0, 0, C(ss_keys));
	register_proc("cog-keys->alist",       1, 0, 0, C(ss_keys_alist));
	register_proc("cog-value",             2, 0, 0, C(ss_value));
	register_proc("cog-value-type",        2, 0, 0, C(ss_value_type));
	register_proc("cog-tv",                1, 0, 0, C(ss_tv));
	register_proc("cog-atomspace",         0, 1, 0, C(ss_as));
	register_proc("cog-as",                0, 1, 0, C(ss_as));
	register_proc("cog-mean",              1, 0, 0, C(ss_get_mean));
	register_proc("cog-confidence",        1, 0, 0, C(ss_get_confidence));
	register_proc("cog-count",             1, 0, 0, C(ss_get_count));

	// Truth-values
	register_proc("cog-tv-mean",           1, 0, 0, C(ss_tv_get_mean));
	register_proc("cog-tv-confidence",     1, 0, 0, C(ss_tv_get_confidence));
	register_proc("cog-tv-count",          1, 0, 0, C(ss_tv_get_count));
	register_proc("cog-tv-merge",          2, 0, 0, C(ss_tv_merge));
	register_proc("cog-tv-merge-hi-conf",  2, 0, 0, C(ss_tv_merge_hi_conf));

	// Atom Spaces
	register_proc("cog-new-atomspace",     0, 0, 1, C(ss_new_as));
	register_proc("cog-atomspace?",        1, 0, 0, C(ss_as_p));
	register_proc("cog-set-atomspace!",    1, 0, 0, C(ss_set_as));
	register_proc("cog-atomspace-env",     0, 1, 0, C(ss_as_env));
	register_proc("cog-atomspace-uuid",    0, 1, 0, C(ss_as_uuid));
	register_proc("cog-atomspace-clear",   0, 1, 0, C(ss_as_clear));
	register_proc("cog-atomspace-readonly?", 0, 1, 0, C(ss_as_readonly_p));
	register_proc("cog-atomspace-ro!",     0, 1, 0, C(ss_as_mark_readonly));
	register_proc("cog-atomspace-rw!",     0, 1, 0, C(ss_as_mark_readwrite));
	register_proc("cog-atomspace-cow?",    0, 1, 0, C(ss_as_cow_p));
	register_proc("cog-atomspace-cow!",    1, 1, 0, C(ss_as_mark_cow));

	// Taking AtomSpace as optional argument
	register_proc("cog-count-atoms",       1, 1, 0, C(ss_count));
	register_proc("cog-map-type",          2, 1, 0, C(ss_map_type));

	// Value types
	register_proc("cog-get-types",         0, 0, 0, C(ss_get_types));
	register_proc("cog-type->int",         1, 0, 0, C(ss_get_type));
	register_proc("cog-get-subtypes",      1, 0, 0, C(ss_get_subtypes));
	register_proc("cog-subtype?",          2, 0, 0, C(ss_subtype_p));

	// Free variables
	register_proc("cog-free-variables",    1, 0, 0, C(ss_get_free_variables));
	register_proc("cog-closed?",           1, 0, 0, C(ss_is_closed));
}

void SchemeSmob::register_proc(const char* name, int req, int opt, int rst, scm_t_subr fcn)
{
	scm_c_define_gsubr(name, req, opt, rst, fcn);
	scm_c_export(name, NULL);
}

/* ===================== END OF FILE ============================ */
