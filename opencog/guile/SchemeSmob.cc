/*
 * SchemeSmob.c
 *
 * Scheme small objects (SMOBS) for opencog -- core functions.
 *
 * Copyright (c) 2008, 2013, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atomspace/AtomSpace.h>
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

	init_smob_type();
	scm_c_define_module("opencog", module_init, NULL);
	scm_c_use_module("opencog");

	atomspace_fluid = scm_make_fluid();
	atomspace_fluid = scm_permanent_object(atomspace_fluid);
	_radix_ten = scm_from_int8(10);

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
	// A SMOB type for everything, incuding atoms.
	cog_misc_tag = scm_make_smob_type ("opencog-misc", sizeof (scm_t_bits));
	scm_set_smob_print (cog_misc_tag, print_misc);
	scm_set_smob_equalp (cog_misc_tag, equalp_misc);
	// scm_set_smob_mark (cog_misc_tag, mark_misc);
	scm_set_smob_free (cog_misc_tag, free_misc);
}

/* ============================================================== */

SCM SchemeSmob::equalp_misc(SCM a, SCM b)
{
	// If they're not something we know about, let scheme sort it out.
	// (Actualy, this should never happen ...)
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, a))
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
		case COG_AS:
		{
			AtomSpace* as = (AtomSpace *) SCM_SMOB_DATA(a);
			AtomSpace* bs = (AtomSpace *) SCM_SMOB_DATA(b);
			scm_remember_upto_here_1(a);
			scm_remember_upto_here_1(b);
			/* Just a simple pointer comparison */
			if (as == bs) return SCM_BOOL_T;
			return SCM_BOOL_F;
		}
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
			ProtoAtomPtr* av = (ProtoAtomPtr *) SCM_SMOB_DATA(a);
			ProtoAtomPtr* bv = (ProtoAtomPtr *) SCM_SMOB_DATA(b);
			scm_remember_upto_here_1(a);
			scm_remember_upto_here_1(b);
			if (av == bv) return SCM_BOOL_T;
			if (*av == *bv) return SCM_BOOL_T;
			if (**av == **bv) return SCM_BOOL_T;
			return SCM_BOOL_F;
		}
	}
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

	// The portion of (opencog) done in scm files.
	// This needs to stay in sync with /opencog/scm/opencog.scm
	scm_c_eval_string("(add-to-load-path \"/usr/local/share/opencog/scm\")");

	// Set the library load path, so that other modules can find
	// thier libraries. Copied from `scm/opencog.scm` and should stay
	// in sync with that file.  This is NOT needed for ordinary usage
	// from the guile REPL, but is needed by the unit tests.  The problem
	// is that the unit tests create SchemeEval class directly, which
	// causes this code here to run, which defines the opencog scheme
	// module. Thus, a later `(use-modules opencog)` is a no-op because
	// guile thinks that it already has done this. But it really hasn't;
	// the contents of `opencog.scm` were never actually run. So what
	// we do is to manually run the contents of that file, below.
	// Something more elegant would be nice.
	//
	// lib64 is used by various versions of CentOS
	scm_c_eval_string(
		"(define path \"/usr/local/lib/opencog:/usr/local/lib64/opencog\")");

	scm_c_eval_string(
		"(setenv \"LTDL_LIBRARY_PATH\""
		"   (if (getenv \"LTDL_LIBRARY_PATH\")"
		"      (string-append (getenv \"LTDL_LIBRARY_PATH\") \":\" path)"
		"      path))");

#define DO_THE_UBER_BAD_HACKERY_FOR_EFFING_UNIT_TESTS_GRRRR
#ifdef DO_THE_UBER_BAD_HACKERY_FOR_EFFING_UNIT_TESTS_GRRRR
	// Loading files from the project directory is broken by design.
	// However, teh unit tests are broken by design.
	// We REALLY should not do this, it violates basic laws of security,
	// usability, debuggability. But some people think that's OK.
	// Too lazy to fix.  See issue
	// https://github.com/opencog/atomspace/issues/705 for details.
	scm_c_eval_string("(add-to-load-path \"" PROJECT_SOURCE_DIR "/opencog/scm\")");
	scm_c_eval_string("(add-to-load-path \"" PROJECT_BINARY_DIR "\")");
#endif

	scm_primitive_load_path(scm_from_utf8_string("opencog/atoms/proto/core_types.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/core-docs.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/utilities.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/apply.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/tv.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/file-utils.scm"));
	scm_primitive_load_path(scm_from_utf8_string("opencog/base/debug-trace.scm"));
}

#ifdef HAVE_GUILE2
 #define C(X) ((scm_t_subr) X)
#else
 #define C(X) ((SCM (*) ()) X)
#endif

void SchemeSmob::register_procs()
{
	register_proc("cog-new-value",         1, 0, 1, C(ss_new_value));
	register_proc("cog-new-node",          2, 0, 1, C(ss_new_node));
	register_proc("cog-new-link",          1, 0, 1, C(ss_new_link));
	register_proc("cog-node",              2, 0, 1, C(ss_node));
	register_proc("cog-link",              1, 0, 1, C(ss_link));
	register_proc("cog-delete",            1, 0, 1, C(ss_delete));
	register_proc("cog-delete-recursive",  1, 0, 1, C(ss_delete_recursive));
	register_proc("cog-extract",           1, 0, 1, C(ss_extract));
	register_proc("cog-extract-recursive", 1, 0, 1, C(ss_extract_recursive));

	register_proc("cog-value?",            1, 0, 0, C(ss_value_p));
	register_proc("cog-atom?",             1, 0, 0, C(ss_atom_p));
	register_proc("cog-node?",             1, 0, 0, C(ss_node_p));
	register_proc("cog-link?",             1, 0, 0, C(ss_link_p));

	// hash-value of the atom
	register_proc("cog-handle",            1, 0, 0, C(ss_handle));
	register_proc("cog-atom-less?",        2, 0, 0, C(ss_atom_less_p));

	// Value API
	register_proc("cog-value->list",       1, 0, 0, C(ss_value_to_list));
	register_proc("cog-value-ref",         2, 0, 0, C(ss_value_ref));

	// Generic property setter on atoms
	register_proc("cog-set-value!",        3, 0, 0, C(ss_set_value));

	// TV property setters on atoms
	register_proc("cog-set-tv!",           2, 0, 0, C(ss_set_tv));
	register_proc("cog-inc-count!",        2, 0, 0, C(ss_inc_count));

	// property getters on atoms
	register_proc("cog-name",              1, 0, 0, C(ss_name));
	register_proc("cog-type",              1, 0, 0, C(ss_type));
	register_proc("cog-arity",             1, 0, 0, C(ss_arity));
	register_proc("cog-incoming-set",      1, 0, 0, C(ss_incoming_set));
	register_proc("cog-incoming-by-type",  2, 0, 0, C(ss_incoming_by_type));
	register_proc("cog-outgoing-set",      1, 0, 0, C(ss_outgoing_set));
	register_proc("cog-outgoing-by-type",  2, 0, 0, C(ss_outgoing_by_type));
	register_proc("cog-outgoing-atom",     2, 0, 0, C(ss_outgoing_atom));
	register_proc("cog-keys",              1, 0, 0, C(ss_keys));
	register_proc("cog-value",             2, 0, 0, C(ss_value));
	register_proc("cog-tv",                1, 0, 0, C(ss_tv));
	register_proc("cog-as",                1, 0, 0, C(ss_as));

	// Truth-values
	register_proc("cog-new-stv",           2, 0, 0, C(ss_new_stv));
	register_proc("cog-new-ctv",           3, 0, 0, C(ss_new_ctv));
	register_proc("cog-new-itv",           3, 0, 0, C(ss_new_itv));
	register_proc("cog-new-ptv",           3, 0, 0, C(ss_new_ptv));
	register_proc("cog-new-ftv",           2, 0, 0, C(ss_new_ftv));
	register_proc("cog-new-etv",           2, 0, 0, C(ss_new_etv));
	register_proc("cog-tv?",               1, 0, 0, C(ss_tv_p));
	register_proc("cog-stv?",              1, 0, 0, C(ss_stv_p));
	register_proc("cog-ctv?",              1, 0, 0, C(ss_ctv_p));
	register_proc("cog-itv?",              1, 0, 0, C(ss_itv_p));
	register_proc("cog-ptv?",              1, 0, 0, C(ss_ptv_p));
	register_proc("cog-ftv?",              1, 0, 0, C(ss_ftv_p));
	register_proc("cog-etv?",              1, 0, 0, C(ss_etv_p));
	register_proc("cog-tv->alist",         1, 0, 0, C(ss_tv_get_value));
	register_proc("cog-tv-mean",           1, 0, 0, C(ss_tv_get_mean));
	register_proc("cog-tv-confidence",     1, 0, 0, C(ss_tv_get_confidence));
	register_proc("cog-tv-count",          1, 0, 0, C(ss_tv_get_count));
	register_proc("cog-tv-merge",          2, 0, 0, C(ss_tv_merge));
	register_proc("cog-tv-merge-hi-conf",  2, 0, 0, C(ss_tv_merge_hi_conf));

	// Atom Spaces
	register_proc("cog-new-atomspace",     0, 1, 0, C(ss_new_as));
	register_proc("cog-atomspace?",        1, 0, 0, C(ss_as_p));
	register_proc("cog-atomspace",         0, 0, 0, C(ss_get_as));
	register_proc("cog-set-atomspace!",    1, 0, 0, C(ss_set_as));
	register_proc("cog-atomspace-env",     1, 0, 0, C(ss_as_env));
	register_proc("cog-atomspace-uuid",    1, 0, 0, C(ss_as_uuid));
	register_proc("cog-atomspace-clear",   1, 0, 0, C(ss_as_clear));

	// Attention values
	register_proc("cog-new-av",            3, 0, 0, C(ss_new_av));
	register_proc("cog-av?",               1, 0, 0, C(ss_av_p));
	register_proc("cog-av->alist",         1, 0, 0, C(ss_av_get_value));

	// Atom types
	register_proc("cog-get-types",         0, 0, 0, C(ss_get_types));
	register_proc("cog-type->int",         1, 0, 0, C(ss_get_type));
	register_proc("cog-type?",             1, 0, 0, C(ss_type_p));
	register_proc("cog-value-type?",       1, 0, 0, C(ss_value_type_p));
	register_proc("cog-node-type?",        1, 0, 0, C(ss_node_type_p));
	register_proc("cog-link-type?",        1, 0, 0, C(ss_link_type_p));
	register_proc("cog-get-subtypes",      1, 0, 0, C(ss_get_subtypes));
	register_proc("cog-subtype?",          2, 0, 0, C(ss_subtype_p));

	// Iterators
	register_proc("cog-map-type",          2, 0, 0, C(ss_map_type));

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
