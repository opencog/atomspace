/*
 * SchemeSmob.h
 *
 * Guile SMOB interface for opencog atoms and truth values.
 * This class implements the actual functions the guile shell invokes
 * when it sees the opencog-specific proceedures.
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linasvepstas@gmail.com>
 */

#ifdef HAVE_GUILE

#ifndef _OPENCOG_SCHEME_SMOB_H
#define _OPENCOG_SCHEME_SMOB_H

#include <atomic>
#include <map>
#include <string>
#include <vector>
#include <libguile.h>

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/types.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/AttentionValue.h>
#include <opencog/truthvalue/TruthValue.h>

namespace opencog {
/** \addtogroup grp_smob
 *  @{
 */

class Atom;

class SchemeSmob
{
	friend class SchemeEval;
	friend class PrimitiveEnviron;
	template <typename TT> friend class SchemePrimitive;

private:

	enum {
		COG_UUID = 1, // unsigned long int
		COG_HANDLE,   // smart pointer
		COG_TV,       // truth values
		COG_AV,       // attention values
		COG_AS,       // atom spaces
		COG_EXTEND    // callbacks into C++ code.
	};

	static std::atomic_flag is_inited;
	static void register_procs(void*);
	static void register_proc(const char*, int, int, int, scm_t_subr);
	static void register_proc_from_scm(const char*);

	// The cog_misc_tag are for all other opencog types, such
	// as truth values, which are ephemeral (garbage-collected)
	static scm_t_bits cog_misc_tag;

	// Initialization functions
	static void init_smob_type(void);

	static int print_misc(SCM, SCM, scm_print_state *);
	static SCM equalp_misc(SCM, SCM);
	static SCM mark_misc(SCM);
	static size_t free_misc(SCM);

	// Atom creation and deletion functions
	static SCM ss_new_node(SCM, SCM, SCM);
	static SCM ss_new_link(SCM, SCM);
	static SCM ss_node(SCM, SCM, SCM);
	static SCM ss_link(SCM, SCM);
	static SCM ss_delete(SCM, SCM);
	static SCM ss_delete_recursive(SCM, SCM);
	static SCM ss_purge(SCM, SCM);
	static SCM ss_purge_recursive(SCM, SCM);
	static SCM ss_atom_p(SCM);
	static SCM ss_node_p(SCM);
	static SCM ss_link_p(SCM);
	static SCM _radix_ten;

	// Atoms to ints, and back.
	static SCM ss_atom(SCM);
	static SCM ss_handle(SCM);

	// return the int of Handle::UNDEFINED
	static SCM ss_undefined_handle(void);

	// Set properties of atoms
	static SCM ss_set_av(SCM, SCM);
	static SCM ss_set_tv(SCM, SCM);
	static SCM ss_merge_tv(SCM, SCM);
	static SCM ss_merge_hi_conf_tv(SCM, SCM);
	static SCM ss_inc_vlti(SCM);
	static SCM ss_dec_vlti(SCM);

	// Atom properties
	static SCM ss_name(SCM);
	static SCM ss_type(SCM);
	static SCM ss_arity(SCM);
	static SCM ss_as(SCM);
	static SCM ss_av(SCM);
	static SCM ss_tv(SCM);
	static SCM ss_incoming_set(SCM);
	static SCM ss_outgoing_set(SCM);

	// Type query functions
	static SCM ss_map_type(SCM, SCM);
	static SCM ss_get_types(void);
	static SCM ss_get_type(SCM);
	static SCM ss_type_p(SCM);
	static SCM ss_node_type_p(SCM);
	static SCM ss_link_type_p(SCM);
	static SCM ss_get_subtypes(SCM);
	static SCM ss_subtype_p(SCM, SCM);

	// Truth values
	static SCM ss_new_stv(SCM, SCM);
	static SCM ss_new_ctv(SCM, SCM, SCM);
	static SCM ss_new_itv(SCM, SCM, SCM);
	static SCM ss_new_ptv(SCM, SCM, SCM);
	static SCM ss_new_ftv(SCM, SCM);
	static SCM ss_tv_p(SCM);
	static SCM tv_p(SCM, TruthValueType);
	static SCM ss_stv_p(SCM);
	static SCM ss_ctv_p(SCM);
	static SCM ss_itv_p(SCM);
	static SCM ss_ptv_p(SCM);
	static SCM ss_ftv_p(SCM);
	static SCM take_tv(TruthValue *);
	static SCM tv_to_scm(TruthValuePtr);
	static SCM ss_tv_get_value(SCM);
	static SCM ss_tv_get_mean(SCM);
	static SCM ss_tv_get_confidence(SCM);
	static SCM ss_tv_get_count(SCM);

	// Atom Spaces
	static SCM ss_new_as(SCM);
	static SCM ss_as_p(SCM);
	static SCM ss_get_as(void);
	static SCM ss_set_as(SCM);
	static SCM ss_as_uuid(SCM);
	static SCM ss_as_clear(SCM);
	static SCM make_as(AtomSpace *);
	static void release_as(AtomSpace *);
	static AtomSpace* ss_to_atomspace(SCM);
	static std::mutex as_mtx;
	static std::map<AtomSpace*, int> deleteable_as;
	static void as_ref_count(SCM, AtomSpace *);

	// Attention values
	static SCM ss_new_av(SCM, SCM, SCM);
	static SCM ss_av_p(SCM);
	static SCM take_av(AttentionValue *);
	static SCM ss_av_get_value(SCM);

	// AttentionalFocus and AttentionalFocus Boundary
	static SCM ss_af_boundary(void);
	static SCM ss_set_af_boundary(SCM);
	static SCM ss_af(void);

	// Callback into misc C++ code.
	static SCM ss_ad_hoc(SCM, SCM);

	// Misc utilities
	static std::string to_string(SCM);
	static TruthValuePtr to_tv(SCM);
	static std::string handle_to_string(SCM);
	static std::string handle_to_string(Handle, int);
	static std::string misc_to_string(SCM);
	static TruthValue *get_tv_from_list(SCM);
	static AttentionValue *get_av_from_list(SCM);
	static AtomSpace *get_as_from_list(SCM);

	// validate arguments coming from scheme passing into C++
	static void throw_exception(const char *, const char *);
	static Type verify_atom_type(SCM, const char *, int pos = 1);
	static Handle verify_handle(SCM, const char *, int pos = 1);
	static TruthValue * verify_tv(SCM, const char *, int pos = 1);
	static AttentionValue * verify_av(SCM, const char *, int pos = 1);
	static std::vector<Handle> verify_handle_list (SCM, const char *,
	                                               int pos = 1);
	static std::string verify_string (SCM, const char *, int pos = 1,
	                                  const char *msg = "expecting string");
	static int verify_int (SCM, const char *, int pos = 1,
	                       const char *msg = "expecting integer");

	static SCM atomspace_fluid;
	static void ss_set_env_as(AtomSpace *);
	SchemeSmob();
public:
	// This makes init publicly visible; needed for the guile module.
	static void init();

	// This allows other users to get the atomspace that scheme is
	// using.
	static AtomSpace* ss_get_env_as(const char *);

	// Helper functions XXX why are these public ??
	// XXX Because the embodiment code uses them :-(
	// The embodiment code should be refactored to not use these.
	static SCM handle_to_scm(Handle);
	static Handle scm_to_handle(SCM);
public:

	// Utility printing functions
	static std::string to_string(Handle);
	static std::string as_to_string(const AtomSpace *);
	static std::string av_to_string(const AttentionValue *);
	static std::string tv_to_string(const TruthValue *);
};

/** @}*/
} // namespace opencog

extern "C" {

void opencog_guile_init(void);

};

#endif // _OPENCOG_SCHEME_SMOB_H

#endif // HAVE_GUILE
