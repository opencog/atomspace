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

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/proto/ProtoAtom.h>
#include <opencog/atoms/proto/types.h>

#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/truthvalue/TruthValue.h>

#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
/** \addtogroup grp_smob
 *  @{
 */

class Atom;

class SchemeSmob
{
	friend class SchemeEval;
	friend class PrimitiveEnviron;
	template<typename R, typename T, class... Args> friend class SchemePrimitive;
	template<typename R, typename T, class... Args> friend class SchemePrimitiveBase;

	friend class LoggerSCM;

private:

	enum {
		COG_PROTOM = 1, // values or atoms - smart pointer
		COG_AS,         // atom spaces
		COG_LOGGER,     // logger
		COG_EXTEND      // callbacks into C++ code.
	};

	// Encrypt and decrypt atomspace pointer, work around to bug
	// https://github.com/opencog/atomspace/issues/1382
    static AtomSpace* encrypt(AtomSpace* as);
    static AtomSpace* decrypt(AtomSpace* as);

	static std::atomic_flag is_inited;
	static void module_init(void*);
	static void register_procs();
	static void register_proc(const char*, int, int, int, scm_t_subr);

	// The cog_misc_tag are for all other opencog types, such
	// as truth values, which are ephemeral (garbage-collected)
	static scm_t_bits cog_misc_tag;

	// Initialization functions
	static void init_smob_type(void);

	static int print_misc(SCM, SCM, scm_print_state *);
	static SCM equalp_misc(SCM, SCM);
	static SCM mark_misc(SCM);
	static size_t free_misc(SCM);

	static SCM handle_to_scm(const Handle&);
	static SCM protom_to_scm(const ProtoAtomPtr&);
	static SCM tv_to_scm(const TruthValuePtr&);
	static SCM av_to_scm(const AttentionValuePtr&);
	static Handle scm_to_handle(SCM);
	static ProtoAtomPtr scm_to_protom(SCM);
	static TruthValuePtr scm_to_tv(SCM);

	static std::vector<double> scm_to_float_list (SCM);
	static std::vector<ProtoAtomPtr> scm_to_protom_list (SCM);
	static std::vector<std::string> scm_to_string_list (SCM);

	// Value, atom creation and deletion functions
	static SCM ss_new_value(SCM, SCM);
	static SCM ss_new_node(SCM, SCM, SCM);
	static SCM ss_new_link(SCM, SCM);
	static SCM ss_node(SCM, SCM, SCM);
	static SCM ss_link(SCM, SCM);
	static SCM ss_delete(SCM, SCM);
	static SCM ss_delete_recursive(SCM, SCM);
	static SCM ss_extract(SCM, SCM);
	static SCM ss_extract_recursive(SCM, SCM);
	static SCM ss_value_p(SCM);
	static SCM ss_atom_p(SCM);
	static SCM ss_node_p(SCM);
	static SCM ss_link_p(SCM);
	static SCM _radix_ten;

	// Return the hash value of the atom.
	static SCM ss_handle(SCM);
	static SCM ss_atom_less_p(SCM, SCM);

	// Get list endcoded in a value
	static SCM ss_value_to_list(SCM);
	static SCM ss_value_ref(SCM, SCM);

	// Property setters on atoms
	static SCM ss_set_tv(SCM, SCM);
	static SCM ss_set_value(SCM, SCM, SCM);
	static SCM ss_inc_count(SCM, SCM);

	// Atom properties
	static SCM ss_name(SCM);
	static SCM ss_type(SCM);
	static SCM ss_arity(SCM);
	static SCM ss_as(SCM);
	static SCM ss_tv(SCM);
	static SCM ss_keys(SCM);
	static SCM ss_value(SCM, SCM);
	static SCM ss_incoming_set(SCM);
	static SCM ss_incoming_by_type(SCM, SCM);
	static SCM ss_outgoing_set(SCM);
	static SCM ss_outgoing_by_type(SCM, SCM);
	static SCM ss_outgoing_atom(SCM, SCM);

	// Type query functions
	static SCM ss_map_type(SCM, SCM);
	static SCM ss_get_types(void);
	static SCM ss_get_type(SCM);
	static SCM ss_type_p(SCM);
	static SCM ss_value_type_p(SCM);
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
	static SCM ss_new_etv(SCM, SCM);
	static SCM ss_tv_p(SCM);
	static SCM tv_p(SCM, Type);
	static SCM ss_stv_p(SCM);
	static SCM ss_ctv_p(SCM);
	static SCM ss_itv_p(SCM);
	static SCM ss_ptv_p(SCM);
	static SCM ss_ftv_p(SCM);
	static SCM ss_etv_p(SCM);
	static SCM ss_tv_get_value(SCM);
	static SCM ss_tv_get_mean(SCM);
	static SCM ss_tv_get_confidence(SCM);
	static SCM ss_tv_get_count(SCM);
	static SCM ss_tv_merge(SCM, SCM);
	static SCM ss_tv_merge_hi_conf(SCM, SCM);

	// Atom Spaces
	static SCM ss_new_as(SCM);
	static SCM ss_as_p(SCM);
	static SCM ss_get_as(void);
	static SCM ss_set_as(SCM);
	static SCM ss_as_env(SCM);
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
	static SCM ss_av_get_value(SCM);

	// Free variables
	static SCM ss_get_free_variables(SCM);
	static SCM ss_is_closed(SCM);

	// Misc utilities
	static std::string to_string(SCM);
	static std::string protom_to_string(SCM);
	static std::string handle_to_string(const Handle&, int);
	static std::string misc_to_string(SCM);
	static TruthValuePtr get_tv_from_list(SCM);
	static AttentionValuePtr get_av_from_list(SCM);
	static AtomSpace* get_as_from_list(SCM);

	// Logger
	static SCM logger_to_scm(Logger*);
	static Logger* ss_to_logger(SCM);
	static std::string logger_to_string(const Logger *);
	static void release_logger(Logger*);
	static Logger* new_logger();
	static std::mutex lgr_mtx;
	static std::set<Logger*> deleteable_lgr;
	
	// validate arguments coming from scheme passing into C++
	static void throw_exception(const std::exception&, const char *, SCM);
	static AtomSpace* verify_atomspace(SCM, const char *, int pos = 1);
	static Type verify_atom_type(SCM, const char *, int pos = 1);
	static Handle verify_handle(SCM, const char *, int pos = 1);
	static ProtoAtomPtr verify_protom(SCM, const char *, int pos = 1);
	static TruthValuePtr verify_tv(SCM, const char *, int pos = 1);
	static AttentionValuePtr verify_av(SCM, const char *, int pos = 1);
	static HandleSeq verify_handle_list (SCM, const char *,
	                                               int pos = 1);
	static std::vector<double> verify_float_list (SCM, const char *,
	                                               int pos = 1);
	static std::vector<ProtoAtomPtr> verify_protom_list (SCM, const char *,
	                                               int pos = 1);
	static std::vector<std::string> verify_string_list (SCM, const char *,
	                                               int pos = 1);
	static std::string verify_string (SCM, const char *, int pos = 1,
	                                  const char *msg = "string");
	static int verify_int (SCM, const char *, int pos = 1,
	                       const char *msg = "integer");
	static size_t verify_size (SCM, const char *, int pos = 1,
	                           const char *msg = "size integer");
	static double verify_real (SCM, const char *, int pos = 1,
	                           const char *msg = "real number");
	static Logger* verify_logger(SCM, const char *, int pos = 1);

	static SCM atomspace_fluid;
	static void ss_set_env_as(AtomSpace *);
	SchemeSmob();
public:
	// This makes init publicly visible; needed for the guile module.
	static void init();

	// This allows other users to get the atomspace that scheme is
	// using.
	static AtomSpace* ss_get_env_as(const char *);
public:

	// Utility printing functions
	static std::string to_string(const Handle&);
	static std::string as_to_string(const AtomSpace *);
	static std::string av_to_string(const AttentionValuePtr&);
	static std::string tv_to_string(const TruthValuePtr&);
};

/** @}*/
} // namespace opencog

extern "C" {

void opencog_guile_init(void);

};

#endif // _OPENCOG_SCHEME_SMOB_H

#endif // HAVE_GUILE
