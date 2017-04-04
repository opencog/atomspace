/*
 * SchemePrimitive.h
 *
 * Allow C++ code to be invoked from scheme --
 * by creating a new scheme primitive function.
 *
 * Copyright (C) 2009,2015 Linas Vepstas
 */

#ifdef HAVE_GUILE

#ifndef _OPENCOG_SCHEME_PRIMITIVE_H
#define _OPENCOG_SCHEME_PRIMITIVE_H

#include <string>

#include <opencog/atoms/base/Handle.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/guile/SchemeSmob.h>
#include <libguile.h>
#include <opencog/atoms/base/ClassServer.h>

namespace opencog {
/** \addtogroup grp_smob
 *  @{
 */

class AtomSpace;
class PrimitiveEnviron
{
	friend class SchemeEval;
	friend class SchemeSmob;
	private:
		static bool is_inited;
		static void init(void);
		static void init_in_module(void*);

		static void * c_wrap_register(void *);
		void really_do_register(const char*, const char*, int);

		const char *tmp_module;
		const char *tmp_name;
		int tmp_nargs;

		static SCM do_call(SCM, SCM);
		static PrimitiveEnviron *verify_pe(SCM, const char *);

	protected:
		void do_register(const char*, const char*, int);
		virtual SCM invoke (SCM) = 0;
		virtual const char *get_module(void) = 0;
		virtual const char *get_name(void) = 0;
		virtual size_t get_size(void) = 0;
		virtual ~PrimitiveEnviron();
};

//! SchemePrimitive -- template class used for defining a new primitve
//!
//! This template may be used to wrap a C++ object in such a way that it
//! can be invoked from scheme code.  The file "PrimitiveExample.cc"
//! provides a detailed example of how to do this, and how to invoke the
//! resulting primitive.
//!
//! This template has a handful of pre-defined signatures. If you cannot
//! find the signature that you need, you will need to extend this
//! template a bit to add the needed signature. This shouldn't be hard;
//! just work from the existing examples; please keep things in
//! alphabetical order.
//
// Users and abusers of this file:
//
// There is a whole lot of abuse of this file, by numerous extremely
// poorly designed scheme bindings. Some are OK and just fine, but 
// many of these users need to be fixed.  It was never the intent
// that this would become a free-for-all, anything-goes dumping ground
// for badly designed API's. But that is what it has become.  Time
// to reverse the decay.
//
// Here is a list of some of the users:
//
// B_B    -- cogutils logger boolean setters/getters.
// B_HH   -- PatternSCM::value_is_type(), etc.
//        -- LGDictSCM::do_lg_conn_type_match(), etc.
// H_HZ   -- cog-bind-first-n
// S_AS   -- CogServerSCM::start_server()
// S_S    -- cogutils logger API, see guile/LoggerSCM.h
// S_SS   -- DistSCM  (Gearman server)
// S_V    -- CogServerSCM::stop_server()
//        -- cogutils logger get_level(), etc.
// V_S    -- cogutils logger setters
//        -- ZMQPersistSCM::do_open()
// V_SSS  -- SQLPersistSCM::do_open()
// V_V    -- SQLPersistSCM::do_close(), do_load(), do_store()
//        -- WordSenseProcessor::run_wsd()
//
// Users that probably should be fixed:
// V_SA   -- PythonSCM::apply_as
//           That's backwards, should probably be V_AS
// K_H    -- SuRealSCM::do_non_cached_sureal_match(), etc.
//           Should be re-written to use H_H not K_H
//
// This API needs to be re-thought, from scratch. Its offensive.
// H_HTQB -- FuzzySCM::do_nlp_fuzzy_match()
//
// This API needs to be re-thought, from scratch. Its offensive.
// Its complete and total crap.
// D_HHTB -- DimEmbedModule::euclidDist()
// Q_HTIB -- DimEmbedModule::kNearestNeighbors()
// V_T    -- DimEmbedModule::logAtomEmbedding()
// V_TI   -- DimEmbedModule::embedAtomSpace()
// V_TIDI -- DimEmbedModule::addKMeansClusters

template<class T>
class SchemePrimitive : public PrimitiveEnviron
{
	private:
		union
		{
			// signature naming convention:
			// a == AtomSpace*
			// b == bool
			// d == double
			// h == Handle
			// i == int
			// q == HandleSeq
			// k == HandleSeqSeq
			// s == string
			// p == TruthValuePtr
			// t == Type
			// v == void
			// z == size_t
			// Extend the above, if required.

			// Below is the list of currently supported signatures.
			// Extend as needed.
			bool (T::*b_b)(bool);
			bool (T::*b_hh)(Handle, Handle);
			bool (T::*b_sddd)(const std::string&,double,double,double);
			bool (T::*b_sdii)(const std::string&,double,int,int);
			bool (T::*b_shddd)(const std::string&,Handle,double,double,double);
			bool (T::*b_siddd)(const std::string&,int,double,double,double);
			double (T::*d_hhtb)(Handle, Handle, Type, bool);
			double (T::*d_s)(const std::string&);
			double (T::*d_shhi)(const std::string&, Handle, Handle,int);
			Handle (T::*h_h)(Handle);
			Handle (T::*h_hh)(Handle, Handle);
			Handle (T::*h_hhh)(Handle, Handle, Handle);
			Handle (T::*h_hhhh)(Handle, Handle, Handle, Handle);
			Handle (T::*h_hs)(Handle, const std::string&);
			Handle (T::*h_htqb)(Handle, Type, const HandleSeq&, bool);
			Handle (T::*h_hz)(Handle, size_t);
			Handle (T::*h_sddd)(const std::string&,double,double,double);
			Handle (T::*h_sh)(const std::string&,Handle);
			Handle (T::*h_shddd)(const std::string&,Handle,double,double,double);
			Handle (T::*h_shi)(const std::string&,Handle,int);
			Handle (T::*h_siddd)(const std::string&,int,double,double,double);
			HandleSeq (T::*q_htib)(Handle, Type, int, bool);
			HandleSeqSeq (T::*k_h)(Handle);
			int (T::*i_v)(void);
			int (T::*i_s)(const std::string&);
			int (T::*i_shhhi)(const std::string&, Handle, Handle,Handle,int);
			std::string (T::*s_as)(AtomSpace*, const std::string&);
			std::string (T::*s_s)(const std::string&);
			std::string (T::*s_ss)(const std::string&, const std::string&);
			std::string (T::*s_sss)(const std::string&,
			                        const std::string&,
			                        const std::string&);
			std::string (T::*s_v)(void);
			TruthValuePtr (T::*p_h)(Handle);
			void (T::*v_b)(bool);
			void (T::*v_i)(int);
			void (T::*v_h)(Handle);
			void (T::*v_s)(const std::string&);
			void (T::*v_sa)(const std::string&, AtomSpace*);
			void (T::*v_sh)(const std::string&, Handle);
			void (T::*v_shi)(const std::string&, Handle, int);
			void (T::*v_ss)(const std::string&,
			                const std::string&);
			void (T::*v_sss)(const std::string&,
			                 const std::string&,
			                 const std::string&);
			void (T::*v_t)(Type);
			void (T::*v_ti)(Type, int);
			void (T::*v_tidi)(Type, int, double, int);
			void (T::*v_v)(void);
		} method;
		T* that;
		const char *scheme_module;
		const char *scheme_name;
		enum
		{
			B_B,   // return boolean, take boolean
			B_HH,  // return boolean, take handle and handle
			D_HHTB,// return double, take handle, handle, and type
			H_H,   // return handle, take handle
			H_HH,  // return handle, take handle and handle
			H_HS,  // return handle, take handle and string
			H_HHH, // return handle, take handle, handle and Handle
			H_HHHH, // return handle, take handle, handle, Handle and Handle
			H_HTQB, // return handle, take handle, type, HandleSeq and boolean
			H_HZ,  // return handle, take handle and size_t
			I_V,   // return int, take void
			Q_HTIB,// return HandleSeq, take handle, type, and bool
			K_H,   // return HandleSeqSeq, take Handle
			S_AS,  // return string, take AtomSpace* and string
			S_S,   // return string, take string
			S_SS,  // return string, take two strings
			S_SSS, // return string, take three strings
			S_V,   // return string, take void
			P_H,   // return truth value, take Handle
			V_B,   // return void, take bool
			V_I,   // return void, take int
			V_H,   // return void, take Handle
			V_S,   // return void, take string
			V_SA,  // return void, take string, Atomspace
			V_SS,  // return void, take two strings
			V_SSS, // return void, take three strings
			V_T,   // return void, take Type
			V_TI,  // return void, take Type and int
			V_TIDI,// return void, take Type, int, double, and int
			V_V    // return void, take void
		} signature;

		virtual SCM invoke (SCM args)
		{
			SCM rc = SCM_EOL;
			switch (signature)
			{
				case B_B:
				{
					bool b = scm_to_bool(scm_car(args));
					bool rb = (that->*method.b_b)(b);
					if (rb) { rc = SCM_BOOL_T; } else { rc = SCM_BOOL_F; }
					break;
				}
				case B_HH:
				{
					Handle h1(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));
					Handle h2(SchemeSmob::verify_handle(scm_cadr(args), scheme_name, 2));
					bool b = (that->*method.b_hh)(h1, h2);

					if (b)
						rc = SCM_BOOL_T;
					else
						rc = SCM_BOOL_F;

					break;
				}
				case D_HHTB:
				{
					Handle h1(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));
					Handle h2(SchemeSmob::verify_handle(scm_cadr(args), scheme_name, 2));
					Type t = SchemeSmob::verify_atom_type(scm_caddr(args), scheme_name, 3);
					bool b = scm_to_bool(scm_cadddr(args));

					double d = (that->*method.d_hhtb)(h1,h2,t,b);
					rc = scm_from_double(d);
					break;
				}
				case H_H:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name));
					Handle rh((that->*method.h_h)(h));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case H_HH:
				{
					Handle h1(SchemeSmob::verify_handle(scm_car(args),
														scheme_name));
					Handle h2(SchemeSmob::verify_handle(scm_cadr(args),
														scheme_name, 2));
					Handle rh((that->*method.h_hh)(h1, h2));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case H_HHH:
				{
					Handle h1(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));
					Handle h2(SchemeSmob::verify_handle(scm_cadr(args), scheme_name, 2));
					Handle h3(SchemeSmob::verify_handle(scm_caddr(args), scheme_name, 3));
					Handle rh((that->*method.h_hhh)(h1, h2, h3));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case H_HHHH:
				{
					Handle h1(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));
					Handle h2(SchemeSmob::verify_handle(scm_cadr(args), scheme_name, 2));
					Handle h3(SchemeSmob::verify_handle(scm_caddr(args), scheme_name, 3));
					Handle h4(SchemeSmob::verify_handle(scm_cadddr(args), scheme_name, 4));
					Handle rh((that->*method.h_hhhh)(h1, h2, h3, h4));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case H_HS:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args),
													   scheme_name));
					std::string s(SchemeSmob::verify_string(scm_cadr(args),
															scheme_name, 2));
					Handle rh((that->*method.h_hs)(h,s));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case H_HTQB:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));

					Type t = SchemeSmob::verify_atom_type(scm_cadr(args), scheme_name, 2);

					SCM list = scm_caddr(args);
					HandleSeq seq = SchemeSmob::verify_handle_list(list, scheme_name, 3);

					bool b = scm_to_bool(scm_cadddr(args));

					Handle rh((that->*method.h_htqb)(h, t, seq, b));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}

				case H_HZ:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));
					size_t sz = SchemeSmob::verify_size(scm_cadr(args), scheme_name, 2);
					Handle rh((that->*method.h_hz)(h,sz));
					rc = SchemeSmob::handle_to_scm(rh);
					break;
				}
				case I_V:
				{
					int i = (that->*method.i_v)();
					rc = scm_from_int(i);
					break;
				}
				case Q_HTIB:
				{
					// First arg is a handle
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name, 1));

					// Second arg is a type
					Type t = SchemeSmob::verify_atom_type(scm_cadr(args), scheme_name, 2);

					// Third arg is an int
					int i = SchemeSmob::verify_int(scm_caddr(args), scheme_name, 3);

					//Fourth arg is a bool
					bool b = scm_to_bool(scm_cadddr(args));

					HandleSeq rHS = (that->*method.q_htib)(h,t,i,b);
					HandleSeq::iterator it = rHS.begin();
					if (it != rHS.end())
						rc = scm_list_1(SchemeSmob::handle_to_scm(*it));
					++it;
					for ( ; it != rHS.end(); ++it)
					{
						rc = scm_cons(SchemeSmob::handle_to_scm(*it), rc);
					}
					break;
				}
				case K_H:
				{
					// the only argument is a handle
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name));
					HandleSeqSeq rHSS = (that->*method.k_h)(h);

					rc = SCM_EOL;

					// reverse iteration to preserve order when doing cons
					for (HandleSeqSeq::reverse_iterator rit = rHSS.rbegin(); rit != rHSS.rend(); ++rit)
					{
						HandleSeq rHS = *rit;
						SCM rcTemp = SCM_EOL;

						for (HandleSeq::reverse_iterator rrit = rHS.rbegin(); rrit != rHS.rend(); ++rrit)
							rcTemp = scm_cons(SchemeSmob::handle_to_scm(*rrit), rcTemp);

						rc = scm_cons(rcTemp, rc);
					}

					break;
				}
				case P_H:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name));
					TruthValuePtr tv((that->*method.p_h)(h));
					rc = SchemeSmob::tv_to_scm(tv);
					break;
				}
				case S_AS:
				{
					// First argument is an AtomSpace ptr.
					AtomSpace* as = SchemeSmob::verify_atomspace(scm_car(args), scheme_name, 1);
					// Second argument is a string
					std::string str(SchemeSmob::verify_string(scm_cadr(args), scheme_name, 2));

					std::string rs = (that->*method.s_as)(as, str);
					rc = scm_from_utf8_string(rs.c_str());
					break;
				}
				case S_S:
				{
					// First argument is a string
					std::string str(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));

					std::string rs = (that->*method.s_s)(str);
					rc = scm_from_utf8_string(rs.c_str());
					break;
				}
				case S_SS:
				{
					// All args are strings
					std::string str1(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));
					std::string str2(SchemeSmob::verify_string(scm_cadr(args), scheme_name, 2));

					std::string rs = (that->*method.s_ss)(str1, str2);
					rc = scm_from_utf8_string(rs.c_str());
					break;
				}
				case S_SSS:
				{
					// All args are strings
					std::string str1(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));
					std::string str2(SchemeSmob::verify_string(scm_cadr(args), scheme_name, 2));
					std::string str3(SchemeSmob::verify_string(scm_caddr(args), scheme_name, 3));

					std::string rs = (that->*method.s_sss)(str1, str2, str3);
					rc = scm_from_utf8_string(rs.c_str());
					break;
				}
				case S_V:
				{
					std::string rs = (that->*method.s_v)();
					rc = scm_from_utf8_string(rs.c_str());
					break;
				}
				case V_B:
				{
					bool b = scm_to_bool(scm_car(args));
					(that->*method.v_b)(b);
					break;
				}
				case V_I:
				{
					int i = SchemeSmob::verify_int(scm_car(args), scheme_name);

					(that->*method.v_i)(i);
					break;
				}
				case V_H:
				{
					Handle h(SchemeSmob::verify_handle(scm_car(args), scheme_name));
					(that->*method.v_h)(h);
					break;
				} 
				case V_S:
				{
					std::string str(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));

					(that->*method.v_s)(str);
					break;
				}
				case V_SA:
				{
					// First argument is a string
					std::string str(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));

					// Second argument is an AtomSpace
					AtomSpace* as = SchemeSmob::verify_atomspace(scm_cadr(args), scheme_name, 2);
					(that->*method.v_sa)(str, as);
					break;
				}
				case V_SS:
				{
					// All args are strings
					std::string str1(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));
					std::string str2(SchemeSmob::verify_string(scm_cadr(args), scheme_name, 2));

					(that->*method.v_ss)(str1, str2);
					break;
				}
				case V_SSS:
				{
					// All args are strings
					std::string str1(SchemeSmob::verify_string(scm_car(args), scheme_name, 1));
					std::string str2(SchemeSmob::verify_string(scm_cadr(args), scheme_name, 2));
					std::string str3(SchemeSmob::verify_string(scm_caddr(args), scheme_name, 3));

					(that->*method.v_sss)(str1, str2, str3);
					break;
				}
				case V_T:
				{
					Type t = SchemeSmob::verify_atom_type(scm_car(args), scheme_name, 1);
					(that->*method.v_t)(t);
					break;
				}
				case V_TI:
				{
					Type t = SchemeSmob::verify_atom_type(scm_car(args), scheme_name, 1);

					int i = SchemeSmob::verify_int(scm_cadr(args), scheme_name, 2);

					(that->*method.v_ti)(t, i);
					break;
				}
				case V_TIDI:
				{
					Type t = SchemeSmob::verify_atom_type(scm_car(args), scheme_name, 1);
					int i = SchemeSmob::verify_int(scm_cadr(args), scheme_name, 2);
					double d = SchemeSmob::verify_real(scm_caddr(args), scheme_name, 3);
					int i2 = SchemeSmob::verify_int(scm_cadddr(args), scheme_name, 4);
					(that->*method.v_tidi)(t, i, d, i2);
					break;
				}
				case V_V:
				{
					(that->*method.v_v)();
					break;
				}
				default:
					throw RuntimeException(TRACE_INFO,
						 "Unsupported signature: %d\n", signature);
			}
			return rc;
		}
	protected:
		virtual const char *get_name(void) { return scheme_name; }
		virtual const char *get_module(void) { return scheme_module; }
		virtual size_t get_size(void) { return sizeof (*this); }
	public:

#define DECLARE_CONSTR_0(SIG, LSIG, RET_TYPE) \
	SchemePrimitive(const char* module, const char* name, \
		 RET_TYPE (T::*cb)(void), T *data) \
	{ \
		that = data; \
		method.LSIG = cb; \
		scheme_module = module; \
		scheme_name = name; \
		signature = SIG; \
		do_register(module, name, 0); /* cb has 0 args */ \
	}

#define DECLARE_CONSTR_1(SIG, LSIG, RET_TYPE, ARG_TYPE) \
	SchemePrimitive(const char* module, const char* name, \
		 RET_TYPE (T::*cb)(ARG_TYPE), T *data) \
	{ \
		that = data; \
		method.LSIG = cb; \
		scheme_module = module; \
		scheme_name = name; \
		signature = SIG; \
		do_register(module, name, 1); /* cb has 1 arg */ \
	}

#define DECLARE_CONSTR_2(SIG, LSIG, RET_TYPE, ARG1_TYPE, ARG2_TYPE) \
	SchemePrimitive(const char* module, const char* name, \
		RET_TYPE (T::*cb)(ARG1_TYPE, ARG2_TYPE), T *data) \
	{ \
		that = data; \
		method.LSIG = cb; \
		scheme_module = module; \
		scheme_name = name; \
		signature = SIG; \
		do_register(module, name, 2); /* cb has 2 args */ \
	}

// XXX FIXME -- WTF!? This is deeply wrong and flawed and broken.
// There should not be any primitives with 3, 4 and 5 arguments!!
// Who added this crap?!??
#define DECLARE_CONSTR_3(SIG, LSIG, RET_TYPE, ARG1_TYPE, ARG2_TYPE, ARG3_TYPE) \
	SchemePrimitive(const char* module, const char* name, \
		 RET_TYPE (T::*cb)(ARG1_TYPE, ARG2_TYPE, ARG3_TYPE), T *data) \
	{ \
		that = data; \
		method.LSIG = cb; \
		scheme_module = module; \
		scheme_name = name; \
		signature = SIG; \
		do_register(module, name, 3); /* cb has 3 args */ \
	}

#define DECLARE_CONSTR_4(SIG, LSIG, RET_TYPE, ARG1_TYPE, ARG2_TYPE, ARG3_TYPE, ARG4_TYPE) \
	SchemePrimitive(const char* module, const char* name, \
		RET_TYPE (T::*cb)(ARG1_TYPE, ARG2_TYPE, ARG3_TYPE, ARG4_TYPE), T *data) \
	{ \
		that = data; \
		method.LSIG = cb; \
		scheme_module = module; \
		scheme_name = name; \
		signature = SIG; \
		do_register(module, name, 4); /* cb has 4 args */ \
	}

		// Declare and define the constructors for this class. They all have
		// the same basic form, except for the types.
// XXX FIXME This is all deeply bad and broken and wrong -- there
// should only be a few basic types; there should NOT be any wrappers
// for functions taking 3,4,5 arguments, and pretty much all functions
// should be taking only Handles as arguments, and pretty much nothing
// else.  All this other crap- returning doubles, and whatnot, is a
// gross abuse of the intended use of these wrappers!  The Scheme
// interfaces ARE NOT MEANT TO BE A GENERAL PURPOSE COMPUTING PLATFORM.
// YOU ARE DESIGNING YOUR SYSTEM WRONG IF YOU NEED MORE THAN A FEW
// ARGUMENTS, OR ARGUMENTS THAT ARE NOT HANDLES!!
// Wtf. Who does this shit, anyway. Fuck me.  This is total crap.
//
		DECLARE_CONSTR_1(B_B,    b_b,  bool, bool)
		DECLARE_CONSTR_2(B_HH,   b_hh, bool, Handle, Handle)
		DECLARE_CONSTR_4(D_HHTB, d_hhtb, double, Handle, Handle, Type, bool)
		DECLARE_CONSTR_1(H_H,    h_h,  Handle, Handle)
		DECLARE_CONSTR_2(H_HH,   h_hh, Handle, Handle, Handle)
		DECLARE_CONSTR_2(H_HS,   h_hs, Handle, Handle, const std::string&)
		DECLARE_CONSTR_4(H_HTQB, h_htqb, Handle, Handle, Type, const HandleSeq&, bool)
		DECLARE_CONSTR_3(H_HHH,  h_hhh, Handle, Handle, Handle, Handle)
		DECLARE_CONSTR_4(H_HHHH, h_hhhh, Handle, Handle, Handle, Handle, Handle)
		DECLARE_CONSTR_2(H_HZ,   h_hz, Handle, Handle, size_t)
		DECLARE_CONSTR_0(I_V,    i_v, int)
		DECLARE_CONSTR_4(Q_HTIB, q_htib, HandleSeq, Handle, Type, int, bool)
		DECLARE_CONSTR_1(K_H,    k_h,  HandleSeqSeq, Handle)
		DECLARE_CONSTR_2(S_AS,   s_as, std::string, AtomSpace*,
		                               const std::string&)
		DECLARE_CONSTR_1(S_S,    s_s,  std::string, const std::string&)
		DECLARE_CONSTR_2(S_SS,   s_ss, std::string, const std::string&,
		                               const std::string&)
		DECLARE_CONSTR_3(S_SSS,  s_sss, std::string, const std::string&,
		                                const std::string&, const std::string&)
		DECLARE_CONSTR_0(S_V,   s_v,  std::string)
		DECLARE_CONSTR_1(P_H,   p_h,  TruthValuePtr, Handle)
		DECLARE_CONSTR_1(V_B,   v_b,  void, bool)
		DECLARE_CONSTR_1(V_I,   v_i,  void, int)
		DECLARE_CONSTR_1(V_H,	v_h,  void, Handle)
		DECLARE_CONSTR_1(V_S,   v_s,  void, const std::string&)
		DECLARE_CONSTR_2(V_SA,  v_sa, void, const std::string&,
		                              AtomSpace*)
		DECLARE_CONSTR_2(V_SS,  v_ss, void, const std::string&,
		                              const std::string&)
		DECLARE_CONSTR_3(V_SSS, v_sss,void, const std::string&,
		                              const std::string&, const std::string&)
		DECLARE_CONSTR_1(V_T,	v_t,  void, Type)
		DECLARE_CONSTR_2(V_TI,  v_ti, void, Type, int)
		DECLARE_CONSTR_4(V_TIDI, v_tidi, void, Type, int, double, int)

		DECLARE_CONSTR_0(V_V,   v_v,  void);
};

#define DECLARE_DECLARE_1(RET,ARG) \
template<class T> \
inline void define_scheme_primitive(const char *name, RET (T::*cb)(ARG), T *data, const char* module = "extension") \
{ \
	/* Note: this is freed automatically by scheme garbage collection */ \
	/* when it is no longer needed. */ \
	new SchemePrimitive<T>(module, name, cb, data); \
}

#define DECLARE_DECLARE_2(RET,ARG1,ARG2) \
template<class T> \
inline void define_scheme_primitive(const char *name, RET (T::*cb)(ARG1,ARG2), T *data, const char* module = "extension") \
{ \
	/* Note: this is freed automatically by scheme garbage collection */ \
	/* when it is no longer needed. */ \
	new SchemePrimitive<T>(module, name, cb, data); \
}

#define DECLARE_DECLARE_3(RET,ARG1,ARG2,ARG3) \
template<class T> \
inline void define_scheme_primitive(const char *name, RET (T::*cb)(ARG1,ARG2,ARG3), T *data, const char* module = "extension") \
{ \
	/* Note: this is freed automatically by scheme garbage collection */ \
	/* when it is no longer needed. */ \
	new SchemePrimitive<T>(module, name, cb, data); \
}

#define DECLARE_DECLARE_4(RET,ARG1,ARG2,ARG3,ARG4) \
template<class T> \
inline void define_scheme_primitive(const char *name, RET (T::*cb)(ARG1,ARG2,ARG3,ARG4), T *data, const char* module = "extension") \
{ \
	/* Note: this is freed automatically by scheme garbage collection */ \
	/* when it is no longer needed. */ \
	new SchemePrimitive<T>(module, name, cb, data); \
}

DECLARE_DECLARE_1(bool, bool)
DECLARE_DECLARE_1(Handle, Handle)
DECLARE_DECLARE_1(HandleSeq, Handle)
DECLARE_DECLARE_1(HandleSeqSeq, Handle)
DECLARE_DECLARE_1(int, void)
DECLARE_DECLARE_1(std::string, const std::string&)
DECLARE_DECLARE_1(std::string, void)
DECLARE_DECLARE_1(TruthValuePtr, Handle)
DECLARE_DECLARE_1(void, bool)
DECLARE_DECLARE_1(void, int)
DECLARE_DECLARE_1(void, Handle)
DECLARE_DECLARE_1(void, const std::string&)
DECLARE_DECLARE_1(void, Type)
DECLARE_DECLARE_1(void, void)

DECLARE_DECLARE_2(bool, Handle, Handle)
DECLARE_DECLARE_2(Handle, Handle, Handle)
DECLARE_DECLARE_2(Handle, Handle, const std::string&)
DECLARE_DECLARE_2(Handle, Handle, size_t)
DECLARE_DECLARE_2(Handle, const std::string&, const HandleSeq&)
DECLARE_DECLARE_2(std::string, AtomSpace*, const std::string&)
DECLARE_DECLARE_2(std::string, const std::string&, const std::string&)
DECLARE_DECLARE_2(void, const std::string&, AtomSpace*)
DECLARE_DECLARE_2(void, const std::string&, const std::string&)
DECLARE_DECLARE_2(void, Type, int)

DECLARE_DECLARE_3(Handle, Handle, Handle, Handle)
DECLARE_DECLARE_3(std::string, const std::string&,
                  const std::string&, const std::string&)
DECLARE_DECLARE_3(void, const std::string&,
                  const std::string&, const std::string&)

DECLARE_DECLARE_4(double, Handle, Handle, Type, bool)
DECLARE_DECLARE_4(Handle, Handle, Type, const HandleSeq&, bool)
DECLARE_DECLARE_4(HandleSeq, Handle, Type, int, bool)
DECLARE_DECLARE_4(void, Type, int, double, int)
DECLARE_DECLARE_4(Handle, Handle, Handle, Handle, Handle)
//** @}*/
}

#endif // _OPENCOG_SCHEME_PRIMITIVE_H

#endif // HAVE_GUILE
