/*
 * SchemePrimitive.h
 *
 * Allow C++ code to be invoked from scheme --
 * by creating a new scheme primitive function.
 *
 * Copyright (C) 2009,2015 Linas Vepstas
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

#ifdef HAVE_GUILE

#ifndef _OPENCOG_SCHEME_PRIMITIVE_H
#define _OPENCOG_SCHEME_PRIMITIVE_H

#include <string>
#include <tuple>
#include <utility>

#include <libguile.h>

#include <opencog/util/Logger.h>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/guile/SchemeSmob.h>

// Remove as soon as C++14 is enabled and gcc 4.9 or clang is a minimum requirement.
#if __cplusplus <= 201103L

namespace std {

#if defined(_GLIBCXX_STRING)
  // Copied/pasted from gcc 4.9 utility.
  /// Class template integer_sequence
  template<typename _Tp, _Tp... _Idx>
    struct integer_sequence
    {
      typedef _Tp value_type;
      static constexpr size_t size() { return sizeof...(_Idx); }
    };

  template<typename _Tp, _Tp _Num,
	   typename _ISeq = typename _Build_index_tuple<_Num>::__type>
    struct _Make_integer_sequence;

  template<typename _Tp, _Tp _Num,  size_t... _Idx>
    struct _Make_integer_sequence<_Tp, _Num, _Index_tuple<_Idx...>>
    {
      static_assert( _Num >= 0,
		     "Cannot make integer sequence of negative length" );

      typedef integer_sequence<_Tp, static_cast<_Tp>(_Idx)...> __type;
    };

  /// Alias template make_integer_sequence
  template<typename _Tp, _Tp _Num>
    using make_integer_sequence
      = typename _Make_integer_sequence<_Tp, _Num>::__type;

  /// Alias template index_sequence
  template<size_t... _Idx>
    using index_sequence = integer_sequence<size_t, _Idx...>;

  /// Alias template make_index_sequence
  template<size_t _Num>
    using make_index_sequence = make_integer_sequence<size_t, _Num>;

  /// Alias template index_sequence_for
  template<typename... _Types>
    using index_sequence_for = make_index_sequence<sizeof...(_Types)>;

#elif defined(_LIBCPP_VERSION)
	// Copied/pasted  from libc++ v1 utility
	template<class _Tp, _Tp... _Ip>
	struct _LIBCPP_TEMPLATE_VIS integer_sequence
	{
	    typedef _Tp value_type;
	    static_assert( is_integral<_Tp>::value,
	                  "std::integer_sequence can only be instantiated with an integral type" );
	    static
	    _LIBCPP_INLINE_VISIBILITY
	    constexpr
	    size_t
	    size() noexcept { return sizeof...(_Ip); }
	};

	template<size_t... _Ip>
	    using index_sequence = integer_sequence<size_t, _Ip...>;

	template <class _Tp, _Tp _Ep>
	using __make_integer_sequence = __make_integer_seq<integer_sequence, _Tp, _Ep>;

	template<class _Tp, _Tp _Np>
	    using make_integer_sequence = __make_integer_sequence<_Tp, _Np>;

	template<size_t _Np>
	    using make_index_sequence = make_integer_sequence<size_t, _Np>;

	template<class... _Tp>
	    using index_sequence_for = make_index_sequence<sizeof...(_Tp)>;
#endif

}
#endif

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

// Before being refactored with variadic templates this class used to
// enumerate all type signatures of all methods passed to
// SchemePrimitive ctor. This was ugly but had the advantage that it
// would discourage people from creating new signatures, in particular
// ones not using Handles. Indeed, in order to foster reflexivity we
// prefer functions that take Handles and return Handles, because they
// can then be called within atomese programs (using
// ExecutionOutputLink or EvaluationLink).
//
// The signatures were represented as (were + indicates one or more):
//
// X_X+
//
// the first X indicating the return type and the ones after the
// underscore indicating the input types, with the following type
// naming conventions:
//
// A == AtomSpace*
// B == bool
// D == double
// H == Handle
// I == int
// Q == HandleSeq
// K == HandleSeqSeq
// S == string
// P == TruthValuePtr
// T == Type
// V == void
// Z == size_t
//
// So for instance the following
//
// S_AS
//
// would indicate a method that takes an Atomspace* and string as
// first and second arguments, and return a string.
//
// Given that signature representation, below is a list of potential
// abuse of this API. Again it was never the intent that this would
// become a free-for-all, anything-goes dumping ground for badly
// designed API's. But that is what it has become. Can we reverse the
// decay?
//
// Here is a list of some of the users:
//
// B_B    -- cogutil logger boolean setters/getters.
// B_HH   -- PatternSCM::value_is_type(), etc.
//        -- LGDictSCM::do_lg_conn_type_match(), etc.
// H_HH   -- ?? someoene??
// H_HHH  -- pointmem (the 3D spatial API)
// H_HHHH -- do_backward_chaining, do_forward_chaining
// H_HT   -- fetch-incoming-by-type
// I_V    -- cogutil RandGen
// P_H    -- FunctionWrapper
// S_AS   -- CogServerSCM::start_server()
// S_S    -- cogutil logger API, see guile/LoggerSCM.h
// S_SS   -- DistSCM  (Gearman server)
// S_V    -- CogServerSCM::stop_server()
//        -- cogutil logger get_level(), etc.
// V_B    -- SQLPersistSCM:do_set_stall, PatternMiner
// V_I    -- cogutil do_randgen_set_seed
// V_II   -- SQLPersistSCM:do_set_hilo
// V_S    -- cogutil logger setters
//        -- SQLPersistSCM::do_open()
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
//
// This API needs to be re-thought, from scratch. Its offensive.
// S_AS   -- PatternMiner
// S_B    -- PatternMiner
// S_I    -- PatternMiner
// S_V    -- PatternMiner
// V_SI   -- PatternMiner

template<typename R, typename C, class... Args >
class SchemePrimitiveBase : public PrimitiveEnviron
{
	typedef R (C::*Method)(Args...);
	using PlainTuple = std::tuple<typename std::remove_reference<Args>::type...>;

	Method method;
	C* that;
	const char *scheme_module;
	const char *scheme_name;

public:
	SchemePrimitiveBase(const char* module, const char* name,
	                    R (C::*cb)(Args...), C *data)
	{
		that = data;
		method = cb;
		scheme_module = module;
		scheme_name = name;
		do_register(module, name, sizeof...(Args));
	}

protected:
	virtual const char *get_name(void) { return scheme_name; }
	virtual const char *get_module(void) { return scheme_module; }
	virtual size_t get_size(void) { return sizeof (*this); }

	// Convert the ith scm argument to its C++ object. The reason the
	// return type is also present as third argument is to tell gcc
	// what overloaded version of scm_to to use. It is a bit of a hack
	// but not that ugly given that the alternative is to use
	// specialized templates.
	SCM scm_to(SCM args, size_t idx, SCM) const
	{
		return scm_list_ref(args, scm_from_size_t(idx));
	}
	bool scm_to(SCM args, size_t idx, bool) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return scm_to_bool(arg);
	}
	size_t scm_to(SCM args, size_t idx, size_t) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_size(arg, scheme_name, idx);
	}
	int scm_to(SCM args, size_t idx, int) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_int(arg, scheme_name, idx);
	}
	double scm_to(SCM args, size_t idx, double) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_real(arg, scheme_name, idx);
	}
	std::string scm_to(SCM args, size_t idx, const std::string&) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_string(arg, scheme_name, idx);
	}
	Type scm_to(SCM args, size_t idx, Type) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_type(arg, scheme_name, idx);
	}
	Handle scm_to(SCM args, size_t idx, const Handle&) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_handle(arg, scheme_name, idx);
	}
	HandleSeq scm_to(SCM args, size_t idx, const HandleSeq&) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_handle_list(arg, scheme_name, idx);
	}
	AtomSpace* scm_to(SCM args, size_t idx, const AtomSpace*) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_atomspace(arg, scheme_name, idx);
	}
	TruthValuePtr scm_to(SCM args, size_t idx, const TruthValuePtr) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_tv(arg, scheme_name, idx);
	}
	ValuePtr scm_to(SCM args, size_t idx, const ValuePtr) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_protom(arg, scheme_name, idx);
	}
	Logger* scm_to(SCM args, size_t idx, const Logger*) const
	{
		SCM arg = scm_list_ref(args, scm_from_size_t(idx));
		return SchemeSmob::verify_logger(arg, scheme_name, idx);
	}

	// Get the Ith argument and convert it to a C++ object.
	template<std::size_t I>
	typename std::tuple_element<I, PlainTuple>::type
		get_conv(SCM args, const PlainTuple& t)
	{
		return scm_to(args, I, std::get<I>(t));
	}

	// Convert the scm arguments into their C++ objects, and call the
	// method over them.
	template<size_t ...S>
	R conv_call_method(SCM args, std::index_sequence<S...>)
	{
		return (that->*method)(get_conv<S>(args, PlainTuple())...);
	}

	// Like invoke but return the C++ type instead of its SCM conversion
	R cpp_invoke(SCM args)
	{
		// Call the method over the scm arguments. The index sequence
		// is used to access the argument types.
		return conv_call_method(args, std::index_sequence_for<Args...>{});
	}
};

// General case when R is non-void
template<typename R, typename C, class... Args>
class SchemePrimitive : public SchemePrimitiveBase<R, C, Args...>
{
	typedef SchemePrimitiveBase<R, C, Args...> super;
public:
	SchemePrimitive(const char* module, const char* name,
	                R (C::*cb)(Args...), C *data)
		: super(module, name, cb, data) {}

protected:
	// Convert any type to SCM
	SCM scm_from(SCM scm) const
	{
		return scm;
	}
	SCM scm_from(bool b)
	{
		return b ? SCM_BOOL_T : SCM_BOOL_F;
	}
	SCM scm_from(int i)
	{
		return scm_from_int(i);
	}
	SCM scm_from(size_t i)
	{
		return scm_from_size_t(i);
	}
	SCM scm_from(double d)
	{
		return scm_from_double(d);
	}
	SCM scm_from(const std::string& s)
	{
		return scm_from_utf8_string(s.c_str());
	}
	SCM scm_from(const Handle& h)
	{
		return SchemeSmob::handle_to_scm(h);
	}
	SCM scm_from(const HandleSeq& hs)
	{
		SCM rc = SCM_EOL;

		// reverse iteration to preserve order when doing cons
		for (HandleSeq::const_reverse_iterator rit = hs.rbegin();
		     rit != hs.rend(); ++rit)
		{
			rc = scm_cons(SchemeSmob::handle_to_scm(*rit), rc);
		}
		return rc;
	}
	SCM scm_from(const HandleSeqSeq& hss)
	{
		SCM rc = SCM_EOL;

		// reverse iteration to preserve order when doing cons
		for (HandleSeqSeq::const_reverse_iterator rit = hss.rbegin();
		     rit != hss.rend(); ++rit)
		{
			const HandleSeq& hs = *rit;
			SCM rcTemp = SCM_EOL;

			for (HandleSeq::const_reverse_iterator rrit = hs.rbegin();
			     rrit != hs.rend(); ++rrit)
				rcTemp = scm_cons(SchemeSmob::handle_to_scm(*rrit), rcTemp);
			rc = scm_cons(rcTemp, rc);
		}
		return rc;
	}
	SCM scm_from(const TruthValuePtr& tv)
	{
		return SchemeSmob::protom_to_scm(ValueCast(tv));
	}
	SCM scm_from(const ValuePtr& pa)
	{
		return SchemeSmob::protom_to_scm(pa);
	}
	SCM scm_from(Logger* lg)
	{
		return SchemeSmob::logger_to_scm(lg);
	}

	virtual SCM invoke (SCM args)
	{
		return scm_from(super::cpp_invoke(args));
	}
};

// Special case when R is void
template<typename C, class... Args>
class SchemePrimitive<void, C, Args...> : public SchemePrimitiveBase<void, C, Args...>
{
	typedef SchemePrimitiveBase<void, C, Args...> super;
public:
	SchemePrimitive(const char* module, const char* name,
	                void (C::*cb)(Args...), C *data)
		: super(module, name, cb, data) {}

protected:

	virtual SCM invoke (SCM args)
	{
		super::cpp_invoke(args);
		return SCM_UNSPECIFIED;
	}
};

template<typename R, typename C, class... Args >
inline void define_scheme_primitive(const char *name,
                                    R (C::*method)(Args...),
                                    C *data,
                                    const char* module="extension")
{
	new SchemePrimitive<R, C, Args...>(module, name, method, data);
}

}

#endif // _OPENCOG_SCHEME_PRIMITIVE_H

#endif // HAVE_GUILE
