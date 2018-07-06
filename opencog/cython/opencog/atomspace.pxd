from libcpp.vector cimport vector
from libcpp.list cimport list as cpplist
from libcpp.memory cimport shared_ptr
from libcpp.string cimport string

cdef extern from "Python.h":
    # Tacky hack to pass atomspace pointer to AtomSpace ctor.
    cdef void* PyLong_AsVoidPtr(object)

    # Needed to return truth value pointers to C++ callers.
    cdef object PyLong_FromVoidPtr(void *p)

ctypedef public long PANDLE

cdef extern from "opencog/cython/opencog/Cast.h":
    # Tacky hack to pass atom pointer to Atom ctor.
    cdef cHandle atom_from_the_void(long p)

    # Tacky hack to convert C objects into Python objects.
    cdef PANDLE   void_from_candle(const cHandle& h)
    cdef PANDLE   void_from_cptr(cHandle* hp)


# Basic wrapping for back_insert_iterator conversion.
cdef extern from "<vector>" namespace "std":
    cdef cppclass output_iterator "back_insert_iterator<vector<opencog::Handle> >"
    cdef output_iterator back_inserter(vector[cHandle])


### TruthValue
ctypedef double count_t
ctypedef float confidence_t
ctypedef float strength_t

cdef extern from "opencog/truthvalue/TruthValue.h" namespace "opencog":
    cdef cppclass tv_ptr "std::shared_ptr<const opencog::TruthValue>":
        tv_ptr()
        tv_ptr(tv_ptr copy)
        tv_ptr(cTruthValue* fun)
        tv_ptr(cSimpleTruthValue* fun)
        cTruthValue* get()

    cdef cppclass cTruthValue "const opencog::TruthValue":
        strength_t get_mean()
        confidence_t get_confidence()
        count_t get_count()
        tv_ptr DEFAULT_TV()
        string to_string()
        bint operator==(cTruthValue h)
        bint operator!=(cTruthValue h)

cdef extern from "opencog/truthvalue/SimpleTruthValue.h" namespace "opencog":
    cdef cppclass cSimpleTruthValue "opencog::SimpleTruthValue":
        cSimpleTruthValue(float, float)
        strength_t get_mean()
        confidence_t get_confidence()
        count_t get_count()
        count_t confidenceToCount(float)
        confidence_t countToConfidence(float)
        tv_ptr DEFAULT_TV()
        string to_string()
        bint operator==(cTruthValue h)
        bint operator!=(cTruthValue h)


# Basic OpenCog types
# NameServer
ctypedef short Type

cdef extern from "opencog/atoms/proto/NameServer.h" namespace "opencog":
    cdef cppclass cNameServer "opencog::NameServer":
        bint isNode(Type t)
        bint isLink(Type t)
        bint isA(Type t, Type t)

        bint isDefined(string typename)
        Type getType(string typename)
        string getTypeName(Type t)
        Type getNumberOfClasses()
    cdef cNameServer nameserver()

cdef extern from "opencog/atoms/proto/atom_types.h" namespace "opencog":
    cdef Type NOTYPE

cdef extern from "opencog/atoms/proto/ProtoAtom.h" namespace "opencog":
    cdef cppclass cProtoAtom "opencog::ProtoAtom":
        Type get_type()
        bint is_atom()
        bint is_node()
        bint is_link()
        
        string to_string()
        string to_short_string()
        bint operator==(const cProtoAtom&)
        bint operator!=(const cProtoAtom&)
    
    ctypedef shared_ptr[cProtoAtom] cProtoAtomPtr "opencog::ProtoAtomPtr"

cdef class ProtoAtom:
    cdef cProtoAtomPtr shared_ptr

cdef ProtoAtom createProtoAtom(cProtoAtomPtr shared_ptr)

# Atom
ctypedef public short av_type

cdef extern from "opencog/atoms/base/Link.h" namespace "opencog":
    pass

cdef extern from "opencog/atoms/base/Atom.h" namespace "opencog":
    cdef cppclass cAtom "opencog::Atom" (cProtoAtom):
        cAtom()

        output_iterator getIncomingSet(output_iterator)

        tv_ptr getTruthValue()
        void setTruthValue(tv_ptr tvp)
        void setValue(const cHandle& key, const cProtoAtomPtr& value)
        cProtoAtomPtr getValue(const cHandle& key) const

        output_iterator getIncomingSetByType(output_iterator, Type type)

        # Conditionally-valid methods. Not defined for all atoms.
        string get_name()
        vector[cHandle] getOutgoingSet()


# Handle
cdef extern from "opencog/atoms/base/Handle.h" namespace "opencog":
    ctypedef shared_ptr[cAtom] cAtomPtr "opencog::AtomPtr"
    
    cdef cppclass cHandle "opencog::Handle" (cAtomPtr):
        cHandle()
        cHandle(const cHandle&)

        cAtom* atom_ptr()
        string to_string()
        string to_short_string()

        bint operator==(cHandle h)
        bint operator!=(cHandle h)
        bint operator<(cHandle h)
        bint operator>(cHandle h)
        bint operator<=(cHandle h)
        bint operator>=(cHandle h)
        cHandle UNDEFINED
# HandleSeq
    cdef cppclass cHandleSeq "opencog::HandleSeq"

cdef class TruthValue:
    cdef tv_ptr *cobj
    cdef _mean(self)
    cdef _confidence(self)
    cdef _count(self)
    cdef cTruthValue* _ptr(self)
    cdef tv_ptr* _tvptr(self)
    cdef _init(self, float mean, float count)

cdef class Atom:
    cdef cHandle *handle
    cdef AtomSpace atomspace
    cdef object _atom_type
    cdef object _name
    cdef object _outgoing



# AtomSpace

cdef extern from "opencog/atomspace/AtomSpace.h" namespace "opencog":
    cdef cppclass cAtomSpace "opencog::AtomSpace":
        AtomSpace()

        cHandle add_node(Type t, string s) except +
        cHandle add_node(Type t, string s, tv_ptr tvn) except +

        cHandle add_link(Type t, vector[cHandle]) except +
        cHandle add_link(Type t, vector[cHandle], tv_ptr tvn) except +

        cHandle get_handle(Type t, string s)
        cHandle get_handle(Type t, vector[cHandle])

        bint is_valid_handle(cHandle h)
        int get_size()

        # ==== query methods ====
        # get by type
        output_iterator get_handles_by_type(output_iterator, Type t, bint subclass)

        void clear()
        bint remove_atom(cHandle h, bint recursive)

cdef AtomSpace_factory(cAtomSpace *to_wrap)

cdef class AtomSpace:
    cdef cAtomSpace *atomspace
    cdef bint owns_atomspace

cdef extern from "opencog/attentionbank/AVUtils.h" namespace "opencog":
    cdef av_type get_sti(const cHandle&)
    cdef av_type get_lti(const cHandle&)
    cdef av_type get_vlti(const cHandle&)

cdef extern from "opencog/attentionbank/AttentionBank.h" namespace "opencog":
    cdef cppclass cAttentionBank "opencog::AttentionBank":
        void set_sti(const cHandle&, av_type stiValue)
        void set_lti(const cHandle&, av_type ltiValue)
        void inc_vlti(const cHandle&)
        void dec_vlti(const cHandle&)

        # get by STI range
        output_iterator get_handles_by_AV(output_iterator, short lowerBound, short upperBound)
        output_iterator get_handles_by_AV(output_iterator, short lowerBound)

        # get from AttentionalFocus
        output_iterator get_handle_set_in_attentional_focus(output_iterator)

    cdef cAttentionBank attentionbank(cAtomSpace*)


cdef extern from "opencog/atomutils/AtomUtils.h" namespace "opencog":
    # C++:
    #
    #   HandleSeq get_predicates(const Handle& target,
    #                     Type predicateType=PREDICATE_NODE,
    #                     bool subClasses=true)
    #   void finalize_opencog();
    #   void configuration_load(const char* configFile);
    #
    cdef vector[cHandle] c_get_predicates "get_predicates" (cHandle& target, Type t, bint subclass)
    cdef vector[cHandle] c_get_predicates_for "get_predicates_for" (cHandle& target, cHandle& predicate)

cdef extern from "opencog/atoms/proto/FloatValue.h" namespace "opencog":
    cdef cppclass cFloatValue "opencog::FloatValue":
        const vector[double]& value() const;
    
    cdef cProtoAtomPtr createFloatValue(...)

cdef extern from "opencog/atoms/proto/StringValue.h" namespace "opencog":
    cdef cppclass cStringValue "opencog::StringValue":
        const vector[string]& value() const;
    
    cdef cProtoAtomPtr createStringValue(...)

cdef extern from "opencog/atoms/proto/LinkValue.h" namespace "opencog":
    cdef cppclass cLinkValue "opencog::LinkValue":
        const vector[cProtoAtomPtr]& value() const;

    cdef cProtoAtomPtr createLinkValue(...)
