from libcpp.vector cimport vector
from libcpp.list cimport list as cpplist


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


# Basic wrapping for std::string conversion.
cdef extern from "<string>" namespace "std":
    cdef cppclass string:
        string()
        string(char *)
        char * c_str()
        int size()


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
        strength_t getMean()
        confidence_t getConfidence()
        count_t getCount()
        tv_ptr DEFAULT_TV()
        string toString()
        bint operator==(cTruthValue h)
        bint operator!=(cTruthValue h)

cdef extern from "opencog/truthvalue/SimpleTruthValue.h" namespace "opencog":
    cdef cppclass cSimpleTruthValue "opencog::SimpleTruthValue":
        cSimpleTruthValue(float, float)
        strength_t getMean()
        confidence_t getConfidence()
        count_t getCount()
        count_t confidenceToCount(float)
        confidence_t countToConfidence(float)
        tv_ptr DEFAULT_TV()
        string toString()
        bint operator==(cTruthValue h)
        bint operator!=(cTruthValue h)


# Basic OpenCog types
# ClassServer
ctypedef short Type

cdef extern from "opencog/atoms/base/ClassServer.h" namespace "opencog":
    cdef cppclass cClassServer "opencog::ClassServer":
        bint isNode(Type t)
        bint isLink(Type t)
        bint isA(Type t, Type t)

        bint isDefined(string typename)
        Type getType(string typename)
        string getTypeName(Type t)
        Type getNumberOfClasses()
    cdef cClassServer classserver()

cdef extern from "opencog/atoms/base/atom_types.h" namespace "opencog":
    cdef Type NOTYPE


# Atom
ctypedef public short av_type

cdef extern from "opencog/atoms/base/Link.h" namespace "opencog":
    pass

cdef extern from "opencog/atoms/base/Atom.h" namespace "opencog":
    cdef cppclass cAtom "opencog::Atom":
        cAtom()

        Type getType()
        int isNode()
        int isLink()

        string toString()
        string toShortString()

        output_iterator getIncomingSet(output_iterator)

        tv_ptr getTruthValue()
        void setTruthValue(tv_ptr tvp)

        output_iterator getIncomingSetByType(output_iterator, Type type, bint subclass)

        # Conditionally-valid methods. Not defined for all atoms.
        string getName()
        vector[cHandle] getOutgoingSet()


# Handle
cdef extern from "opencog/atoms/base/Handle.h" namespace "opencog":
    cdef cppclass cHandle "opencog::Handle":
        cHandle()
        cHandle(const cHandle&)
        
        cAtom* atom_ptr()
        string toString()
        string toShortString()

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

cdef extern from "opencog/attentionbank/AttentionBank.h" namespace "opencog":
    cdef cppclass cAttentionBank "opencog::AttentionBank":
        av_type get_sti(const cHandle&)
        av_type get_lti(const cHandle&)
        av_type get_vlti(const cHandle&)

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
