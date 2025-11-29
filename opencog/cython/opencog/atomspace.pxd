from libcpp cimport bool
from libcpp.vector cimport vector
from libcpp.memory cimport shared_ptr
from libcpp.set cimport set as cpp_set
from libcpp.string cimport string
from cython.operator cimport dereference as deref

# Basic wrapping for back_insert_iterator conversion.
cdef extern from "<vector>" namespace "std":
    cdef cppclass output_iterator "back_insert_iterator<vector<opencog::Handle> >"
    cdef output_iterator back_inserter(vector[cHandle])

# Basic OpenCog types
# NameServer
ctypedef short Type

cdef extern from "opencog/atoms/atom_types/NameServer.h" namespace "opencog":
    cdef cppclass cNameServer "opencog::NameServer":
        bint isNode(Type t) nogil const
        bint isLink(Type t) nogil const
        bint isA(Type t, Type t) nogil const

        bint isDefined(string typename) nogil const
        Type getType(string typename) nogil const
        string getTypeName(Type t) nogil const
        Type getNumberOfClasses() nogil const

        bint beginTypeDecls(const char* module) nogil
        void endTypeDecls() nogil
        Type declType(const Type parent, const string& name) nogil

    cdef cNameServer nameserver() nogil

cdef extern from "opencog/atoms/atom_types/atom_types.h" namespace "opencog":
    cdef Type NOTYPE

# Value
cdef extern from "opencog/atoms/value/Value.h" namespace "opencog":
    cdef cppclass cValue "opencog::Value":
        Type get_type() nogil const
        bint is_atom() nogil const
        bint is_node() nogil const
        bint is_link() nogil const

        string to_string() nogil const
        string to_short_string() nogil const
        bint operator==(const cValue&) nogil const
        bint operator!=(const cValue&) nogil const

    ctypedef shared_ptr[cValue] cValuePtr "opencog::ValuePtr"

cdef class Value:
    cdef cValuePtr shared_ptr
    cdef inline cValuePtr get_c_value_ptr(self) nogil
    cdef inline cValue* get_c_raw_ptr(self) nogil

    @staticmethod
    cdef Value create(cValuePtr& ptr)


# ContentHash
ctypedef size_t ContentHash;

# Atom
cdef extern from "opencog/atoms/base/Atom.h" namespace "opencog":
    cdef cppclass cAtom "opencog::Atom" (cValue):
        cAtom()

        void setValue(const cHandle& key, const cValuePtr& value) nogil
        cValuePtr getValue(const cHandle& key) nogil const
        cpp_set[cHandle] getKeys() nogil const

        vector[cHandle] getIncomingSet() nogil const
        vector[cHandle] getIncomingSetByType(Type type) nogil const

        bool is_executable() nogil const
        cValuePtr execute() nogil except +

        string to_string() nogil const
        string to_short_string() nogil const
        string id_to_string() nogil const

        # Conditionally-valid methods. Not defined for all atoms.
        string get_name() nogil const
        vector[cHandle] getOutgoingSet() nogil const
        ContentHash get_hash() nogil const

        bool operator==(const cAtom&) nogil const
        bool operator<(const cAtom&) nogil const

        cAtomSpace* getAtomSpace() nogil const


    cdef cHandle handle_cast "HandleCast" (cValuePtr) nogil except +
    cdef cHandle truth_key() nogil

# Handle
cdef extern from "opencog/atoms/base/Handle.h" namespace "opencog":
    ctypedef shared_ptr[cAtom] cAtomPtr "opencog::AtomPtr"

    cdef cppclass cHandle "opencog::Handle" (cAtomPtr):
        cHandle()
        cHandle(const cHandle&)

        cAtom* atom_ptr() nogil

        bint operator==(cHandle h) nogil
        bint operator!=(cHandle h) nogil
        bint operator<(cHandle h) nogil
        bint operator>(cHandle h) nogil
        bint operator<=(cHandle h) nogil
        bint operator>=(cHandle h) nogil
        cHandle UNDEFINED

    cdef cppclass cHandleSeq "opencog::HandleSeq"

cdef class Atom(Value):
    cdef object _name
    cdef object _outgoing
    cdef object _atomspace
    cdef object _hash
    # Cython compiler complains that signature of the method should be
    # compatible with one from the parent class. It is the reason why we cannot
    # have Atom.create and Value.create at same time.
    @staticmethod
    cdef Atom createAtom(cHandle& handle)


cdef vector[cHandle] atom_list_to_vector(list lst);

# AtomSpace
cdef extern from "opencog/atomspace/AtomSpace.h" namespace "opencog":
    cdef cppclass cAtomSpace "opencog::AtomSpace":
        cHandle add_atom(cHandle handle) nogil except +

        cHandle xadd_node(Type t, string s) nogil except +
        cHandle xadd_link(Type t, vector[cHandle]) nogil except +

        cHandle xget_handle(Type t, string s) nogil const
        cHandle xget_handle(Type t, vector[cHandle]) nogil const

        cHandle set_value(cHandle h, cHandle key, cValuePtr value) nogil
        cHandle get_atom(cHandle & h) nogil const
        bint is_valid_handle(cHandle h) nogil const
        int get_size() nogil const
        string get_name() nogil const

        # ==== query methods ====
        # get by type
        void get_handles_by_type(vector[cHandle], Type t, bint subclass) nogil const

        void clear() nogil
        bint extract_atom(cHandle h, bint recursive) nogil

    ctypedef shared_ptr[cAtomSpace] cAtomSpacePtr "opencog::AtomSpacePtr"

    cdef cHandle createAtomSpace(cAtomSpace *parent) nogil
    cdef cHandle as_cast "AtomSpaceCast"(cAtomSpace *) nogil except +

cdef class AtomSpace(Atom):
    cdef object parent_atomspace

# The py_atom is used by the c++ PythonEval class to convert C++
# instances into python objects so that they can be handed as
# arguments to python functions called from C++.
#
# The py_value_ptr is used by the c++ PythonEval class to get a
# straight-up c++ ValuePtr with all the right reference counts, etc.
cdef object py_atom(cHandle h)

# Older cythons (before 2024) get compiler errors with the noexcept
# keyword. Newer cythons without it get nag notes about optimization.
# ubuntu-24.04 works; ubuntu-22.04 fails; debian-bookworm (2023) fails
# cdef cValuePtr py_value_ptr(object py_value) noexcept with gil
cdef cValuePtr py_value_ptr(object py_value) with gil

# The two below are used by cython code to work with the C++
# incstances flowing across the cython declarations.
cdef object AtomSpace_factoid(cHandle to_wrap)
cdef object create_python_value_from_c_value(const cValuePtr& value)


# BoolValue
cdef extern from "opencog/atoms/value/BoolValue.h" namespace "opencog":
    cdef cppclass cBoolValue "opencog::BoolValue":
        cBoolValue(bool value) nogil
        cBoolValue(const vector[bool]& values) nogil
        const vector[bool]& value() nogil const

    cdef shared_ptr[cBoolValue] c_createBoolValue_single "opencog::createBoolValue" (bool) nogil
    cdef shared_ptr[cBoolValue] c_createBoolValue_vector "opencog::createBoolValue" (const vector[bool]&) nogil


# FloatValue
cdef extern from "opencog/atoms/value/FloatValue.h" namespace "opencog":
    cdef cppclass cFloatValue "opencog::FloatValue":
        cFloatValue(double value) nogil
        cFloatValue(const vector[double]& values) nogil
        const vector[double]& value() nogil const

    cdef shared_ptr[cFloatValue] c_createFloatValue_single "opencog::createFloatValue" (double) nogil
    cdef shared_ptr[cFloatValue] c_createFloatValue_vector "opencog::createFloatValue" (const vector[double]&) nogil


# StringValue
cdef extern from "opencog/atoms/value/StringValue.h" namespace "opencog":
    cdef cppclass cStringValue "opencog::StringValue":
        cStringValue(const string& value) nogil
        cStringValue(const vector[string]& values) nogil
        const vector[string]& value() nogil const

    cdef shared_ptr[cStringValue] c_createStringValue_single "opencog::createStringValue" (const string&) nogil
    cdef shared_ptr[cStringValue] c_createStringValue_vector "opencog::createStringValue" (const vector[string]&) nogil


# LinkValue
cdef extern from "opencog/atoms/value/LinkValue.h" namespace "opencog":
    cdef cppclass cLinkValue "opencog::LinkValue":
        cLinkValue(const vector[cValuePtr]& values) nogil
        const vector[cValuePtr]& value() nogil const

    cdef shared_ptr[cLinkValue] c_createLinkValue "opencog::createLinkValue" (const vector[cValuePtr]&) nogil


# QueueValue
cdef extern from "opencog/util/concurrent_queue.h" namespace "opencog::concurrent_queue":
    cdef cppclass Canceled:
        pass

cdef extern from "opencog/atoms/value/QueueValue.h" namespace "opencog":
    cdef cppclass cQueueValue "opencog::QueueValue":
        cQueueValue()
        cQueueValue(const vector[cValuePtr]& values) nogil
        void open() nogil
        void close() nogil
        bint is_closed() nogil const
        void add(const cValuePtr&) nogil except +
        cValuePtr remove() nogil except +
        size_t size() nogil const
        void clear() nogil
        const vector[cValuePtr]& value() nogil const

    cdef shared_ptr[cQueueValue] c_createQueueValue_empty "opencog::createQueueValue" () nogil
    cdef shared_ptr[cQueueValue] c_createQueueValue_vector "opencog::createQueueValue" (const vector[cValuePtr]&) nogil


# UnisetValue
cdef extern from "opencog/atoms/value/UnisetValue.h" namespace "opencog":
    cdef cppclass cUnisetValue "opencog::UnisetValue":
        cUnisetValue()
        cUnisetValue(const vector[cValuePtr]& values) nogil
        void open() nogil
        void close() nogil
        bint is_closed() nogil const
        void add(const cValuePtr&) nogil except +
        cValuePtr remove() nogil except +
        size_t size() nogil const
        void clear() nogil
        const vector[cValuePtr]& value() nogil const

    cdef shared_ptr[cUnisetValue] c_createUnisetValue_empty "opencog::createUnisetValue" () nogil
    cdef shared_ptr[cUnisetValue] c_createUnisetValue_vector "opencog::createUnisetValue" (const vector[cValuePtr]&) nogil


# VoidValue
cdef extern from "opencog/atoms/value/VoidValue.h" namespace "opencog":
    cdef cppclass cVoidValue "opencog::VoidValue":
        pass
    cValuePtr c_createVoidValue "opencog::createVoidValue" [T]() nogil
