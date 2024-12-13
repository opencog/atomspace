
from libcpp cimport bool
from opencog.atomspace cimport cValuePtr, cHandle, cAtomSpace

cdef extern from "PersistCython.h" namespace "opencog":
# cdef extern from "PersistCython.h" :
	cdef void storage_open(const cHandle&)
	cdef void storage_close(const cHandle&)
	cdef bool storage_connected(const cHandle&)

	cdef cHandle dflt_fetch_atom(const cHandle&)
	cdef cHandle dflt_store_atom(const cHandle&)

	cdef cHandle dflt_fetch_value(const cHandle&, const cHandle&)
	cdef cHandle dflt_fetch_incoming_set(const cHandle&)
#	cdef cHandle dflt_fetch_incoming_by_type(const cHandle&, cType t)
	cdef cHandle dflt_fetch_query2(const cHandle& query, const cHandle& key)
	cdef cHandle dflt_fetch_query4(const cHandle& query, const cHandle& key,
                                cHandle meta, bool fresh)
	cdef void dflt_store_value(const cHandle& h, const cHandle& key)
	cdef void dflt_update_value(const cHandle& h, const cHandle& key, cValuePtr delta)
#	cdef void dflt_load_type(Type t)
	cdef void dflt_load_atomspace(const cHandle& space)
	cdef void dflt_store_atomspace(const cHandle& space)
#	cdef cHandleSeq dflt_load_frames()
	cdef void dflt_store_frames(const cHandle& has)
	cdef void dflt_delete_frame(const cHandle& has)
	cdef bool dflt_delete(const cHandle& h)
	cdef bool dflt_delete_recursive(const cHandle& h)
	cdef void dflt_barrier()
	cdef void dflt_erase()
	cdef void dflt_proxy_open()
	cdef void dflt_proxy_close()
	cdef void dflt_set_proxy(const cHandle& h)
#	cdef std::string dflt_monitor()
	cdef cHandle current_storage()
