IF (HAVE_ZMQ)

ADD_SUBDIRECTORY (atomspace)
set(atomspace_libs ${atomspace_libs} PARENT_SCOPE)

ENDIF (HAVE_ZMQ)
