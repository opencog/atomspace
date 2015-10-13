
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# The build order used here is loosely in terms of 
# dependencies: the later parts depend on, or may
# someday depend on the earlier parts.
#
ADD_SUBDIRECTORY (rule-engine)

IF (HAVE_ATOMSPACE)
    ADD_SUBDIRECTORY (atoms)
    ADD_SUBDIRECTORY (atomspace)
    ADD_SUBDIRECTORY (atomspaceutils)
    ADD_SUBDIRECTORY (atomutils)
    ADD_SUBDIRECTORY (benchmark)
    ADD_SUBDIRECTORY (query)
    ADD_SUBDIRECTORY (persist)
ENDIF (HAVE_ATOMSPACE)

# Extension language support
IF (HAVE_ATOMSPACE AND HAVE_GUILE)
	ADD_SUBDIRECTORY (guile)
	ADD_SUBDIRECTORY (scm)
ENDIF (HAVE_ATOMSPACE AND HAVE_GUILE)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)


# Haskell bindings.
IF (XHAVE_STACK)
    ADD_SUBDIRECTORY (haskell)
ENDIF (XHAVE_STACK)
