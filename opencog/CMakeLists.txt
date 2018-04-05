
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

# The build order used here is loosely in terms of 
# dependencies: the later parts depend on, or may
# someday depend on the earlier parts.
#
IF (HAVE_ATOMSPACE)
	ADD_SUBDIRECTORY (atoms)
	ADD_SUBDIRECTORY (atomspace)
	ADD_SUBDIRECTORY (atomspaceutils)
	ADD_SUBDIRECTORY (atomutils)
	ADD_SUBDIRECTORY (attentionbank)
	ADD_SUBDIRECTORY (eval)
	ADD_SUBDIRECTORY (query)
	ADD_SUBDIRECTORY (persist)
	ADD_SUBDIRECTORY (rule-engine)
	ADD_SUBDIRECTORY (truthvalue)
	ADD_SUBDIRECTORY (unify)
ENDIF (HAVE_ATOMSPACE)

# Extension language support
IF (HAVE_ATOMSPACE AND HAVE_GUILE)
	ADD_SUBDIRECTORY (guile)
	ADD_SUBDIRECTORY (scm)
	ADD_SUBDIRECTORY (matrix)
	ADD_SUBDIRECTORY (sheaf)
ENDIF (HAVE_ATOMSPACE AND HAVE_GUILE)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

# Haskell bindings.
ADD_SUBDIRECTORY (haskell)
