
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog as-config" "ATOMSPACE_TEST")

# The build order used here is loosely in terms of 
# dependencies: the later parts depend on, or may
# someday depend on the earlier parts.
#
ADD_SUBDIRECTORY (atoms)
ADD_SUBDIRECTORY (atomspace)
ADD_SUBDIRECTORY (eval)
ADD_SUBDIRECTORY (query)
ADD_SUBDIRECTORY (persist)

# Extension language support
IF (HAVE_GUILE)
	ADD_SUBDIRECTORY (guile)
	ADD_SUBDIRECTORY (scm)
	ADD_SUBDIRECTORY (matrix)
ENDIF (HAVE_GUILE)

IF (HAVE_CYTHON)
	ADD_SUBDIRECTORY (cython)
ENDIF (HAVE_CYTHON)

# Haskell bindings.
IF (HAVE_STACK)
	ADD_SUBDIRECTORY (haskell)
ENDIF (HAVE_STACK)

# OCaml bindings.
IF (HAVE_OCAML)
	ADD_SUBDIRECTORY (ocaml)
ENDIF (HAVE_OCAML)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-config.scm SCM_CONFIG TRUE)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/as-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME as-config.scm)
