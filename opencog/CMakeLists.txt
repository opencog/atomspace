
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog as-config" "ATOMSPACE_TEST")

# The build order used here is loosely in terms of 
# dependencies: the later parts depend on, or may
# someday depend on the earlier parts.
#
IF (HAVE_ATOMSPACE)
	ADD_SUBDIRECTORY (atoms)
	ADD_SUBDIRECTORY (atomspace)
	ADD_SUBDIRECTORY (atomspaceutils)
	ADD_SUBDIRECTORY (eval)
	ADD_SUBDIRECTORY (query)
	ADD_SUBDIRECTORY (persist)
	ADD_SUBDIRECTORY (ure)
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

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-config.scm SCM_CONFIG TRUE)

WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/as-config-installable.scm DESTINATION ${GUILE_SITE_DIR}/opencog RENAME as-config.scm)
