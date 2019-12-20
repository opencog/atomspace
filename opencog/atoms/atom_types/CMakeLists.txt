
OPENCOG_ADD_ATOM_TYPES(atom_types.script
	atom_types.h
	atom_types.definitions
	atom_types.inheritance
	core_types.scm
	core_types.pyx)

# The target `opencog_atom_types` is satisfied when `atom_types.h`
# appears in the file system. This target is used in other
# CMakefiles to make sure that this directory is visited, first.
ADD_CUSTOM_TARGET(opencog_atom_types DEPENDS atom_types.h)

# For some obscure reason, CMake cannot figure out that these source
# files depend on `atom_types.h`, and tries to compile them before
# the `atom_types.h` file has been created. This becomes particularly
# bad during parallel make, which races with the creation of
# `atom_types.h`.
SET_SOURCE_FILES_PROPERTIES(
	atom_types_init.cc
	NameServer.cc
	DEPENDS opencog_atom_types)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atom_types
	atom_types_init.cc
	NameServer.cc
	)

TARGET_LINK_LIBRARIES(atom_types
	${COGUTIL_LIBRARY}
	)

INSTALL (TARGETS atom_types EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
	)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/atom_types.h
	atom_types.cc
	NameServer.h
	types.h
	DESTINATION "include/opencog/atoms/atom_types"
	)

# Install the auto-generated atom types as well
ADD_GUILE_MODULE (FILES
	${CMAKE_CURRENT_BINARY_DIR}/core_types.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/atoms/atom_types"
	DEPENDS opencog_atom_types
	)
