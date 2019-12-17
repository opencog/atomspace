
OPENCOG_ADD_ATOM_TYPES(atom_types.script
	atom_types.h
	atom_types.definitions
	atom_types.inheritance
	core_types.scm
	core_types.pyx)

ADD_CUSTOM_TARGET(opencog_atom_types DEPENDS atom_types.h
	atom_types.definitions atom_types.inheritance core_types.scm
	core_types.pyx)

# The goal of SET_SOURCE_FILE_PROPERTIES is to guarantee that the
# creation of `atom_types.h` is finished BEFORE the compilation of
# `NameServer.cc` starts. Similar concerns apply to the use of the
# pyx file, as well as the two C files (definitions, inheritance).
SET_SOURCE_FILES_PROPERTIES(atom_types.h PROPERTIES GENERATED TRUE)
SET_SOURCE_FILES_PROPERTIES(atom_types.definitions PROPERTIES GENERATED TRUE)
SET_SOURCE_FILES_PROPERTIES(atom_types.inheritance PROPERTIES GENERATED TRUE)
SET_SOURCE_FILES_PROPERTIES(core_types.pyx PROPERTIES GENERATED TRUE)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atom_types
	atom_types_init.cc
	NameServer.cc
	)

# Without this, parallel make will race and crap up the generated files.
# Actually, that's not true ... even with this, compilation of other
# units might start before the generation has finished.  Ugh.
ADD_DEPENDENCIES(atom_types opencog_atom_types)

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
