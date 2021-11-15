
OPENCOG_GEN_CPP_ATOMTYPES(atom_types.script
	atom_types.h
	atom_types.definitions
	atom_types.inheritance)

OPENCOG_GEN_SCM_ATOMTYPES(atom_types.script core_types.scm)
OPENCOG_GEN_PYTHON_ATOMTYPES(atom_types.script core_types.pyx)

# The target `opencog_atom_types` is satisfied when `atom_types.h`
# appears in the file system. This target is used in other
# CMakefiles to make sure that this directory is visited, first.
ADD_CUSTOM_TARGET(opencog_atom_types
	DEPENDS atom_types.h core_types.scm core_types.pyx)

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
	${CMAKE_CURRENT_BINARY_DIR}/atom_names.h
	atom_types.cc
	NameServer.h
	types.h
	DESTINATION "include/opencog/atoms/atom_types"
	)

# Install the auto-generated atom types as well
ADD_GUILE_MODULE (FILES
	${CMAKE_CURRENT_BINARY_DIR}/core_types.scm
	MODULE "opencog"
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/base"
	DEPENDS opencog_atom_types
	)
