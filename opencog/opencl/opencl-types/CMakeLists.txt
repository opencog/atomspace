
# --------------------------------------------------

OPENCOG_GEN_CXX_ATOMTYPES(opencl_types.script
	opencl_types.h
	opencl_types.definitions
	opencl_types.inheritance)

OPENCOG_GEN_SCM_ATOMTYPES(opencl_types.script opencl_types.scm)
OPENCOG_GEN_PYTHON_ATOMTYPES(opencl_types.script opencl_types.pyx)

ADD_CUSTOM_TARGET(opencl_types
	DEPENDS opencl_types.h opencl_types.scm opencl_types.pyx)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY(opencl-types SHARED
	opencl_types_init.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(opencl-types opencl_types opencog_atom_types)

TARGET_LINK_LIBRARIES(opencl-types
	${ATOMSPACE_atomtypes_LIBRARY}
)

INSTALL (TARGETS opencl-types
	EXPORT AtomSpaceTargets
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/opencl_types.h
	DESTINATION "include/opencog/opencl"
)

# Install the auto-generated atom types as well
ADD_GUILE_MODULE (FILES
	${CMAKE_CURRENT_BINARY_DIR}/opencl_types.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/opencl/types"
	DEPENDS opencl_types
)
ADD_GUILE_EXTENSION(SCM_CONFIG opencl-types "opencog-ext-path-opencl-types")
