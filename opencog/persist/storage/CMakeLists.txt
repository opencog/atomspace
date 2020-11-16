
# --------------------------------------------------
OPENCOG_ADD_ATOM_TYPES(storage_types.script
	storage_types.h
	storage_types.definitions
	storage_types.inheritance
	storage_types.scm
	storage_types.pyx)
ADD_CUSTOM_TARGET(storage_types DEPENDS storage_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY(storage-types SHARED
	storage_types_init.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(storage-types storage_types opencog_atom_types)

TARGET_LINK_LIBRARIES(storage-types
	${ATOMSPACE_atomtypes_LIBRARY}
)

INSTALL (TARGETS storage-types
	EXPORT AtomSpaceTargets
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	${CMAKE_CURRENT_BINARY_DIR}/storage_types.h
	DESTINATION "include/opencog/persist/storage"
)

# Install the auto-generated atom types as well
ADD_GUILE_MODULE (FILES
	${CMAKE_CURRENT_BINARY_DIR}/storage_types.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/persist/types"
	DEPENDS storage_types
)
ADD_GUILE_EXTENSION(SCM_CONFIG storage-types "opencog-ext-path-storage-types")
