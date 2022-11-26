
# The storage_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (persistflow
	FetchValueOfLink.cc
	StoreValueOfLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(persistflow opencog_atom_types)

TARGET_LINK_LIBRARIES(persistflow
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS persistflow EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FetchValueOfLink.h
	StoreValueOfLink.h
	DESTINATION "include/opencog/persist/flow"
)
