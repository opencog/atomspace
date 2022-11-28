
# The storage_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (persist-flow
	FetchValueOfLink.cc
	StoreValueOfLink.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(persist-flow opencog_atom_types)

TARGET_LINK_LIBRARIES(persist-flow
	persist
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS persist-flow EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FetchValueOfLink.h
	StoreValueOfLink.h
	DESTINATION "include/opencog/persist/flow"
)
