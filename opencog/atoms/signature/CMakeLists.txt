
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (signature
	TypeChoice.cc
	TypeIntersectionLink.cc
	TypeNode.cc
	TypeUtils.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(signature opencog_atom_types)

TARGET_LINK_LIBRARIES(signature
	grant
	atomcore
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS signature EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	TypeChoice.h
	TypeIntersectionLink.h
	TypeNode.h
	TypeUtils.h
	DESTINATION "include/opencog/atoms/signature"
)
