INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (opencl-atoms
	OpenclFloatValue.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(opencl-atoms opencl_types)

TARGET_LINK_LIBRARIES(opencl-atoms
	opencl-types
)

INSTALL (TARGETS opencl-atoms EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	OpenclFloatValue.h
	DESTINATION "include/opencog/opencl/stream"
)
