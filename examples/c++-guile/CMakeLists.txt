INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_EXECUTABLE(PrimitiveExample
	PrimitiveExample.cc
)

TARGET_LINK_LIBRARIES(PrimitiveExample
	smob
	atomspace
	clearbox
)

ADD_LIBRARY (example
	ExampleSCM.cc
)

TARGET_LINK_LIBRARIES(example
	smob
	atomspace
)

# This is what the install should look like.
# INSTALL (TARGETS example DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
# INSTALL (FILES opencog/example.scm DESTINATION "${GUILE_SITE_DIR}/opencog")
