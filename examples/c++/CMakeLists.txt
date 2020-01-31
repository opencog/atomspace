INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_EXECUTABLE(basic
	basic.cc
)

TARGET_LINK_LIBRARIES(basic
	atomspace
)

# This is what the install should look like.
# INSTALL (TARGETS example DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
# INSTALL (FILES opencog/example.scm DESTINATION "${GUILE_SITE_DIR}/opencog")
