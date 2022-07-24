#
# Demo chemistry apps.
#
# The chem_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_EXECUTABLE(cpp-types-demo
   chemain
)

TARGET_LINK_LIBRARIES(cpp-types-demo
	${OPENCOG_atomspace_LIRARIES}
	chem-demo-types
	atomspace
)
