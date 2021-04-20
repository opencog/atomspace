
LINK_LIBRARIES(
	atombase
	atomspace
)

ADD_CXXTEST(AtomUTest)
ADD_CXXTEST(NodeUTest)
ADD_CXXTEST(LinkUTest)
ADD_CXXTEST(ClassServerUTest)
ADD_CXXTEST(HandleUTest)

# Special unit test atom types, tested by the FactoryUTest
OPENCOG_ADD_ATOM_TYPES(
   test_types.script
   test_types.h
   test_types.definitions
   test_types.inheritance
   test_types.scm
   test_types.pyx
)
ADD_CUSTOM_TARGET(test_atom_types DEPENDS test_types.h)

# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (test-types SHARED
   test_types
   TestTypes
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(test-types test_atom_types)

TARGET_LINK_LIBRARIES(test-types
   ${ATOMSPACE_atomtypes_LIBRARY}
)

ADD_CXXTEST(FactoryUTest)
TARGET_LINK_LIBRARIES(FactoryUTest test-types)
