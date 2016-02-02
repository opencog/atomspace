ADD_CUSTOM_TARGET(haskell-atomspace-test
    DEPENDS atomspace-cwrapper
    COMMAND sh buildTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell Test-Suite."
)

ADD_DEPENDENCIES(tests haskell-atomspace-test)

#The Code does not actually depend on haskell-atomspace-test
#but we can't build both at the same time
ADD_DEPENDENCIES(haskell-atomspace-test haskell-atomspace-executionlib)

ADD_TEST(NAME HaskellAtomSpace
    COMMAND sh runTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell" "${CMAKE_CURRENT_SOURCE_DIR}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
