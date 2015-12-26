ADD_CUSTOM_TARGET(haskell-atomspace-executiontest
    DEPENDS atomspace-cwrapper
    COMMAND sh buildTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell" "${CMAKE_CURRENT_SOURCE_DIR}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell Execution-Test."
)

#The Code does not actually depend on haskell-atomspace-test
#but we can't build both at the same time
ADD_DEPENDENCIES(haskell-atomspace-executiontest haskell-atomspace-test)

ADD_DEPENDENCIES(tests haskell-atomspace-executiontest)

ADD_TEST(NAME HaskellAtomSpaceExecution
    COMMAND sh runTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell" "${CMAKE_CURRENT_SOURCE_DIR}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
