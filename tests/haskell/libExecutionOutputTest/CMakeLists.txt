ADD_CUSTOM_TARGET(haskell-atomspace-executiontest
    DEPENDS atomspace-cwrapper
    COMMAND sh buildTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell" "${CMAKE_CURRENT_SOURCE_DIR}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell Execution-Test."
)

ADD_DEPENDENCIES(tests haskell-atomspace-executiontest)

ADD_TEST(NAME HaskellAtomSpaceExecution
    COMMAND sh runTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell" "${CMAKE_CURRENT_SOURCE_DIR}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
