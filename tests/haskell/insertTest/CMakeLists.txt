ADD_CUSTOM_TARGET(haskell-atomspace-test
    DEPENDS atomspace-cwrapper
    COMMAND sh buildTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell Test-Suite."
)

ADD_DEPENDENCIES(tests haskell-atomspace-test)

ADD_TEST(NAME HaskellAtomSpace
    COMMAND sh runTest.sh "${PROJECT_BINARY_DIR}/opencog/haskell"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
