
ADD_TEST(NAME HaskellAtomSpace
    COMMAND sh build.sh "${PROJECT_BINARY_DIR}/opencog/haskell"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
