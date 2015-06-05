ADD_LIBRARY (haskell-atomspace SHARED
    AtomSpace_CWrapper.cpp
)

ADD_DEPENDENCIES(haskell-atomspace atomspace)

TARGET_LINK_LIBRARIES(haskell-atomspace
	atomspace
)

INSTALL (TARGETS haskell-atomspace
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL(CODE
    "MESSAGE(STATUS \"Installing: opencog-atomspace haskell library...\")
     execute_process(
        COMMAND cabal install
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/opencog/haskell OUTPUT_QUIET
     )"
)

