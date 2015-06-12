ADD_LIBRARY (haskell-atomspace SHARED
    AtomSpace_CWrapper.cpp
)

ADD_DEPENDENCIES(haskell-atomspace atomspace)

TARGET_LINK_LIBRARIES(haskell-atomspace
	atomspace
)

ADD_CUSTOM_TARGET(haskell-atomspace-env ALL
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/CheckEnv.cmake
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/opencog/haskell
)

INSTALL (TARGETS haskell-atomspace
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL(CODE
    "MESSAGE(STATUS \"Installing: opencog-atomspace haskell library...\")
     execute_process(
        COMMAND cabal install --global
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/opencog/haskell OUTPUT_QUIET
     )"
)

