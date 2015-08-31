ADD_LIBRARY (haskell-atomspace SHARED
    AtomSpace_CWrapper.cpp
    PatternMatcher_CWrapper.cpp
)

ADD_DEPENDENCIES(haskell-atomspace atomspace query)

TARGET_LINK_LIBRARIES(haskell-atomspace
	atomspace
	query
	execution
)

ADD_CUSTOM_TARGET(haskell-atomspace-lib ALL
    DEPENDS haskell-atomspace
    COMMAND stack build --extra-lib-dirs=${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -DBIN_DIR=${CMAKE_CURRENT_BINARY_DIR} -P setupghci.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell bindings."
)

INSTALL (TARGETS haskell-atomspace
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
