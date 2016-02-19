ADD_LIBRARY (atomspace-cwrapper
    AtomSpace_CWrapper.cpp
    PatternMatcher_CWrapper.cpp
    Exec_CWrapper.cpp
    Utils_CWrapper.cpp
)

ADD_DEPENDENCIES(atomspace-cwrapper atomspace query atoms)

TARGET_LINK_LIBRARIES(atomspace-cwrapper
	atomspace
	query
	execution
)

ADD_CUSTOM_TARGET(haskell-atomspace-lib ALL
    DEPENDS atomspace-cwrapper
    COMMAND sh build.sh ${CMAKE_CURRENT_BINARY_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Haskell bindings."
)

INSTALL (TARGETS atomspace-cwrapper
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
