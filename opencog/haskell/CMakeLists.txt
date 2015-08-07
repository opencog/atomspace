ADD_LIBRARY (haskell-atomspace SHARED
    AtomSpace_CWrapper.cpp
)

ADD_DEPENDENCIES(haskell-atomspace atomspace)

TARGET_LINK_LIBRARIES(haskell-atomspace
	atomspace
)

ADD_CUSTOM_TARGET(haskell-atomspace-lib ALL
    DEPENDS haskell-atomspace
    COMMAND stack build --extra-lib-dirs=${CMAKE_CURRENT_BINARY_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

INSTALL (TARGETS haskell-atomspace
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
