ADD_LIBRARY (haskell-atomspace SHARED
    AtomSpace_CWrapper.cpp
)

ADD_DEPENDENCIES(haskell-atomspace atomspace)

TARGET_LINK_LIBRARIES(haskell-atomspace
	atomspace
)

FILE(GLOB_RECURSE HASKELL_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*)

ADD_CUSTOM_TARGET(haskell-atomspace-lib ALL
    DEPENDS haskell-atomspace
            ${CMAKE_CURRENT_BINARY_DIR}/dist
)

ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/dist
    COMMAND ${CMAKE_COMMAND} -DBINDIR=${CMAKE_CURRENT_BINARY_DIR}
                             -P ${CMAKE_CURRENT_SOURCE_DIR}/InstallLib.cmake
    COMMENT "Installing: opencog-atomspace haskell library..."
    DEPENDS ${HASKELL_FILES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

INSTALL (TARGETS haskell-atomspace
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
