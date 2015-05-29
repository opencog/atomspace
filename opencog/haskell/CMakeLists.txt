ADD_LIBRARY (atomspace_wrapper SHARED
    AtomSpace_CWrapper.cpp
)

ADD_DEPENDENCIES(atomspace_wrapper atomspace)

TARGET_LINK_LIBRARIES(atomspace_wrapper
	atomspace
)

INSTALL (TARGETS atomspace_wrapper
	LIBRARY DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL(CODE
     "execute_process(COMMAND cabal install   WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/opencog/haskell OUTPUT_QUIET)"
)

