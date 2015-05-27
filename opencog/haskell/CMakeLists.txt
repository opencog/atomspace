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

INSTALL (FILES
    AtomSpace_CWrapper.h
	DESTINATION "include/${PROJECT_NAME}/atomspace_cwrapper"
)

