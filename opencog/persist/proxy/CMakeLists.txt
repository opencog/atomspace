
# The storage_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (persist-proxy
	PassThruProxy.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(persist-proxy storage_atom_types)

TARGET_LINK_LIBRARIES(persist-proxy
	persist
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

# Arghhhh. I hate this stuff. This sets up a file-load path that the
# `(opencog persist)` module will use.  There is no `(opencog persist-proxy)`
# module.
ADD_GUILE_EXTENSION(SCM_CONFIG persist-proxy "opencog-ext-path-persist-proxy")

INSTALL (TARGETS persist-proxy EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FetchValueOfLink.h
	StoreValueOfLink.h
	DESTINATION "include/opencog/persist/flow"
)
