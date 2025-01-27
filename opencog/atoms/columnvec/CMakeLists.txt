
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (columnvec
	FloatColumn.cc
	LinkColumn.cc
	SexprColumn.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(columnvec opencog_atom_types)

TARGET_LINK_LIBRARIES(columnvec
	atomcore
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS columnvec EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FloatColumn.h
	LinkColumn.h
	SexprColumn.h
	DESTINATION "include/opencog/atoms/columnvec"
)
