
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (foreign
	DatalogAST.cc
	ForeignAST.cc
	LispAST.cc
	LispASTParser.cc
	LispASTPrinter.cc
	SexprAST.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(foreign opencog_atom_types)

TARGET_LINK_LIBRARIES(foreign
	atombase
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS foreign EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	DatalogAST.h
	ForeignAST.h
	LispAST.h
	SexprAST.h
	DESTINATION "include/opencog/atoms/foreign"
)
