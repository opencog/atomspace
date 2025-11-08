INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (value
	Value.cc
	BoolValue.cc
	ContainerValue.cc
	Float32Value.cc
	FlatStream.cc
	FloatValue.cc
	FormulaStream.cc
	FutureStream.cc
	LinkValue.cc
	QueueValue.cc
	RandomStream.cc
	SectionValue.cc
	SortedStream.cc
	StringValue.cc
	UnisetValue.cc
	ValueFactory.cc
	VoidValue.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(value opencog_atom_types)

TARGET_LINK_LIBRARIES(value
	atom_types
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS value EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	BoolValue.h
	ContainerValue.h
	FlatStream.h
	Float32Value.h
	FloatValue.h
	FormulaStream.h
	FutureStream.h
	LinkValue.h
	QueueValue.h
	RandomStream.h
	SectionValue.h
	SortedStream.h
	StringValue.h
	UnisetValue.h
	Value.h
	ValueFactory.h
	VoidValue.h
	DESTINATION "include/opencog/atoms/value"
)
