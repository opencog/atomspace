if(HAVE_CYTHON)
	INCLUDE_DIRECTORIES(
        	${PYTHON_INCLUDE_DIRS}
        	${CMAKE_CURRENT_SOURCE_DIR}
        	${CMAKE_CURRENT_BINARY_DIR})
endif(HAVE_CYTHON)

ADD_LIBRARY (truthvalue
	CountTruthValue.cc
	EvidenceCountTruthValue.cc
	FuzzyTruthValue.cc
	IndefiniteTruthValue.cc
	ProbabilisticTruthValue.cc
	SimpleTruthValue.cc
	TruthValue.cc
)

if(HAVE_CYTHON)
   set_property(TARGET truthvalue
           APPEND PROPERTY SOURCES
	   TensorTruthValue.cc)
endif(HAVE_CYTHON)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(truthvalue opencog_atom_types)

TARGET_LINK_LIBRARIES(truthvalue
	value
	${COGUTIL_LIBRARY}
)
if(HAVE_CYTHON)
	TARGET_LINK_LIBRARIES(truthvalue
	        ${PYTHON_LIBRARIES}
	)
endif(HAVE_CYTHON)



INSTALL (TARGETS truthvalue EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	CountTruthValue.h
	FuzzyTruthValue.h
	IndefiniteTruthValue.h
	ProbabilisticTruthValue.h
	SimpleTruthValue.h
	EvidenceCountTruthValue.h
	TruthValue.h
	TensorTruthValue.h
	DESTINATION "include/opencog/atoms/truthvalue"
)
