SET(CMAKE_SHARED_LIBRARY_SUFFIX .so)

ADD_SUBDIRECTORY (modules)

INCLUDE_DIRECTORIES(
	${GUILE_INCLUDE_DIR}
)

ADD_LIBRARY (smob
	SchemeEval.cc
	SchemeModule.cc
	SchemePrimitive.cc
	SchemeSmob.cc
	SchemeSmobAtom.cc
	SchemeSmobAS.cc
	SchemeSmobGC.cc
	SchemeSmobNew.cc
	SchemeSmobPrint.cc
	SchemeSmobTV.cc
	SchemeSmobValue.cc
	SchemeSmobLogger.cc
)

TARGET_LINK_LIBRARIES(smob

	# We want to list every possible library that defines some
	# atom type, so that the library ctors will run, and add those
	# atom types to the classserver. If this is not done, then the
	# classserver will fail to run atom factories, viz, will fail
	# to run the ctors for C++-backed atom types, resulting in
	# screwball failures.
	${NO_AS_NEEDED}
	clearbox
	join
	pattern
	execution
	atomflow
	atomcore
	foreign
	atombase
	truthvalue
	atomspace
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

ADD_GUILE_EXTENSION(SCM_CONFIG smob "opencog-ext-path-smob")

INSTALL (TARGETS smob
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	SchemeEval.h
	SchemeModule.h
	SchemePrimitive.h
	SchemeSmob.h
	DESTINATION "include/opencog/guile"
)
