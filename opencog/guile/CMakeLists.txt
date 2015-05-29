INCLUDE_DIRECTORIES(
	${GUILE_INCLUDE_DIR}
)

ADD_LIBRARY (smob SHARED
	SchemeEval.cc
	SchemeModule.cc
	SchemePrimitive.cc
	SchemeSmob.cc
	SchemeSmobAtom.cc
	SchemeSmobAS.cc
	SchemeSmobAV.cc
	SchemeSmobGC.cc
	SchemeSmobNew.cc
	SchemeSmobTV.cc
	SchemeSmobAF.cc
	load-file
)

TARGET_LINK_LIBRARIES(smob
	atomspace
	${COGUTIL_LIBRARY}
	${GUILE_LIBRARIES}
)

INSTALL (TARGETS smob DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	SchemeEval.h
	SchemeModule.h
	SchemePrimitive.h
	SchemeSmob.h
	load-file.h
	DESTINATION "include/${PROJECT_NAME}/guile"
)
