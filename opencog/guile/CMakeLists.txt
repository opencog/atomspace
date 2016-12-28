INCLUDE_DIRECTORIES(
	${GUILE_INCLUDE_DIR}
)

ADD_LIBRARY (smob
	SchemeEval.cc
	SchemeModule.cc
	SchemePrimitive.cc
	SchemeSmob.cc
	SchemeSmobAtom.cc
	SchemeSmobAF.cc
	SchemeSmobAS.cc
	SchemeSmobAV.cc
	SchemeSmobGC.cc
	SchemeSmobNew.cc
	SchemeSmobTV.cc
	SchemeSmobValue.cc
)

ADD_LIBRARY (logger
	LoggerSCM.cc
)

TARGET_LINK_LIBRARIES(smob
	attentionbank
	atomspace
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

TARGET_LINK_LIBRARIES(logger
	smob
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS smob DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS logger DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	SchemeEval.h
	SchemeModule.h
	SchemePrimitive.h
	SchemeSmob.h
	DESTINATION "include/opencog/guile"
)
