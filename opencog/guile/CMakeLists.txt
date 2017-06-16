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
	SchemeSmobLogger.cc
)

ADD_LIBRARY (logger
	LoggerSCM.cc
)

ADD_LIBRARY (randgen
	RandGenSCM.cc
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
    ruleengine # for the ure logger
)

TARGET_LINK_LIBRARIES(randgen
	smob
	${GUILE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

# XXX FIXME: We should also be installing `ln -s` links into one of
# these eight locations:
#    /usr/lib/guile/2.0/extensions
#    /usr/local/lib/guile/2.0/extensions
#    /usr/lib64/guile/2.0/extensions
#    /usr/local/lib64/guile/2.0/extensions
#    /usr/lib/guile/2.2/extensions
#    /usr/local/lib/guile/2.2/extensions
#    /usr/lib64/guile/2.2/extensions
#    /usr/local/lib64/guile/2.2/extensions
# ... but which one?  what a pain.
INSTALL (TARGETS smob DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS logger DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")
INSTALL (TARGETS randgen DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	SchemeEval.h
	SchemeModule.h
	SchemePrimitive.h
	SchemeSmob.h
	DESTINATION "include/opencog/guile"
)
