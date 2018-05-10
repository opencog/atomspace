INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

#Compile
ADD_EXECUTABLE(crudexample
	CRUDExample
	)
TARGET_LINK_LIBRARIES(crudexample
	atomspace
	attentionbank
	)

ADD_EXECUTABLE(ureexample
	UREExample
	)
TARGET_LINK_LIBRARIES(ureexample
	${COGUTIL_LIBRARY}
	atomspace 
	ruleengine
	)

ADD_EXECUTABLE(aseventsexample
    AtomSpaceEventSubscribeExample
    )
TARGET_LINK_LIBRARIES(aseventsexample
    atomspace
    attentionbank
    )
