INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

#Compile
ADD_EXECUTABLE(crudexample
	CRUDExample
	)
TARGET_LINK_LIBRARIES(crudexample
	atomspace
	)

ADD_EXECUTABLE(ureexample
	UREExample
	)
TARGET_LINK_LIBRARIES(ureexample
	${COGUTIL_LIBRARY}
	atomspace 
	ruleengine
	)

IF(Boost_FOUND)
	ADD_EXECUTABLE(aseventsexample
		AtomSpaceEventSubscribeExample
		)
	TARGET_LINK_LIBRARIES(aseventsexample
		atomspace
		)
ENDIF(Boost_FOUND)
