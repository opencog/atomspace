
LINK_LIBRARIES(
	ruleengine
	atomspace
	clearbox
)

ADD_CXXTEST(ForwardChainerUTest)
ADD_CXXTEST(BackwardChainerUTest)
ADD_CXXTEST(URECommonsUTest)
ADD_CXXTEST(UREConfigReaderUTest)
ADD_CXXTEST(DefaultForwardChainerCBUTest)
ADD_CXXTEST(FCMemoryUTest)

CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/tests/rule-engine/test_cpolicy.json
               ${PROJECT_BINARY_DIR}/tests/rule-engine/test_cpolicy.json)
