ADD_CXXTEST(ApplyLinkUTest)
TARGET_LINK_LIBRARIES(ApplyLinkUTest execution atomspace)

LINK_LIBRARIES(execution smob atomspace)

ADD_CXXTEST(DefinedSchemaUTest)
ADD_CXXTEST(GroundedSchemaLocalUTest)
ADD_CXXTEST(MapLinkUTest)
