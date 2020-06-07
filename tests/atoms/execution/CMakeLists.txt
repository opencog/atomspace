LINK_LIBRARIES(execution smob atomspace)

ADD_CXXTEST(DefinedSchemaUTest)
ADD_CXXTEST(GroundedSchemaLocalUTest)
ADD_CXXTEST(MapLinkUTest)

TARGET_LINK_LIBRARIES(GroundedSchemaLocalUTest grounded)
