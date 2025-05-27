IF (HAVE_GUILE)
	LINK_LIBRARIES(execution smob atomspace)

	ADD_CXXTEST(DefinedSchemaUTest)
ENDIF (HAVE_GUILE)
