LINK_LIBRARIES(execution smob atomspace)

ADD_CXXTEST(DefinedSchemaUTest)

ADD_GUILE_TEST(ExecEvalTest exec-eval-test.scm)
