
# Tests

ADD_GUILE_TEST(SexprQueryTest sexpr-query-test.scm)


# Comment out until someone cares.
# This works great, except that it depends on getting linked to
# code that is provided by the storage module, which is done only
# after the atomspace is built. So I think we need to move
# things to there .... including the crazy atom types ... which
# probably belong in their own crazy ast diretory.
#
# ADD_GUILE_TEST(DatalogBasicTest datalog-basic-test.scm)
