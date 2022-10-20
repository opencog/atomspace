
LINK_LIBRARIES(
	atombase
	atomspace
)

IF(HAVE_GUILE)
	ADD_CXXTEST(AtomTableUTest)
	TARGET_LINK_LIBRARIES(AtomTableUTest smob clearbox)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(HashMixUTest)
ADD_CXXTEST(AtomSpaceUTest)
ADD_CXXTEST(UseCountUTest)
ADD_CXXTEST(MultiSpaceUTest)
ADD_CXXTEST(COWSpaceUTest)
ADD_CXXTEST(RemoveUTest)
ADD_CXXTEST(ReAddUTest)

ADD_GUILE_TEST(CoverBasic cover-basic-test.scm)
ADD_GUILE_TEST(DeepSpace deep-space-test.scm)
ADD_GUILE_TEST(FrameIncoming frame-incoming-test.scm)
ADD_GUILE_TEST(CoverSpace cover-space-test.scm)
ADD_GUILE_TEST(CoverDelete cover-delete-test.scm)
ADD_GUILE_TEST(CoverBasicDelete cover-basic-delete-test.scm)
ADD_GUILE_TEST(RecoverTest recover-test.scm)
ADD_GUILE_TEST(RecoverStackTest recover-stack-test.scm)
