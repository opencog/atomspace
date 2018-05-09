
LINK_LIBRARIES(
	atombase
	atomutils
	atomspaceutils
	atomspace
#	clearbox
)

IF(HAVE_GUILE)
	ADD_CXXTEST(AtomTableUTest)
	TARGET_LINK_LIBRARIES(AtomTableUTest smob clearbox)
ENDIF(HAVE_GUILE)

ADD_CXXTEST(ClassServerUTest)
ADD_CXXTEST(AtomUTest)
ADD_CXXTEST(NodeUTest)
ADD_CXXTEST(LinkUTest)
ADD_CXXTEST(TLBUTest)
ADD_CXXTEST(AtomSpaceUTest)
ADD_CXXTEST(AtomSpaceAsyncUTest)
ADD_CXXTEST(UseCountUTest)
ADD_CXXTEST(MultiSpaceUTest)
ADD_CXXTEST(RemoveUTest)
ADD_CXXTEST(ThreadSafeHandleMapUTest)

# The ValuationTable is no longer used or even built, so don't test it.
# ADD_CXXTEST(ValuationTableUTest)
