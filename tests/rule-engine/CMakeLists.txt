ADD_SUBDIRECTORY (forwardchainer)
ADD_SUBDIRECTORY (backwardchainer)

LINK_LIBRARIES(
	ruleengine
	atomspace
	clearbox
)

# Run the tests in logical order, not alphabetical order:
# The URE reader has to work, else the chainers will fail
ADD_CXXTEST(ChainerUtilsUTest)
ADD_CXXTEST(UREConfigUTest)
ADD_CXXTEST(RuleUTest)
