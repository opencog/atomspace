
LINK_LIBRARIES(
	ruleengine
	atomspace
	clearbox
)

# Run the tests in logical order, not alphabetical order:
# The URE reader has to work, else the chainers will fail
ADD_CXXTEST(ChainerUtilsUTest)
ADD_CXXTEST(URECommonsUTest)
ADD_CXXTEST(UREConfigReaderUTest)
ADD_CXXTEST(ForwardChainerUTest)
ADD_CXXTEST(BackwardChainerUTest)
