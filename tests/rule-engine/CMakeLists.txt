LINK_LIBRARIES(
	ruleengine
	atomspace
	logger
)

# Run the tests in logical order, not alphabetical order:
# The URE reader has to work, else the chainers will fail
ADD_CXXTEST(UREConfigUTest)
ADD_CXXTEST(BetaDistributionUTest)
ADD_CXXTEST(ActionSelectionUTest)
ADD_CXXTEST(RuleUTest)

ADD_SUBDIRECTORY (forwardchainer)
ADD_SUBDIRECTORY (backwardchainer)
