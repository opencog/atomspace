
#include <opencog/atomspace/ClassServer.h>

using namespace opencog;

Type DEFINED_LINGUISTIC_RELATIONSHIP_NODE
	= classserver().addType(NODE, "DefinedLinguisticRelationshipNode");
Type LEMMA_LINK
	= classserver().addType(LINK, "LemmaLink");
Type PARSE_LINK
	= classserver().addType(LINK, "ParseLink");
Type PARSE_NODE
	= classserver().addType(NODE, "ParseNode");
Type PART_OF_SPEECH_LINK
	= classserver().addType(LINK, "PartOfSpeechLink");
Type REFERENCE_LINK
	= classserver().addType(LINK, "ReferenceLink");
Type TIME_NODE
	= classserver().addType(NODE, "TimeNode");
Type WORD_INSTANCE_LINK
	= classserver().addType(LINK, "WordInstanceLink");
Type WORD_INSTANCE_NODE
	= classserver().addType(NODE, "WordInstanceNode");
Type WORD_NODE
	= classserver().addType(NODE, "WordNode");
