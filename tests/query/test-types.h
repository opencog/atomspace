
#include <opencog/atomspace/ClassServer.h>

using namespace opencog;

Type AT_TIME_LINK
	= classserver().addType(ORDERED_LINK, "AtTimeLink");
Type DEFINED_LINGUISTIC_CONCEPT_NODE
	= classserver().addType(NODE, "DefinedLinguisticConceptNode");
Type DEFINED_LINGUISTIC_RELATIONSHIP_NODE
	= classserver().addType(NODE, "DefinedLinguisticRelationshipNode");
Type DOCUMENT_NODE
	= classserver().addType(NODE, "DocumentNode");
Type FEATURE_LINK
	= classserver().addType(LINK, "FeatureLink");
Type FEATURE_NODE
	= classserver().addType(NODE, "FeatureNode");
Type HEBBIAN_LINK
	= classserver().addType(LINK, "HebbianLink");
Type LEMMA_LINK
	= classserver().addType(LINK, "LemmaLink");
Type LEMMA_NODE
	= classserver().addType(NODE, "LemmaNode");
Type PARSE_LINK
	= classserver().addType(LINK, "ParseLink");
Type PARSE_NODE
	= classserver().addType(NODE, "ParseNode");
Type PART_OF_SPEECH_LINK
	= classserver().addType(LINK, "PartOfSpeechLink");
Type PART_OF_SPEECH_NODE
	= classserver().addType(NODE, "PartOfSpeechNode");
Type PREPOSITIONAL_RELATIONSHIP_NODE
	= classserver().addType(NODE, "PrepositionalRelationshipNode");
Type REFERENCE_LINK
	= classserver().addType(LINK, "ReferenceLink");
Type SEME_NODE
	= classserver().addType(NODE, "SemeNode");
Type SENTENCE_NODE
	= classserver().addType(NODE, "SentenceNode");
Type TIME_NODE
	= classserver().addType(NODE, "TimeNode");
Type WORD_INSTANCE_LINK
	= classserver().addType(LINK, "WordInstanceLink");
Type WORD_INSTANCE_NODE
	= classserver().addType(NODE, "WordInstanceNode");
Type WORD_NODE
	= classserver().addType(NODE, "WordNode");
Type WORD_SENSE_NODE
	= classserver().addType(NODE, "WordSenseNode");
