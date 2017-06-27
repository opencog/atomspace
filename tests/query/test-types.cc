
#include "test-types.h"


classserver().beginTypeDecls();
AT_TIME_LINK = classserver().declType(ORDERED_LINK, "AtTimeLink");
DEFINED_LINGUISTIC_CONCEPT_NODE = classserver().declType(NODE, "DefinedLinguisticConceptNode");
DEFINED_LINGUISTIC_RELATIONSHIP_NODE = classserver().declType(NODE, "DefinedLinguisticRelationshipNode");
DOCUMENT_NODE = classserver().declType(NODE, "DocumentNode");
FEATURE_LINK = classserver().declType(LINK, "FeatureLink");
FEATURE_NODE = classserver().declType(NODE, "FeatureNode");
HEBBIAN_LINK = classserver().declType(LINK, "HebbianLink");
LEMMA_LINK = classserver().declType(LINK, "LemmaLink");
LEMMA_NODE = classserver().declType(NODE, "LemmaNode");
LG_CONNECTOR_NODE = classserver().declType(NODE, "LgConnectorNode");
PARSE_LINK = classserver().declType(LINK, "ParseLink");
PARSE_NODE = classserver().declType(NODE, "ParseNode");
PART_OF_SPEECH_LINK = classserver().declType(LINK, "PartOfSpeechLink");
PART_OF_SPEECH_NODE = classserver().declType(NODE, "PartOfSpeechNode");
PREPOSITIONAL_RELATIONSHIP_NODE = classserver().declType(NODE, "PrepositionalRelationshipNode");
REFERENCE_LINK = classserver().declType(LINK, "ReferenceLink");
SEME_NODE = classserver().declType(NODE, "SemeNode");
SENTENCE_NODE = classserver().declType(NODE, "SentenceNode");
TIME_NODE = classserver().declType(NODE, "TimeNode");
WORD_INSTANCE_LINK = classserver().declType(LINK, "WordInstanceLink");
WORD_INSTANCE_NODE = classserver().declType(NODE, "WordInstanceNode");
WORD_NODE = classserver().declType(NODE, "WordNode");
WORD_SENSE_NODE = classserver().declType(NODE, "WordSenseNode");
classserver().endTypeDecls();
