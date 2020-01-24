

nameserver().beginTypeDecls("test types");
ANTI_LINK = nameserver().declType(LINK, "AntiLink");
AT_TIME_LINK = nameserver().declType(ORDERED_LINK, "AtTimeLink");
DEFINED_LINGUISTIC_CONCEPT_NODE = nameserver().declType(NODE, "DefinedLinguisticConceptNode");
DEFINED_LINGUISTIC_RELATIONSHIP_NODE = nameserver().declType(NODE, "DefinedLinguisticRelationshipNode");
DOCUMENT_NODE = nameserver().declType(NODE, "DocumentNode");
FEATURE_LINK = nameserver().declType(LINK, "FeatureLink");
FEATURE_NODE = nameserver().declType(NODE, "FeatureNode");
HEBBIAN_LINK = nameserver().declType(LINK, "HebbianLink");
LEMMA_LINK = nameserver().declType(LINK, "LemmaLink");
LEMMA_NODE = nameserver().declType(NODE, "LemmaNode");
LG_CONNECTOR_NODE = nameserver().declType(NODE, "LgConnectorNode");
PARSE_LINK = nameserver().declType(LINK, "ParseLink");
PARSE_NODE = nameserver().declType(NODE, "ParseNode");
PART_OF_SPEECH_LINK = nameserver().declType(LINK, "PartOfSpeechLink");
PART_OF_SPEECH_NODE = nameserver().declType(NODE, "PartOfSpeechNode");
PREPOSITIONAL_RELATIONSHIP_NODE = nameserver().declType(NODE, "PrepositionalRelationshipNode");
REFERENCE_LINK = nameserver().declType(LINK, "ReferenceLink");
SEME_NODE = nameserver().declType(NODE, "SemeNode");
SENTENCE_NODE = nameserver().declType(NODE, "SentenceNode");
TIME_NODE = nameserver().declType(NODE, "TimeNode");
WORD_INSTANCE_LINK = nameserver().declType(LINK, "WordInstanceLink");
WORD_INSTANCE_NODE = nameserver().declType(NODE, "WordInstanceNode");
WORD_NODE = nameserver().declType(NODE, "WordNode");
WORD_SENSE_NODE = nameserver().declType(NODE, "WordSenseNode");
nameserver().endTypeDecls();
