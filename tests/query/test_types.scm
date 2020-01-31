
; required atom types.

(define (AntiLink . x)
	(apply cog-new-link (append (list 'AntiLink) x)))

(define (AtTimeLink . x)
	(apply cog-new-link (append (list 'AtTimeLink) x)))

(define (FeatureLink . x)
	(apply cog-new-link (append (list 'FeatureLink) x)))
(define (Feature . x)
	(apply cog-new-link (append (list 'FeatureLink) x)))

(define (HebbianLink . x)
	(apply cog-new-link (append (list 'HebbianLink) x)))

(define (LemmaLink . x)
	(apply cog-new-link (append (list 'LemmaLink) x)))

(define (ParseLink . x)
	(apply cog-new-link (append (list 'ParseLink) x)))

(define (PartOfSpeechLink . x)
	(apply cog-new-link (append (list 'PartOfSpeechLink) x)))

(define (ReferenceLink . x)
	(apply cog-new-link (append (list 'ReferenceLink) x)))

(define (WordInstanceLink . x)
	(apply cog-new-link (append (list 'WordInstanceLink) x)))

(define (DefinedLinguisticConceptNode . x)
	(apply cog-new-node (append (list 'DefinedLinguisticConceptNode) x)))

(define (DefinedLinguisticRelationshipNode . x)
	(apply cog-new-node (append (list 'DefinedLinguisticRelationshipNode) x)))

(define (DocumentNode . x)
	(apply cog-new-node (append (list 'DocumentNode) x)))

(define (FeatureNode . x)
	(apply cog-new-node (append (list 'FeatureNode) x)))

(define (LemmaNode . x)
	(apply cog-new-node (append (list 'LemmaNode) x)))

(define (LgConnectorNode . x)
   (apply cog-new-node (append (list 'LgConnectorNode) x)))

(define (ParseNode . x)
	(apply cog-new-node (append (list 'ParseNode) x)))

(define (PartOfSpeechNode . x)
	(apply cog-new-node (append (list 'PartOfSpeechNode) x)))

(define (PrepositionalRelationshipNode . x)
	(apply cog-new-node (append (list 'PrepositionalRelationshipNode) x)))

(define (SemeNode . x)
	(apply cog-new-node (append (list 'SemeNode) x)))
(define (Seme . x)
	(apply cog-new-node (append (list 'SemeNode) x)))

(define (SentenceNode . x)
	(apply cog-new-node (append (list 'SentenceNode) x)))

(define (TimeNode . x)
	(apply cog-new-node (append (list 'TimeNode) x)))

(define (WordInstanceNode . x)
	(apply cog-new-node (append (list 'WordInstanceNode) x)))

(define (WordNode . x)
	(apply cog-new-node (append (list 'WordNode) x)))
(define (Word . x)
	(apply cog-new-node (append (list 'WordNode) x)))

(define (WordSenseNode . x)
	(apply cog-new-node (append (list 'WordSenseNode) x)))
(define (WordSense . x)
	(apply cog-new-node (append (list 'WordSenseNode) x)))
