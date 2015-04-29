
; TODO
; Fake out the required atom types.

(define (LemmaLink . x)
	(apply cog-new-link (append (list 'LemmaLink) x)))

(define (ParseLink . x)
	(apply cog-new-link (append (list 'ParseLink) x)))

(define (PartOfSpeechLink . x)
	(apply cog-new-link (append (list 'PartOfSpeechLink) x)))

(define (WordInstanceLink . x)
	(apply cog-new-link (append (list 'WordInstanceLink) x)))

(define (DefinedLinguisticRelationshipNode . x)
	(apply cog-new-node (append (list 'DefinedLinguisticRelationshipNode) x)))

(define (ParseNode . x)
	(apply cog-new-node (append (list 'ParseNode) x)))

(define (TimeNode . x)
	(apply cog-new-node (append (list 'TimeNode) x)))

(define (WordInstanceNode . x)
	(apply cog-new-node (append (list 'WordInstanceNode) x)))

(define (WordNode . x)
	(apply cog-new-node (append (list 'WordNode) x)))

