;
; Test utilities

; test count truth value incrementation
(define counter (ConceptNode "asdf"))
(define (test-incr-cnt) (cog-inc-count! counter 1))

(define key (PredicateNode "key"))
(define (test-incr-value) (cog-inc-value! counter key 0.5 3))

; test cog-get-partner
(define partner (ConceptNode "partner"))
(define pare (ListLink partner counter))
(define recount (cog-get-partner pare partner))

; test cog-pred-get-partner
(define evl (EvaluationLink (WordNode "asdf") pare))
(define rrcnt (cog-pred-get-partner evl partner))

; test cog-get-link
(define ref (ReferenceLink (ConceptNode "asdf") (WordNode "pqrs")))
(define wref (car (cog-get-link 'ReferenceLink 'ConceptNode (WordNode "pqrs"))))

; test cog-get-atoms. Warning: it uses previously defined nodes, so
; should be updated if any new nodes are introduced above.
(define cpts (Set (cog-get-atoms 'ConceptNode)))
(define cpts-n-subtypes (Set (cog-get-atoms 'ConceptNode #t)))
(define nodes-n-subtypes (Set (cog-get-atoms 'Node #t)))
(define x-cpts (Set (ConceptNode "asdf") (ConceptNode "partner")))
(define x-cpts-n-subtypes x-cpts)
(define x-nodes-n-subtypes (Set
                             (ConceptNode "asdf")
                             (ConceptNode "partner")
                             (PredicateNode "key")
                             (WordNode "asdf")
                             (WordNode "pqrs")))
