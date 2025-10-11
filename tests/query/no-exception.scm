;; For NoExceptionUTest. An execution returns no atom, which is passed
;; to the ImplicationLink implicant, as a result the ImplicationLink
;; C++ factory fails. I believe the pattern matcher should ignore this
;; failure by catching the Exception rather than crashing (passing the
;; Exception above).

;; Failing (before fixing) bind link
(define bl
(CollectionOf
(QueryLink
  (VariableList
    (TypedVariableLink
      (VariableNode "$A-648d8b51") ; [56][45]
      (TypeChoice
        (TypeNode "LambdaLink") ; [3][1]
        (TypeNode "PredicateNode") ; [4][1]
      ) ; [5][1]
    ) ; [57][45]
    (TypedVariableLink
      (VariableNode "$A-4c2b3a57") ; [49][45]
      (TypeChoice
        (TypeNode "LambdaLink") ; [3][1]
        (TypeNode "PredicateNode") ; [4][1]
      ) ; [5][1]
    ) ; [50][45]
  ) ; [58][45]
  (AndLink
    (ImplicationLink
      (VariableNode "$A-4c2b3a57") ; [49][45]
      (PredicateNode "T") ; [32][1]
    ) ; [51][45]
    (VariableNode "$A-648d8b51") ; [56][45]
    (ImplicationLink
      (VariableNode "$A-648d8b51") ; [56][45]
      (VariableNode "$A-4c2b3a57") ; [49][45]
    ) ; [59][45]
  ) ; [63][45]
  (ExecutionOutputLink
    (GroundedSchemaNode "scm: crisp-modus-ponens-formula") ; [12][1]
    (ListLink
      (ExecutionOutputLink
        (GroundedSchemaNode "scm: crisp-modus-ponens-formula") ; [12][1]
        (ListLink
          (VariableNode "$A-648d8b51") ; [56][45]
          (ImplicationLink
            (VariableNode "$A-648d8b51") ; [56][45]
            (VariableNode "$A-4c2b3a57") ; [49][45]
          ) ; [59][45]
          (VariableNode "$A-4c2b3a57") ; [49][45]
        ) ; [61][45]
      ) ; [62][45]
      (ImplicationLink
        (ExecutionOutputLink ; <-- this call returns no atom
          (GroundedSchemaNode "scm: crisp-modus-ponens-formula") ; [12][1]
          (ListLink
            (VariableNode "$A-648d8b51") ; [56][45]
            (ImplicationLink
              (VariableNode "$A-648d8b51") ; [56][45]
              (VariableNode "$A-4c2b3a57") ; [49][45]
            ) ; [59][45]
            (VariableNode "$A-4c2b3a57") ; [49][45]
          ) ; [61][45]
        ) ; [62][45]
        (PredicateNode "T") ; [32][1]
      ) ; [64][45]
      (PredicateNode "T") ; [32][1]
    ) ; [65][45]
  ) ; [66][45]
) ; [67][45]
)
)

(define tvkey (Predicate "*-TruthValueKey-*"))

(define (get-tv ATOM)
	(cog-value ATOM tvkey))

(define (get-tv-mean TV)
	(if TV (cog-value-ref TV 0) #f))

(define (get-tv-confidence TV)
	(if TV (cog-value-ref TV 1) #f))

(define (get-mean ATOM)
	(define tv (get-tv ATOM))
	(if tv (get-tv-mean tv) 1))

(define (get-confidence ATOM)
	(define tv (get-tv ATOM))
	(if tv (get-tv-confidence tv) 0))

;; Schema returning undefined handle
(define (crisp-modus-ponens-formula A AB B)
    (let (  (sA (get-mean A))
            (cA (get-confidence A))
            (sAB (get-mean AB))
            (cAB (get-confidence AB)))
        (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
            (cog-set-value! B tvkey (FloatValue 1 1)))))

;; Grounds
(cog-set-value! (Implication (Predicate "R") (Predicate "S"))
	tvkey (FloatValue 1 1))

(cog-set-value! (Implication (Predicate "S") (Predicate "T"))
	tvkey (FloatValue 1 1))
