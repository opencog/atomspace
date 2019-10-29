;; For NoExceptionUTest. An execution returns no atom, which is passed
;; to the ImplicationLink implicant, as a result the ImplicationLink
;; C++ factory fails. I believe the pattern matcher should ignore this
;; failure by catching the Exception rather than crashing (passing the
;; Exception above).

;; Failing (before fixing) bind link
(define bl
(BindLink
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

;; Schema returning undefined handle
(define (crisp-modus-ponens-formula A AB B)
    (let (  (sA (cog-mean A))
            (cA (cog-confidence A))
            (sAB (cog-mean AB))
            (cAB (cog-confidence AB)))
        (if (and (>= sA 0.5) (>= cA 0.5) (>= sAB 0.5) (>= cAB 0.5))
            (cog-set-tv! B (stv 1 1)))))

;; Grounds
(Implication (stv 1 1)
 (Predicate "R")
 (Predicate "S"))

(Implication (stv 1 1)
 (Predicate "S")
 (Predicate "T"))
