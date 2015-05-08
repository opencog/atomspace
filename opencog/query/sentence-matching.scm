; Returns one or more sentences that are structurally similar to the input one.
;
; For example:
;    (get-similar-sentences "What did Pete eat?")
;
; Possible result: 
;    (Pete ate apples .)
;
(define (get-similar-sentences input-sentence)
    ; Make sure it is at least a SetLink before sending it to sureal for sentence generation
    (define (to-sureal a-link) (if (equal? 'SetLink (cog-type a-link)) (sureal a-link) #f))

    ; Generate sentences from each of the R2L-SetLinks
    (define (generate-sentences r2l-setlinks) (if (> (length r2l-setlinks) 0) (map to-sureal r2l-setlinks) '()))

    (begin
        ; Delete identical sentences from the return set
        (delete-duplicates
            ; Use SuReal to generate sentences from their corresponding SetLinks
            (generate-sentences
                ; TODO: Filter out non-SetLinks here, or do it in (generate-sentences)
                ; Search for any similar SetLinks in the atomspace
                (cog-outgoing-set (cog-fuzzy-match
                    ; Get the R2L SetLink of the input sentence
                    (car (cog-chase-link 'ReferenceLink 'SetLink
                        (car (cog-chase-link 'InterpretationLink 'InterpretationNode
                            (car (cog-chase-link 'ParseLink 'ParseNode
                                (car (nlp-parse input-sentence))
                            ))
                        ))
                    ))
                ))
            )
        )
    )
)

