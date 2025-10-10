

; The bindlink below has two disconnected parts. The pattern matcher
; should throw an error for this situation, as it is an undesirable
; kind of thing to have.
(define anaphora-resolution
   (QueryLink
      (VariableList
         (TypedVariableLink
            (VariableNode "$word-inst-antecedent")
            (TypeNode "WordInstanceNode")
         )
         (TypedVariableLink
            (VariableNode "$word-inst-anaphor")
            (TypeNode "WordInstanceNode")
         )
      )
     (AndLink
         (ListLink
            (AnchorNode "Recent Unresolved references")
            (VariableNode "$word-inst-anaphor")
         )
         (PartOfSpeechLink
            (VariableNode "$word-inst-antecedent")
            (DefinedLinguisticConceptNode "noun")
         )
      )
      (ReferenceLink
         (VariableNode "$word-inst-antecedent")
         (VariableNode "$word-inst-anaphor")
      )
   )
)

; weird hack to interface with the C++ code ...
(define (get-bindlink) anaphora-resolution)


; An unused binlink, it provides some confounding junk to confuse
; the pattern matcher.
(define pronoun-finder
   (QueryLink
      (VariableList
           (VariableNode "$sent")
           (VariableNode "$parse")
           (VariableNode "$word-inst")
      )
     (AndLink
          (ListLink (AnchorNode "# New Parsed Sentence") (VariableNode "$sent"))
          (ParseLink (VariableNode "$parse") (VariableNode "$sent"))
          (WordInstanceLink (VariableNode "$word-inst") (VariableNode "$parse"))
          (InheritanceLink
             (VariableNode "$word-inst")
             (DefinedLinguisticConceptNode "pronoun")
          )
     )
     (ListLink
          (AnchorNode "Recent Unresolved references")
          (VariableNode "$word-inst")
     )
   )
)

(ListLink
   (AnchorNode "Recent Unresolved references")
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
)


; Some pointless junk in the atomspace.  Not needed for this test case,
; its just here wasting space.
;
; Relex parse of sentence Tom ate it
; (S (NP Tom.m) (VP ate.v-d (NP it)))

(ReferenceLink
   (WordInstanceNode "LEFT-WALL@sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
   (WordNode "###LEFT-WALL###")
)

(WordInstanceLink
   (WordInstanceNode "LEFT-WALL@sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
)

(ReferenceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (WordNode "Tom")
)

(WordInstanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
)

(ReferenceLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (WordNode "ate")
)

(WordInstanceLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
)

(ReferenceLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (WordNode "it")
)

(WordInstanceLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
)

(ReferenceLink
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
   (ListLink
     (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
     (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
     (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   )
)

(ParseLink
   (ParseNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
   (SentenceNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8")
)

(EvaluationLink
   (LgConnectorNode "Osx")
   (ListLink
      (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
      (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   )
)

(EvaluationLink
   (LgConnectorNode "WV")
   (ListLink
      (WordInstanceNode "LEFT-WALL@sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
      (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   )
)

(EvaluationLink
   (LgConnectorNode "Ss")
   (ListLink
      (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
      (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   )
)

(EvaluationLink
   (LgConnectorNode "Wd")
   (ListLink
      (WordInstanceNode "LEFT-WALL@sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8_parse_0")
      (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   )
)

(LemmaLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (WordNode "Tom")
)

(LemmaLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (WordNode "eat")
)

(LemmaLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (WordNode "it")
)

; _subj (<<eat>>, <<Tom>>)
(EvaluationLink
   (DefinedLinguisticRelationshipNode "_subj")
   (ListLink
      (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
      (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   )
)

; _obj (<<eat>>, <<it>>)
(EvaluationLink
   (DefinedLinguisticRelationshipNode "_obj")
   (ListLink
      (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
      (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   )
)

; tense (eat, past)
(InheritanceLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (DefinedLinguisticConceptNode "past")
)

; subscript-TAG (eat, .v-d)
(InheritanceLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (DefinedLinguisticConceptNode ".v-d")
)

; pos (eat, verb)
(PartOfSpeechLink
   (WordInstanceNode "ate@0d0cf639-73c2-4d6f-9893-7de70ea9c654")
   (DefinedLinguisticConceptNode "verb")
)

; gender (Tom, masculine)
(InheritanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode "masculine")
)

; person-FLAG (Tom, T)
(InheritanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode "person")
)

; subscript-TAG (Tom, .m)
(InheritanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode ".m")
)

; definite-FLAG (Tom, T)
(InheritanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode "definite")
)

; pos (Tom, noun)
(PartOfSpeechLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode "noun")
)

; noun_number (Tom, singular)
(InheritanceLink
   (WordInstanceNode "Tom@316f309d-cf95-493b-bf55-98bebcc91c5a")
   (DefinedLinguisticConceptNode "singular")
)

; gender (it, neuter)
(InheritanceLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (DefinedLinguisticConceptNode "neuter")
)

; definite-FLAG (it, T)
(InheritanceLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (DefinedLinguisticConceptNode "definite")
)

; pos (it, noun)
(PartOfSpeechLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (DefinedLinguisticConceptNode "noun")
)

; pronoun-FLAG (it, T)
(InheritanceLink
   (WordInstanceNode "it@2f788834-beeb-4c8d-914d-8b677bca95fb")
   (DefinedLinguisticConceptNode "pronoun")
)

(ListLink
   (AnchorNode "# New Parsed Sentence")
   (SentenceNode "sentence@f73f6f0d-8822-4220-b6fa-7afa24ec3af8")
)
; END OF SENTENCE
