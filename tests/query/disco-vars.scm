;
; disco-vars.scm
;
; Several patterns with implicit clauses.
;
; Prior to issue #2516, these were considered to be ill-formed
; search patterns, because they lacked explicit clauses that
; asserted the presence of a variable in the atompsace.  However,
; it seems to be OK to assume implicit presence, when it is not
; declared explicity. So, in fact, everything below should be
; considered to be valid.

(use-modules (opencog))

(define (B)
(GetLink
   (VariableList (VariableNode "$A") (VariableNode "$B"))
   (AndLink
      (EqualLink (VariableNode "$A") (VariableNode "$B")))))

(define (Ba)
(GetLink
   (VariableList (VariableNode "$A") (VariableNode "$B"))
   (AndLink
      (PresentLink (VariableNode "$A"))
      (EqualLink (VariableNode "$A") (VariableNode "$B")))))

(define (Bu)
(GetLink
   (VariableList (VariableNode "$A") (VariableNode "$B") (VariableNode "$C"))
   (AndLink
      (InheritanceLink (VariableNode "$A") (VariableNode "$B"))
      (EqualLink (VariableNode "$A") (VariableNode "$C")))))
