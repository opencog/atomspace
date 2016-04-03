;
; disco-vars.scm
;
; Several patterns with ill-formed variable declarations.

; The below used to be ill-formed, but the current matcher
; accepts this and ... wastes CPU time with it.  Technically,
; it is kind-of ill-formed, since there should be a "present"
; search being made for it. But we punt for now.
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
