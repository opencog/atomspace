;
; disco-vars.scm
;
; Several patterns with ill-formed variable declarations.

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
