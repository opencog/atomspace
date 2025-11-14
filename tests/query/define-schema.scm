;
; Demonstrate the used of DefinedSchemaNodes
;
(define (get-timestamp)
   (NumberNode (current-time)))

(DefineLink
   (DefinedSchemaNode "set timestamp")
   (PutLink
      (EdgeLink (PredicateNode "event-timestamp")
         (ListLink (VariableNode "$ts")))
      (ExecutionOutputLink
         (GroundedSchemaNode "scm: get-timestamp")
         (ListLink))))

; (cog-execute! (DefinedSchemaNode "set timestamp"))
