; https://github.com/opencog/atomspace/issues/211

(use-modules ((opencog exec)))

(EvaluationLink
   (ConceptNode "arkle")
   (ConceptNode "barkle")
   (ConceptNode "curry"))

(EvaluationLink
   (ConceptNode "glib")
   (ConceptNode "blab"))

(define get
(GetLink
   (VariableList (VariableNode "$a") (VariableNode "$b")
      (TypedVariableLink (VariableNode "$lnk")
          (TypeNode "EvaluationLink")))
   (AndLink
       (VariableNode "$lnk")
       (EvaluationLink
           (VariableNode "$a")
           (VariableNode "$b"))
       (EqualLink
           (VariableNode "$lnk")
           (EvaluationLink
               (VariableNode "$a")
               (VariableNode "$b"))))))
