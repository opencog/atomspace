; https://github.com/opencog/atomspace/issues/211

(use-modules ((opencog exec)))

(EdgeLink
   (ConceptNode "arkle")
   (ConceptNode "barkle")
   (ConceptNode "curry"))

(EdgeLink
   (ConceptNode "glib")
   (ConceptNode "blab"))

(define get
(Meet
   (VariableList (VariableNode "$a") (VariableNode "$b")
      (TypedVariableLink (VariableNode "$lnk")
          (TypeNode "EdgeLink")))
   (AndLink
       (VariableNode "$lnk")
       (EdgeLink
           (VariableNode "$a")
           (VariableNode "$b"))
       (EqualLink
           (VariableNode "$lnk")
           (EdgeLink
               (VariableNode "$a")
               (VariableNode "$b"))))))
