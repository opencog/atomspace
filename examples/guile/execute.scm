;
; Several ExecutionOutputLink examples.
;
; An example of using the cog-execute! function to trigger the execution
; of ExecutationOutputLink's.  This example illustrates using functions
; defined in scheme, python and 'atomese'.
;
; See also the except.scm example to see what happens when exceptions
; are thrown.
;
(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (opencog python))

; The below demonstrates the use of python code in an execution link.
; Begin by loading the python code. (See `python.scm` for more details).
(python-eval "execfile('my_py_func.py')")

; Execute the python function `my_py_func`. The python function shhould
; return an atom, which is then printed.
(cog-execute!
	(ExecutionOutputLink
		(GroundedSchemaNode "py:my_py_func")
		(ListLink
			(ConceptNode "1")
			(ConceptNode "2"))))

; Similar to the above, but, in this case, a truth value is returned.
; Notice that cog-evaluate! is used instead of cog-execute!
(cog-evaluate!
	(EvaluationLink
		(GroundedPredicateNode "py:my_py_predicate")
		(ListLink
			(ConceptNode "3")
			(ConceptNode "4"))))

; -------------------------------------------------------------
; Equivalent example, invokes scheme code.
;
(define (my-scm-func atoma atomb)
	(display "My func called with atom arguments\n")
	(display atoma) (display atomb)
	(newline)
	(ConceptNode "I'm returning this atom")
)

(cog-execute!
	(ExecutionOutputLink
		(GroundedSchemaNode "scm:my-scm-func")
		(ListLink
			(ConceptNode "1")
			(ConceptNode "2"))))

; -------------------------------------------------------------
; Another example, using a DefineLink to define a SchemaNode

(DefineLink
	(DefinedSchemaNode "x+y*10")
	(LambdaLink
		(VariableList
			(VariableNode "$X")
			(VariableNode "$Y"))
		(PlusLink
			(VariableNode "$X")
			(TimesLink
				(VariableNode "$Y")
				(NumberNode 10)))))

(cog-execute!
	(ExecutionOutputLink
		(DefinedSchemaNode "x+y*10")
		(ListLink
			(NumberNode "2")
			(NumberNode "4"))))

; One can also do this, although it is a bit more subtle: the
; PutLink substitutes arguments for variables. The result of the
; beta-reduction is executable, so cog-execute! executes it.

(cog-execute!
   (PutLink
      (DefinedSchemaNode "x+y*10")
      (ListLink
         (NumberNode "2")
         (NumberNode "4"))))

; -------------------------------------------------------------
; Similar to the above, except that it skips using the DefineLink

(cog-execute!
	(ExecutionOutputLink
		(LambdaLink
			(VariableList
				(VariableNode "$X")
				(VariableNode "$Y"))
			(PlusLink
				(VariableNode "$X")
				(TimesLink
					(VariableNode "$Y")
					(NumberNode 10))))
		(ListLink
			(NumberNode "2")
			(NumberNode "4"))))

; -------------------------------------------------------------
