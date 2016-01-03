;
; Several ExecutionOutputLink examples.
; An example of using the cog-execute! function to trigger the execution
; of ExecutationOutputLink's.  This example illustrates using functions
; defined in scheme, python and 'atomese'.
;
; See also the except.scm example to see what happens when exceptions
; are thrown.
;
(use-modules (opencog))
(use-modules (opencog exec))

; Using python code in an execution link. Be sure to set the PYTHONPATH
; environment varialbe first, so that the cython and python libraries
; can be found. Something similar to this should do:
;
; export PYTHONPATH=/usr/local/opencog/cython:/usr/local/opencog/python:./examples/guile
; or maybe this:
; export PYTHONPATH=/usr/localbuild/opencog/cython:./opencog/python:./opencog/python/opencog:./examples/guile
;
; Also, be sure that my_py_func.py is loaded.
; XXX -- Seems that Python is currently borken; don't know how to fix.
;
(cog-execute!
	(ExecutionOutputLink
		(GroundedSchemaNode "py:my_py_func")
		(ListLink
			(ConceptNode "1")
			(ConceptNode "2"))))

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
