;
; execute.scm -- Calling callbacks with ExecutionOutputLink.
;
; Arbitrary Python, C++, Haskell, Scheme and Atomese code can be
; called from Atomese, using the ExecutionOutputLink. This allows
; user-defined code to be wrapped up in such a way as to cause
; side-effects when it is triggered.
;
; A note of caution, though: ExecutionOutputLink is a kind-of hack.
; It allows certain simple kinds of "cheats" to be done, but is a
; very low-quality long-term systems programming solution. It is
; best for certain simple things; do NOT use it to build sophisticated
; systems.  See the comments further down about objects and
; continuations.
;
; The example starts with Python, and moves to Scheme examples.
; The most interesting examples, however, are at the end: these
; show how to trigger Atomese programs.
;
; See the `except.scm` example to see what happens when exceptions
; are thrown.
;
; The ExecutionOutputLink is not a continuation, and cannot be used
; as a closure. If you really want closures and continuations, then
; treat Atom types as C++ classes or as Python objects. In brief:
; a type is a type is a type: Atom types can be C++ classes, and
; that is exactly how BindLink, GetLink and PutLink are implemented.
; Don't be afraid of creating more types and classes to do your work.
;
; What else is wrong with ExecutionOutputLink? Well, one of the big
; problems with it is that is an opaque data type. It is impossible
; to know what it does, what it's semantics are. That means that it's
; impossible to use in reasoning.
;
; So, for example: consider an arithmetic expression built out of
; NumberNode, VariableNode, PlusLink and TimesLink. One can perform
; reasoning on this, since plus and times are well-known algebraic
; operations obeying well-known axioms. Describe those axioms in
; Atomese, and then one can start reasoning on arithmetic.
;
; Almost all Atomese Link and Node types obey (well-defined) axioms,
; even if these are only implicit, not fully documented, and not fully
; realized. In principle, you can reason on almost any kinds of
; structures build from Atoms.
;
; ExecutionOutputLinks do not obey any axioms, by definition.  One
; can know, a priori, that PlusLink will add numbers together. One
; cannot know what (GroundedSchema "py:plus") does, no matter how
; suggestively named the Python function is.  If you want to build
; something fancy, and reason on it, create new Atom types, and
; write C++ classes to bring them alive.
;
(use-modules (opencog) (opencog exec) (opencog python))

; The below demonstrates the use of python code in an ExecutionOutputLink.
; Begin by loading the python code. (See `python.scm` for more details).
(python-eval "exec(open('my_py_func.py').read())")

; Execute the python function `my_py_func`. The python function should
; return an atom, which is then printed.  This python function is
; expecting two arguments, which must be atoms. The ListLink provides
; the arguments: the two ConceptNodes are passed to `my_py_func`.
(cog-execute!
	(ExecutionOutput
		(GroundedSchema "py:my_py_func")
		(List
			(Concept "1")
			(Concept "2"))))

; Similar to the above, but, in this case, a TruthValue is returned.
; Notice that cog-evaluate! is used instead of cog-execute!
(cog-evaluate!
	(Evaluation
		(GroundedPredicate "py:my_py_predicate")
		(List
			(Concept "3")
			(Concept "4"))))

; -------------------------------------------------------------
; Equivalent example, invokes scheme code.
;
(define (my-scm-func atoma atomb)
	(display "My func called with atom arguments\n")
	(display atoma) (display atomb)
	(newline)
	(Concept "I'm returning this atom")
)

(cog-execute!
	(ExecutionOutput
		(GroundedSchema "scm:my-scm-func")
		(List
			(Concept "1")
			(Concept "2"))))

; -------------------------------------------------------------
; Another example, using a DefineLink to define a SchemaNode

(DefineLink
	(DefinedSchema "x+y*10")
	(Lambda
		(VariableList
			(Variable "$X")
			(Variable "$Y"))
		(Plus
			(Variable "$X")
			(Times
				(Variable "$Y")
				(Number 10)))))

(cog-execute!
	(ExecutionOutput
		(DefinedSchema "x+y*10")
		(List
			(Number "2")
			(Number "4"))))

; One can also do this, although it is a bit more subtle: the
; PutLink substitutes arguments for variables. The result of the
; beta-reduction is executable, so cog-execute! executes it.

(cog-execute!
   (Put
      (DefinedSchema "x+y*10")
      (List
         (Number "2")
         (Number "4"))))

; -------------------------------------------------------------
; Similar to the above, except that it skips using the DefineLink

(cog-execute!
	(ExecutionOutput
		(Lambda
			(VariableList
				(Variable "$X")
				(Variable "$Y"))
			(Plus
				(Variable "$X")
				(Times
					(Variable "$Y")
					(Number 10))))
		(List
			(Number "2")
			(Number "4"))))

; -------------------------------------------------------------
