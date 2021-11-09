;
; infinite.scm -- Infinite recursion
;
; This demo assumes you've read and understood the `recrusive.scm` demo.
;
; This demo illustrates how create infinite-recursive queries that don't
; blow out the stack. Basically, the final step of the recusion is
; replaced by a kind-of-like tail-recursive step, which can be thought
; of as a kind-of-like a call to a continuation.
;
(use-modules (opencog) (opencog exec))

; ----------
; This uses the same demo upper ontology as `recursive.scm`, except
; this will be modified to be circular. Thus, attempting to determine
; inheritance will lead to infinite regress.
;
(Inheritance (Concept "physical thing") (Concept "thing"))
(Inheritance (Concept "living thing") (Concept "physical thing"))
(Inheritance (Concept "animal") (Concept "living thing"))
(Inheritance (Concept "bilateria") (Concept "animal"))
(Inheritance (Concept "chordate") (Concept "bilateria"))
(Inheritance (Concept "vertebrate") (Concept "chordate"))
(Inheritance (Concept "mammal") (Concept "vertebrate"))
(Inheritance (Concept "human") (Concept "mammal"))
(Inheritance (Concept "Ben") (Concept "human"))

; This last declaration doesn't fit the pattern above; it introduces
; a circular loop: 'everything' is a 'Ben'.
(Inheritance (Concept "thing") (Concept "Ben"))

; ----------
; Just as in the `recursive.scm` demo, inheritance is implemented as
; 'transitive closure'. This is the idea that given some relation
; R(x,y) (in this case, the InheritanceLink) that either one has
; R(a,b) is directly, immediately true for elements a,b or that
; there is a transitive chain
;
;     R(a,x) & R(x,y) & ... & R(z,b)
;
; for some intermediate elements x,y,...,z. The & here denotes logical
; 'and'; each of the R must be logical-true.
;
; The above can be implemented programmatically by defining a recursive
; relation S(x,y) as follows:
;
;    S(x,y) := R(x,y) or (R(x,w) & S(w,y))
;
; The := symbol here is the definition of S. It is recursive in that the
; definition makes reference to itself. It just says that either S is R,
; or that we can peel off one level, and try again.
;
; The definition below is explained in detail in `recursive.scm`
;
(Define
	(DefinedPredicate "recursive relation")                 ;; Step 1.
	(Lambda
		(VariableList (Variable "this") (Variable "that"))   ;; Step 2.
		(SequentialOr                                        ;; Step 3.
			(Present
				(Inheritance (Variable "this") (Variable "that"))) ;; Step 4.
			(Satisfaction
				(Variable "middle")                            ;; Step 5.
				(And
					(Present                                    ;; Step 6.
						(Inheritance (Variable "this") (Variable "middle")))
					(Put                                        ;; Step 7.
						(DefinedPredicate "recursive relation")
						(List (Variable "middle") (Variable "that"))))))))

; Let's test it out. Does it work? The query below returns (stv 1 1)
; that is, 'logical-true', because the inheritance chain between 'Ben'
; and 'animal' is finite in length.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "recursive relation")
		(List (Concept "Ben") (Concept "animal"))))

; If we try to verify that 'Ben' isn't 'foobar', there is a problem: the
; attempted transitive closure never ends, because 'foobar' cannot be
; found in the chain.  Running the below results in an error message
; (currently, the error is about a "Transient space memleak!") This is
; because, at eash step through the inheritance chain, a temporary
; AtomSpace is created, to be used as a scratchpad (a Kripke frame, to
; be formal). Eventually, one runs out of room for this infinite regress
; of scratchpads, and an error results.
(cog-evaluate!
	(Evaluation
		(DefinedPredicate "recursive relation")
		(List (Concept "Ben") (Concept "foobar"))))

; ----------

(Define
	(DefinedPredicate "inf regress")
	(Lambda
		(VariableList (Variable "this") (Variable "that"))
		(SequentialOr
			(Present
				(Inheritance (Variable "this") (Variable "that")))
			(Satisfaction
				(Variable "middle")
				(And
					(Present
						(Inheritance (Variable "this") (Variable "middle")))
					(Continuation
						(DefinedPredicate "inf regress")
						(List (Variable "middle") (Variable "that"))))))))

(cog-evaluate!
	(Evaluation
		(DefinedPredicate "inf regress")
		(List (Concept "Ben") (Concept "foobar"))))

; ----------
; That's All Folks!  The End!

