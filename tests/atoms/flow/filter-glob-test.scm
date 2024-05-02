;
; filter-glob-test.scm -- Simple tests of top-level globs.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "filter-glob-test")
(test-begin tname)

; -----------
(define glob-all (Filter (Glob "$x") (Concept "va")))
(define e-glob-all (cog-execute! glob-all))

(test-assert "glob of everything"
	(equal? e-glob-all (List (Concept "va"))))

; -----------
(define glob-rule
	(Filter
		(Rule (Glob "$x")(Glob "$x")(Glob "$x"))
		(Concept "vb")))
(define e-glob-rule (cog-execute! glob-rule))

(test-assert "glob of everything"
	(equal? e-glob-rule (List (Concept "vb"))))

; -----------
(define glob-of
   (Filter
      (Rule (Glob "$x")(Glob "$x")(Glob "$x"))
		(ValueOf (Concept "a") (Predicate "b"))))

(cog-set-value! (Concept "a") (Predicate "b") (Concept "vc"))

(define e-glob-of (cog-execute! glob-of))
(test-assert "glob of value of"
	(equal? e-glob-of (List (Concept "vc"))))

(cog-set-value! (Concept "a") (Predicate "b")
	(StringValue "a" "b" "c"))

(define e-glob-svof (cog-execute! glob-of))
(test-assert "glob of string"
	(equal? e-glob-svof (LinkValue (StringValue "a" "b" "c"))))

(cog-set-value! (Concept "a") (Predicate "b")
	(LinkValue (StringValue "d" "e" "f")))

(define e-glob-lsvof (cog-execute! glob-of))
(test-assert "glob of linkstring"
	(equal? e-glob-lsvof (LinkValue (LinkValue (StringValue "d" "e" "f")))))

; -----------

(cog-set-value! (Concept "a") (Predicate "b")
	(LinkValue (LinkValue (StringValue "d" "e" "f"))))

; Earlier versions of the code core dumped when executing the below,
; with a null-pointer deref, because `(Glob "$uh-ohhhh")` was never
; grounded. This test passes if there is no crash.
(define glob-match
   (Filter
      (Rule
			(LinkSignature (Type 'LinkValue) (Glob "$x"))
			(Glob "$uh-ohhhh"))
		(ValueOf (Concept "a") (Predicate "b"))))

(cog-execute! glob-match)
(define uhohh (cog-execute! glob-match))
(test-assert "bad grounding glob"
	(equal? uhohh (LinkValue (Glob "$uh-ohhhh"))))

; -----------

(test-end tname)

(opencog-test-end)
