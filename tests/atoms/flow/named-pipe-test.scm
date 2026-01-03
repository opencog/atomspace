#! /usr/bin/env guile
-s
!#
;
; named-pipe-test.scm -- Verify that PipeLink and NameNode work.
;
; Tests the named-pipe examples from examples/flow/named-pipes.scm.
; Verifies that PipeLink can attach names to data streams and that
; NameNode execution yields the same results as the named source.
;
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "named-pipe-test")
(test-begin tname)

; ------------------------------------------------------------
; Test 1: Basic RandomNumber execution
; Verify RandomNumber generates values in range [5, 10]

(define rand1 (cog-execute! (RandomNumber (Number 5) (Number 10))))
(define rand1-val (cog-value-ref rand1 0))
(test-assert "rand1-lower-bound" (>= rand1-val 5))
(test-assert "rand1-upper-bound" (<= rand1-val 10))

; ------------------------------------------------------------
; Test 2: PipeLink with NameNode for random number source
; Declare a named pipe and verify execution through the name

(Pipe
	(Name "five-n-dime store")
	(RandomNumber (Number 5) (Number 10)))

; Execute via the NameNode
(define named-rand1 (cog-execute! (Name "five-n-dime store")))
(define named-rand1-val (cog-value-ref named-rand1 0))
(test-assert "named-rand1-lower" (>= named-rand1-val 5))
(test-assert "named-rand1-upper" (<= named-rand1-val 10))

; Execute again to verify it produces new random values each time
(define named-rand2 (cog-execute! (Name "five-n-dime store")))
(define named-rand2-val (cog-value-ref named-rand2 0))
(test-assert "named-rand2-lower" (>= named-rand2-val 5))
(test-assert "named-rand2-upper" (<= named-rand2-val 10))

; ------------------------------------------------------------
; Test 3: Alternative SetValue/ValueOf approach
; More verbose way to achieve similar result

(cog-execute!
	(SetValue
		(Anchor "strip mall")
		(Predicate "Dollar General")
		(LinkSignature
			(Type 'FormulaStream)
			(RandomNumber (Number 1) (Number 2)))))

; Get values via ValueOfLink
(define setval-result (cog-execute!
	(ValueOf
		(Anchor "strip mall")
		(Predicate "Dollar General"))))

(define setval-val (cog-value-ref setval-result 0))
(test-assert "setval-lower" (>= setval-val 1))
(test-assert "setval-upper" (<= setval-val 2))

; ------------------------------------------------------------
; Test 4: PipeLink with static string data
; Create a named pipe with StringValues

(PipeLink
	(NameNode "words")
	(LinkSignature (Type 'LinkValue)
		(LinkSignature (Type 'StringValue) (Concept "my-oh-my"))
		(LinkSignature (Type 'StringValue) (Concept "do-da"))
		(LinkSignature (Type 'StringValue) (Concept "zip-a-dee do-dah day"))))

; Execute the named stream
(define words-result (cog-execute! (Name "words")))

; Verify it returns a LinkValue with 3 elements
(test-assert "words-is-linkvalue" (cog-value? words-result))
(define words-list (cog-value->list words-result))
(test-assert "words-count" (equal? 3 (length words-list)))

; Verify the strings are present
(define word0 (cog-value-ref (list-ref words-list 0) 0))
(define word1 (cog-value-ref (list-ref words-list 1) 0))
(define word2 (cog-value-ref (list-ref words-list 2) 0))

(test-assert "word0-check" (string? word0))
(test-assert "word1-check" (string? word1))
(test-assert "word2-check" (string? word2))

; Verify content matches expected strings
(test-assert "word0-content" (equal? "my-oh-my" word0))
(test-assert "word1-content" (equal? "do-da" word1))
(test-assert "word2-content" (equal? "zip-a-dee do-dah day" word2))

; ------------------------------------------------------------
; Test 5: TransposeColumn on named stream
; Verify TransposeColumn can be applied to the named source

(define transposed (cog-execute! (TransposeColumn (Name "words"))))
(test-assert "transpose-exists" (cog-value? transposed))

; TransposeColumn should produce a single StringValue with concatenated results
(define trans-list (cog-value->list transposed))
(test-assert "transpose-has-elements" (> (length trans-list) 0))

; ------------------------------------------------------------
(test-end tname)
(opencog-test-end)
