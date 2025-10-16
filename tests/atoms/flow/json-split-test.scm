;
; json-split-test.scm -- Verify that JsonSplitLink works.
;

(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "json-split-test")
(test-begin tname)

; -------------------------------------------------------------
; Test 1: Simple JSON object

(define json-obj-simple
	(JsonSplit (Node "{\"name\":\"Alice\",\"age\":\"30\"}")))

(define result1 (cog-execute! json-obj-simple))
(format #t "Simple JSON object: ~A\n" result1)

(define expected1
	(LinkValue
		(LinkValue (StringValue "name") (StringValue "Alice"))
		(LinkValue (StringValue "age") (StringValue "30"))))

(test-assert "simple-object" (equal? result1 expected1))

; -------------------------------------------------------------
; Test 2: Simple JSON array

(define json-arr-simple
	(JsonSplit (Node "[\"a\",\"b\",\"c\"]")))

(define result2 (cog-execute! json-arr-simple))
(format #t "Simple JSON array: ~A\n" result2)

(define expected2
	(LinkValue
		(StringValue "a")
		(StringValue "b")
		(StringValue "c")))

(test-assert "simple-array" (equal? result2 expected2))

; -------------------------------------------------------------
; Test 3: JSON with numbers

(define json-numbers
	(JsonSplit (Node "[1,2.5,-3,1.5e2]")))

(define result3 (cog-execute! json-numbers))
(format #t "JSON numbers: ~A\n" result3)

(define expected3
	(LinkValue
		(StringValue "1")
		(StringValue "2.5")
		(StringValue "-3")
		(StringValue "1.5e2")))

(test-assert "number-array" (equal? result3 expected3))

; -------------------------------------------------------------
; Test 4: JSON with literals (true, false, null)

(define json-literals
	(JsonSplit (Node "{\"active\":true,\"deleted\":false,\"data\":null}")))

(define result4 (cog-execute! json-literals))
(format #t "JSON literals: ~A\n" result4)

(define expected4
	(LinkValue
		(LinkValue (StringValue "active") (StringValue "true"))
		(LinkValue (StringValue "deleted") (StringValue "false"))
		(LinkValue (StringValue "data") (StringValue "null"))))

(test-assert "literals" (equal? result4 expected4))

; -------------------------------------------------------------
; Test 5: JSON with escaped strings

(define json-escaped
	(JsonSplit (Node "{\"message\":\"Hello\\nWorld\",\"path\":\"C:\\\\Users\"}")))

(define result5 (cog-execute! json-escaped))
(format #t "JSON escaped strings: ~A\n" result5)

(define expected5
	(LinkValue
		(LinkValue (StringValue "message") (StringValue "Hello\nWorld"))
		(LinkValue (StringValue "path") (StringValue "C:\\Users"))))

(test-assert "escaped-strings" (equal? result5 expected5))

; -------------------------------------------------------------
; Test 6: Nested JSON object

(define json-nested
	(JsonSplit (Node "{\"user\":{\"name\":\"Bob\",\"id\":\"123\"},\"active\":true}")))

(define result6 (cog-execute! json-nested))
(format #t "Nested JSON: ~A\n" result6)

(define expected6
	(LinkValue
		(LinkValue
			(StringValue "user")
			(LinkValue
				(LinkValue (StringValue "name") (StringValue "Bob"))
				(LinkValue (StringValue "id") (StringValue "123"))))
		(LinkValue (StringValue "active") (StringValue "true"))))

(test-assert "nested-object" (equal? result6 expected6))

; -------------------------------------------------------------
; Test 7: JSON array of objects

(define json-arr-objects
	(JsonSplit (Node "[{\"x\":\"1\"},{\"y\":\"2\"}]")))

(define result7 (cog-execute! json-arr-objects))
(format #t "Array of objects: ~A\n" result7)

(define expected7
	(LinkValue
		(LinkValue (LinkValue (StringValue "x") (StringValue "1")))
		(LinkValue (LinkValue (StringValue "y") (StringValue "2")))))

(test-assert "array-of-objects" (equal? result7 expected7))

; -------------------------------------------------------------
; Test 8: Empty JSON object

(define json-empty-obj
	(JsonSplit (Node "{}")))

(define result8 (cog-execute! json-empty-obj))
(format #t "Empty JSON object: ~A\n" result8)

(define expected8 (LinkValue))

(test-assert "empty-object" (equal? result8 expected8))

; -------------------------------------------------------------
; Test 9: Empty JSON array

(define json-empty-arr
	(JsonSplit (Node "[]")))

(define result9 (cog-execute! json-empty-arr))
(format #t "Empty JSON array: ~A\n" result9)

(define expected9 (LinkValue))

(test-assert "empty-array" (equal? result9 expected9))

; -------------------------------------------------------------
; Test 10: JSON with whitespace

(define json-whitespace
	(JsonSplit (Node "  {  \"key\"  :  \"value\"  }  ")))

(define result10 (cog-execute! json-whitespace))
(format #t "JSON with whitespace: ~A\n" result10)

(define expected10
	(LinkValue
		(LinkValue (StringValue "key") (StringValue "value"))))

(test-assert "whitespace" (equal? result10 expected10))

; -------------------------------------------------------------
; Test 11: Parse from StringValue (multiple JSON strings)

(cog-set-value! (Anchor "json-data") (Predicate "content")
	(StringValue
		"{\"first\":\"1\"}"
		"{\"second\":\"2\"}"))

(define json-from-value
	(JsonSplit (ValueOf (Anchor "json-data") (Predicate "content"))))

(define result11 (cog-execute! json-from-value))
(format #t "From StringValue: ~A\n" result11)

(define expected11
	(LinkValue
		(LinkValue (LinkValue (StringValue "first") (StringValue "1")))
		(LinkValue (LinkValue (StringValue "second") (StringValue "2")))))

(test-assert "from-string-value" (equal? result11 expected11))

; -------------------------------------------------------------
; Test 12: Unicode escape sequences

(define json-unicode
	(JsonSplit (Node "{\"smile\":\"\\u263A\",\"heart\":\"\\u2665\"}")))

(define result12 (cog-execute! json-unicode))
(format #t "JSON unicode: ~A\n" result12)

; Note: \u263A is ☺ and \u2665 is ♥
(define expected12
	(LinkValue
		(LinkValue (StringValue "smile") (StringValue "☺"))
		(LinkValue (StringValue "heart") (StringValue "♥"))))

(test-assert "unicode-escapes" (equal? result12 expected12))

; -------------------------------------------------------------
; Test 13: Complex nested structure

(define json-complex
	(JsonSplit (Node "{\"data\":[{\"id\":\"1\",\"tags\":[\"a\",\"b\"]},{\"id\":\"2\",\"tags\":[\"c\"]}]}")))

(define result13 (cog-execute! json-complex))
(format #t "Complex nested: ~A\n" result13)

(define expected13
	(LinkValue
		(LinkValue
			(StringValue "data")
			(LinkValue
				(LinkValue
					(LinkValue (StringValue "id") (StringValue "1"))
					(LinkValue
						(StringValue "tags")
						(LinkValue (StringValue "a") (StringValue "b"))))
				(LinkValue
					(LinkValue (StringValue "id") (StringValue "2"))
					(LinkValue
						(StringValue "tags")
						(LinkValue (StringValue "c"))))))))

(test-assert "complex-nested" (equal? result13 expected13))

; -------------------------------------------------------------
(test-end tname)

(opencog-test-end)
