;
; bool-value-print-test.scm -- Verify that BoolValue to_string() works
; (It doesn't; it needs a length value, but no one cares right now)
;
(use-modules (opencog))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "bool-value-print-test")
(test-begin tname)

(define b15 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0))
(define p15 (format #f "~A" b15))
(format #t "p15 = >>~A<<\n" p15)
(test-assert "p15"
	(equal? p15 "(BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0)\n"))

(define b16 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1))
(define p16 (format #f "~A" b16))
(format #t "p16 = >>~A<<\n" p16)
(test-assert "p16"
	(equal? p16 "(BoolValue #xf731)\n"))

(define b17 (BoolValue 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1))
(define p17 (format #f "~A" b17))
(format #t "p17 = >>~A<<\n" p17)
(test-assert "p17"
	(equal? p17 "(BoolValue #x1ee63)\n"))

(define b21 (BoolValue 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1))
(define p21 (format #f "~A" b21))
(format #t "p21 = >>~A<<\n" p21)
(test-assert "p21"
	(equal? p21 "(BoolValue #x01ee63)\n"))

(define b25 (BoolValue 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0))
(define p25 (format #f "~A" b25))
(format #t "p25 = >>~A<<\n" p25)
(test-assert "p25"
	(equal? p25 "(BoolValue #x01ee630)\n"))

(define b29 (BoolValue 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0))
(define p29 (format #f "~A" b29))
(format #t "p29 = >>~A<<\n" p29)
(test-assert "p29"
	(equal? p29 "(BoolValue #x001ee630)\n"))

(define b61 (BoolValue 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p61 (format #f "~A" b61))
(format #t "p61 = >>~A<<\n" p61)
(test-assert "p61"
	(equal? p61 "(BoolValue #x001ee63012345678)\n"))

(define b63 (BoolValue 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p63 (format #f "~A" b63))
(format #t "p63 = >>~A<<\n" p63)
(test-assert "p63"
	(equal? p63 "(BoolValue #x601ee63012345678)\n"))

(define b64 (BoolValue 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p64 (format #f "~A" b64))
(format #t "p64 = >>~A<<\n" p64)
(test-assert "p64"
	(equal? p64 "(BoolValue #xe01ee63012345678)\n"))

(define b65 (BoolValue 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p65 (format #f "~A" b65))
(format #t "p65 = >>~A<<\n" p65)
(test-assert "p65"
	(equal? p65 "(BoolValue #x1e01ee63012345678)\n"))

(define b66 (BoolValue 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p66 (format #f "~A" b66))
(format #t "p66 = >>~A<<\n" p66)
(test-assert "p66"
	(equal? p66 "(BoolValue #x3e01ee63012345678)\n"))

(define b67 (BoolValue 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p67 (format #f "~A" b67))
(format #t "p67 = >>~A<<\n" p67)
(test-assert "p67"
	(equal? p67 "(BoolValue #x7e01ee63012345678)\n"))

(define b68 (BoolValue 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p68 (format #f "~A" b68))
(format #t "p68 = >>~A<<\n" p68)
(test-assert "p68"
	(equal? p68 "(BoolValue #xfe01ee63012345678)\n"))

(define b69 (BoolValue 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 0 0))
(define p69 (format #f "~A" b69))
(format #t "p69 = >>~A<<\n" p69)
(test-assert "p69"
	(equal? p69 "(BoolValue #x1fe01ee63012345678)\n"))

(define b101 (BoolValue 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1
0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1
0 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1
1 0 0 0 0))
(define p101 (format #f "~A" b101))
(format #t "p101 = >>~A<<\n" p101)
(test-assert "p101"
	(equal? p101 "(BoolValue #x1fe01ee630123456789abcdef0)\n"))

(define b117 (BoolValue 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1
0 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1
0 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1
1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p117 (format #f "~A" b117))
(format #t "p117 = >>~A<<\n" p117)
(test-assert "p117"
	(equal? p117 "(BoolValue #x1fe01ee630123456789abcdef03210)\n"))

(define b126 (BoolValue
1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p126 (format #f "~A" b126))
(format #t "p126 = >>~A<<\n" p126)
(test-assert "p126"
	(equal? p126 "(BoolValue #x369fe01ee630123456789abcdef03210)\n"))

(define b127 (BoolValue
1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p127 (format #f "~A" b127))
(format #t "p127 = >>~A<<\n" p127)
(test-assert "p127"
	(equal? p127 "(BoolValue #x769fe01ee630123456789abcdef03210)\n"))

(define b128 (BoolValue
1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p128 (format #f "~A" b128))
(format #t "p128 = >>~A<<\n" p128)
(test-assert "p128"
	(equal? p128 "(BoolValue #xf69fe01ee630123456789abcdef03210)\n"))

(define b129 (BoolValue
0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p129 (format #f "~A" b129))
(format #t "p129 = >>~A<<\n" p129)
(test-assert "p129"
	(equal? p129 "(BoolValue #x0f69fe01ee630123456789abcdef03210)\n"))

(define b132 (BoolValue
0 1 1 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p132 (format #f "~A" b132))
(format #t "p132 = >>~A<<\n" p132)
(test-assert "p132"
	(equal? p132 "(BoolValue #x6f69fe01ee630123456789abcdef03210)\n"))

(define b133 (BoolValue
1 0 0 0 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p133 (format #f "~A" b133))
(format #t "p133 = >>~A<<\n" p133)
(test-assert "p133"
	(equal? p133 "(BoolValue #x10f69fe01ee630123456789abcdef03210)\n"))

(define b136 (BoolValue
0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0))
(define p136 (format #f "~A" b136))
(format #t "p136 = >>~A<<\n" p136)
(test-assert "p136"
	(equal? p136 "(BoolValue #x00f69fe01ee630123456789abcdef03210)\n"))

(define b140 (BoolValue
0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0))
(define p140 (format #f "~A" b140))
(format #t "p140 = >>~A<<\n" p140)
(test-assert "p140"
	(equal? p140 "(BoolValue #x00f69fe01ee630123456789abcdef032100)\n"))

(define b141 (BoolValue
1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1
1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 0 1
1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0
0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0))
(define p141 (format #f "~A" b141))
(format #t "p141 = >>~A<<\n" p141)
(test-assert "p141"
	(equal? p141 "(BoolValue #x100f69fe01ee630123456789abcdef032100)\n"))

(test-end tname)

(opencog-test-end)
