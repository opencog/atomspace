;
; flat-stream-test.scm -- Test FlatStream flattening behavior
;
; FlatStream takes a stream source that returns LinkValues and flattens
; them by returning one item at a time. Each access to the stream via
; printing or comparison advances the stream state.
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog test-runner))

(opencog-test-runner)
(define tname "flat-stream-basic-test")
(test-begin tname)

; Create a nested LinkValue structure with two inner LinkValues
(define syn
   (LinkValue
      (LinkValue
         (FloatValue 1 2 3)
         (FloatValue 4 5 6))

      (LinkValue
         (FloatValue 7 8 9)
         (FloatValue 10 11 12))))

; Attach it to an atom
(cog-set-value! (Anchor "foo") (Predicate "bar") syn)

; Create a FlatStream that will flatten the values
(define fs (FlatStream (ValueOf (Anchor "foo") (Predicate "bar"))))

; Expected values for comparison
(define first-item (LinkValue (FloatValue 1 2 3) (FloatValue 4 5 6)))
(define second-item (LinkValue (FloatValue 7 8 9) (FloatValue 10 11 12)))

; First access - should return first item
(format #t "Stream first access: ~A\n" fs)
(test-assert "first-access-match" (equal? fs first-item))

; Second access - should return second item (not equal to first)
(format #t "Stream second access: ~A\n" fs)
(test-assert "second-access-nomatch" (not (equal? fs first-item)))
(test-assert "second-access-match" (equal? fs second-item))

; Third access - should wrap around to first item
; It wraps because after the second item, the end of the LinkValue
; is reached, so FlatStream calls the ValueOf to get "the next list".
; It gets the next list, and then cycles like this, forever.
(format #t "Stream third access: ~A\n" fs)
(test-assert "third-access-match" (equal? fs first-item))

; Fourth access - should be second item again
(format #t "Stream fourth access: ~A\n" fs)
(test-assert "fourth-access-nomatch" (not (equal? fs first-item)))
(test-assert "fourth-access-match" (equal? fs second-item))

(test-end tname)

(opencog-test-end)
