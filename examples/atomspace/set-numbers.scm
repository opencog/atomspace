;; Construct Zermelo and Von Neumann numbers

;; Set constructor. We use List instead of Set because this link
;; should be discouraged or used with caution, and doesn't change the
;; underlying constructions.
(define SC List)

(define (dec n)
  (- n 1))

(define (zermelo n)
"
  Construct the first n+1 Zermelo numbers

  (zermelo 0) -> (SC)
  (zermelo n) -> (SC (zermelo n-1))
"
  (if (= n 0)
      (SC)
      (SC (zermelo (dec n)))))

(define (out-n-self x)
"
  Given an atom x, create a set with its outgoing set and itself
"
  (SC (cog-outgoing-set x) x))

(define (von-neumann n)
"
  Construct the first n+1 Von Neumann numbers

  (von-neumann 0) -> (SC)
  (von-neumann n) -> (SC (von-neumann n-1)) union (von-neumann n-1)
"
  (if (= n 0)
      (SC)
      (out-n-self (von-neumann (dec n)))))

;; Examples

;; The following
;;
;; (zermelo 3)
;;
;; is expected to return
;;
;; (ListLink
;;    (ListLink
;;       (ListLink
;;          (ListLink
;;          )
;;       )
;;    )
;; )

;; The following
;;
;; (von-neumann 3)
;;
;; is expected to return
;;
;; (ListLink
;;    (ListLink
;;    )
;;    (ListLink
;;       (ListLink
;;       )
;;    )
;;    (ListLink
;;       (ListLink
;;       )
;;       (ListLink
;;          (ListLink
;;          )
;;       )
;;    )
;; )
