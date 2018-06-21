;; Construct Zermelo and Von Neumann numbers

(define (zermelo n)
"
  Construct the first n+1 Zermelo numbers

  (zermelo 0) -> (Set)
  (zermelo n) -> (Set (zermelo n-1))
"
  (if (= n 0)
      (Set)
      (Set (zermelo (+ n -1)))))

(define (von-neumann n)
"
  Construct the first n+1 Von Neumann numbers

  (von-neumann 0) -> (Set)
  (von-neumann n) -> (Set (von-neumann n-1)) union (von-neumann n-1)
"
  (if (= n 0)
      (Set)
      (let* ((dec-n (+ n -1))
             (dec-vn (von-neumann dec-n))
             (dec-vn-mbrs (cog-outgoing-set dec-vn)))
        (Set dec-vn dec-vn-mbrs))))

;; Examples

;; The following
;;
;; (zermelo 3)
;;
;; is expected to return
;;
;; (SetLink
;;    (SetLink
;;       (SetLink
;;          (SetLink
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
;; (SetLink
;;    (SetLink
;;    )
;;    (SetLink
;;       (SetLink
;;       )
;;    )
;;    (SetLink
;;       (SetLink
;;       )
;;       (SetLink
;;          (SetLink
;;          )
;;       )
;;    )
;; )
