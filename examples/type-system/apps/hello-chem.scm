;
; Example chemistry app, in scheme
;
(use-modules (opencog) (opencog exec))
(use-modules (opencog demo-types))

(define dihydrogen-monoxide
	(Molecule
		(SB
			(O "big man oxygen")
			(H "one proton"))
		(SB
			(O "big man oxygen")
			(H "another proton"))
	))

(format #t "Look ma! See what I made! This: ~A\n" dihydrogen-monoxide)

(define radioactive (Carbon14Node "wood"))

(define decay-products (cog-execute! radioactive))

(format #t "The C14 decay products are:\n~A\n" decay-products)

; The end!
; That's all folks!
