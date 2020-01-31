;
; OpenCog Pattern matcher module
;

(define-module (opencog query))

(use-modules (opencog))
(use-modules (opencog exec))

(display "\n")
(display "====> Attention!\n")
(display "====> Obsolete! You do not need the `(opencog query)` module.\n")
(display "====> Please use `(opencog exec)` and `cog-execute!` instead.\n")
(display "\n")

(define-public (cog-bind handle)
"
   cog-bind - Obsolete!
   Do not use cog-bind, use cog-execute! instead.
"
	(display "Obsolete! Do not use cog-bind, use cog-execute! instead.\n")
	(cog-execute! handle)
)

(define-public (cog-recognize handle)
"
   cog-recognize - Obsolete!
   Do not use cog-recognize, use cog-execute! instead.
"
	(display "Obsolete! Do not use cog-recognize, use cog-execute! instead.\n")
	(cog-execute! handle)
)

(define-public (cog-satisfy handle)
"
   cog-satisfy - Obsolete!
   Do not use cog-satisfy, use cog-execute! instead.
"
	(display "Obsolete! Do not use cog-satisfy, use cog-evaluate! instead.\n")
	(cog-evaluate! handle)
)
(define-public (cog-satisfying-set handle)
"
   cog-satisfying-set - Obsolete!
   Do not use cog-satisfying-set, use cog-execute! instead.
"
	(display "Obsolete! Do not use cog-satisfying-set, use cog-execute! insead.\n")
	(cog-execute! handle)
)
