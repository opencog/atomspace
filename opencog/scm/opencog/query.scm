;
; OpenCog Pattern matcher module
;

(define-module (opencog query))

; We need this to set the LTDL_LIBRARY_PATH
(use-modules (opencog))
(use-modules (opencog exec))

(define-public (cog-bind handle)
	(display "Obsolete! Do not use cog-bind, use cog-execute! instead.\n")
	(cog-execute! handle)
)
(define-public (cog-recognize handle)
	(display "Obsolete! Do not use cog-recognize, use cog-execute! instead.\n")
	(cog-execute! handle)
)
(define-public (cog-satisfy handle)
	(display "Obsolete! Do not use cog-satisfy, use cog-evaluate! instead.\n")
	(cog-evaluate! handle)
)
(define-public (cog-satisfying-set handle)
	(display "Obsolete! Do not use cog-satisfying-set, use cog-execute! insead.\n")
	(cog-execute! handle)
)
