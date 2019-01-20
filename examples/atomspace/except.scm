;
; except.scm -- Catching exceptions from bad code.
;
; Code that is invoked by ExecutionOutputLink can be buggy; exceptions
; will be thrown. This exceptions can be caught and handled in scheme.
;
; See `execute.scm` for more `cog-execute!` examples.
;

(use-modules (opencog) (opencog exec))

; First, just give it some broken junk.  See what happens.
(cog-execute!
   (ExecutionOutput
      (GroundedSchema "py:b0rk3n_junk")
      (List
         (Concept "1")
         (Concept "2"))))

; C++ exceptions are converted into scheme exceptions, and can be
; caught, as usual.
(catch
   #t
   (lambda ()
      (cog-execute!
         (ExecutionOutput
            (GroundedSchema "py:b0rk3n_junk")
            (List
               (Concept "1")
               (Concept "2")))))
   (lambda (key . args)
      (display "Ohhh noooo Mr. Bill!!! ") (display key)
      (newline)
      (display "Sluggo says to ... ") (display args)
      (newline) (newline)
   ))


; Exception-producing code, but for mal-formed scheme.
;
(cog-execute!
   (ExecutionOutput
      (GroundedSchema "scm:(((((uber-badf")
      (List
         (Concept "1")
         (Concept "2"))))
