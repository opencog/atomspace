;
; Example of calling the wrapped c++ code.
; Run this example by starting guile, and then cut-n-pasting the
; code below. Alternately, run `guile -l run-example.scm`
;

; By default, guile modules are installed at
;    /usr/local/share/guile/site/3.0
; which is one of the paths that appear in the %load-path guile
; variable. However, since we are not installing this example, we
; need to tell guile where to find it.

(add-to-load-path "../examples/c++-guile")

; Print the list of load paths
%load-path

; Alternately, the below will also work.
; (add-to-load-path (string-append (getcwd) "../examples/c++-guile"))

; Now, load the modules, as normal.
(use-modules (opencog))
(use-modules (opencog example))

; Call the two wrapped functions, found in `ExampleSCM.cc`
(hey-print (Concept "a"))
(hey-printmore (Concept "a"))

; A fancier example, showing that `hey-printmore` really does return
; the TruthValue.
(define b (Concept "bbb" (SimpleTruthValue 0.6 0.8)))
(hey-printmore b)

; The End. That's all folks!
