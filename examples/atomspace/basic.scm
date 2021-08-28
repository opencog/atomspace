;
; basic.scm -- Basic guile usage example!
;
; See `opencog/guile/README` or http://wiki.opencog.org/w/Scheme
; for additional documentation.
;
; If you have installed opencog, then start guile simply by saying
; "guile" at the bash prompt $.  Otherwise, you will need to do this:
;
; $ guile -L opencog/scm -L build
;
; where "build" is where-ever you built opencog.
;
; Another possibility: add stuff to your ~/.guile file, for example:
; (add-to-load-path "/home/yourname/opencog/build")
; (add-to-load-path "/home/yourname/opencog/opencog/scm")
;

(use-modules (opencog))

; Create an atom and place it in the default AtomSpace.
(ConceptNode "asdf")

; Access this newly created atom.
(cog-node 'ConceptNode "asdf")

; Access an atom that does not exist.
(cog-node 'ConceptNode "qwerty")

; Run the following REPL commands
; * Print a list of all (currently loaded) opencog functions.
; ,apropos cog

; * Get the documentation of a a given function
; ,describe cog-new-node
; ,describe cog-node
