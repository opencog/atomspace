; Function to substitute variables by arguments in a given atom or
; more specifically forAllLink
;
; Author Nil Geisweiller <ngeiswei@gmail.com>
;

;------------------------------------------------------------------------
; Get the list of variables from forAll, recalling that
; ForAllLink
;    VariableList X1,...,Xn
;    Body
(define-public (get-variables forAll) (cog-outgoing-set (gar forAll)))
; Get the body from forAll
(define-public (get-body forAll) (gadr forAll))
; Return a list of pairs (variable . atom) given a forAllLink and a
; list of atoms corresponding to arguments.
(define-public (get-bindings forAll arguments)
    (map (lambda (v a) (cons v a)) (get-variables forAll) arguments))

;------------------------------------------------------------------------
; Substitute variables by arguments in body given bindings
(define-public (substitute-var bindings body)
  (let ((body_type (cog-type body)))
    (cond ; recursive case
          ((cog-subtype? 'VariableList body_type) (VariableList))
          ((cog-subtype? 'Link body_type)
           (apply cog-new-link
                  (cons body_type
                        (map (lambda (child) (substitute-var bindings child))
                             (cog-outgoing-set body)))))
          ; base cases
          ((cog-subtype? 'VariableNode body_type) (cdr (assoc body bindings)))
          (else body))))
; Perform universal instantiation given ForAll atom and a list of
; atoms representing the arguments
(define-public (universal-instantiate forAll arguments)
  (substitute-var (get-bindings forAll arguments) (get-body forAll)))
