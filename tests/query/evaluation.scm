;
; Basic unit testing for different ways of nesting evaluatable links.
; This uses `IdenticalLink` for all testing. See `eval-equal.scm` for
; a version that uses `EqualLink`.
;
(use-modules (opencog))
(use-modules (opencog exec))

;;; Populate the atomspace with a cover of a directed bouquet
;;; of four circles. A bouquet has just one vertex, and so all
;;; nodes project down to just that one vertex in the base space.
;;; There are directed arrows pointing, in the node order, from
;;; one to the next, in the covering space.
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea one"))

(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea two"))
(AssociativeLink (ConceptNode "idea two") (ConceptNode "idea one"))

(AssociativeLink (ConceptNode "idea two") (ConceptNode "idea three"))
(AssociativeLink (ConceptNode "idea three") (ConceptNode "idea one"))

(AssociativeLink (ConceptNode "idea three") (ConceptNode "idea four"))
(AssociativeLink (ConceptNode "idea four") (ConceptNode "idea one"))

(AssociativeLink (ConceptNode "idea four") (ConceptNode "idea five"))
(AssociativeLink (ConceptNode "idea five") (ConceptNode "idea one"))

(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea three"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea four"))
(AssociativeLink (ConceptNode "idea one") (ConceptNode "idea five"))

;;; The recurring core of all the tests

(define  one->x
	(AssociativeLink
		(ConceptNode "idea one")
		(VariableNode "$x")
	)
)
(define x->one
	(AssociativeLink
		(VariableNode "$x")
		(ConceptNode "idea one")
	)
)

(define (wrapper core)
	(BindLink
		(VariableNode "$x")
		(AndLink core)
		(VariableNode "$x")
	)
)

;;; Explore the connectivity of the graph

;; All five nodes are bi-connected.
(define (five-arcs)
	(wrapper (list one->x x->one))
)

;; Reject all but one arc
(define (one-arc-one)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
		)
	)
)

(define (one-arc-three)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
		)
	)
)

;;; conflict thus not satisfiable $x cannot be 3 and 4 at the same time
(define (zero-arcs)
	(wrapper
		(list one->x x->one
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
		)
	)
)

;;;; -----------------------------------------------------------------
;;; Boolean logic connectives!

;; reject node three only; of the five, four remain
(define (four-arcs)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			)
		)
	)
)

;; reject three nodes; of the five, two remain
(define (two-arcs)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
			)
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
			)
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea five"))
			)
		)
	)
)

;; reject node one only; of the five, four remain
(define (four-not)
	(wrapper
		(list one->x x->one
			(NotLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
			)
		)
	)
)

;; accept either of two.
(define (two-or)
	(wrapper
		(list one->x x->one
			(OrLink
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
				(IdenticalLink (VariableNode "$x") (ConceptNode "idea two"))
			)
		)
	)
)

;; accept neither of two. (accept three)
(define (three-nor)
	(wrapper
		(list one->x x->one
			(NotLink
				(OrLink
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea one"))
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea two"))
				)
			)
		)
	)
)

;; accept any of the first two;
(define (two-fancy)
	(wrapper
		(list one->x x->one
			(AndLink
				(NotLink
					(IdenticalLink (VariableNode "$x") (ConceptNode "idea three"))
				)
				(NotLink
					(OrLink
						(IdenticalLink (VariableNode "$x") (ConceptNode "idea four"))
						(IdenticalLink (VariableNode "$x") (ConceptNode "idea five"))
					)
				)
			)
		)
	)
)
