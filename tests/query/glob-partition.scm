;
; glob-partition.scm
;
; Reported in issue #1825
;
(use-modules (opencog) (opencog exec))

(define partition

(BindLink
	(VariableList
		(TypedVariableLink
			(GlobNode "$begin")
			(IntervalLink (NumberNode 0) (NumberNode -1)))
		(TypedVariableLink
			(GlobNode "$end")
			(IntervalLink (NumberNode 0) (NumberNode -1))))

	; The Concept foo should be found in four different locations.
	(List
		(GlobNode "$begin")
		(Concept "foo")
		(GlobNode "$end"))
	(OrderedLink
		(Concept "begin")
		(GlobNode "$begin")
		(Concept "end")
		(GlobNode "$end")))
)

; Data
(List (Concept "foo")(Concept "foo")(Concept "foo")(Concept "foo"))

; ----------------------------------------------------
(define part-deeper

(BindLink
	(VariableList
		(TypedVariableLink
			(GlobNode "$begin")
			(IntervalLink (NumberNode 0) (NumberNode -1)))
		(TypedVariableLink
			(GlobNode "$end")
			(IntervalLink (NumberNode 0) (NumberNode -1))))

	; The Concept foo should be found in four different locations.
	(List (List
		(GlobNode "$begin")
		(Concept "bar")
		(GlobNode "$end")))
	(OrderedLink
		(Concept "begin")
		(GlobNode "$begin")
		(Concept "end")
		(GlobNode "$end")))
)

; Data
(List (List (Concept "bar")(Concept "bar")(Concept "bar")(Concept "bar")))
