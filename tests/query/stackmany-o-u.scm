
;; The matching solution, and the bind link are in the middle 
;; of this file, so that the pattern matcher doesn't accidentally
;; start searching with the correct solution first, just because
;; we loaded it into the atomspace first. Its down in the middle
;; of this file ...

;; Just like stackmore-o-u.scm except that there are 4 items
;; in the unordered link

;; this should not match.
(InheritanceLink
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(AtTimeLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(PrepositionalRelationshipNode "Big Red Button")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(FeatureNode "here kitty kitty")
	(EvaluationLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(ConceptNode "big idea")
	(SetLink
		(FeatureNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; Should match to this, and only this.
;; Put this in the middle of the file, so that its kind-of
;; randomized in the atomspace -- i.e. not at the begining (lowest
;; handle uuid's) nor at the end (highest handle numbers) of the
;; atomspace
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(FeatureLink
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(WordNode "bird is the word")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SemeNode "ActivationModulatorUpdater")
	(ConceptNode "big idea")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(HebbianLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

(define (many_ou)
	(BindLink
		;; variable decls
		(VariableList
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
		;; body
		(MemberLink             ; TODO this MemberLink is ill-formed
			(VariableNode "$var_schema")
			(VariableNode "$var_number")
			(SetLink
				(LemmaNode "thing2")
				(LemmaNode "thing3")
				(VariableNode "$var_schema")
				(LemmaNode "thing1")
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
	)
)

;; this should not match.
(SubsetLink
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(WordNode "bird is the word")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(ParseLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(SchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing ohh noo")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

;; this should not match.
(MemberLink                             ; TODO this MemberLink is ill-formed
	(PrepositionalRelationshipNode "Big Red Button")
	(NumberNode "0.24")
	(SetLink
		(SchemaNode "ActivationModulatorUpdater")
		(LemmaNode "thing1")
		(LemmaNode "thing2")
		(LemmaNode "thing3")
	)
)

