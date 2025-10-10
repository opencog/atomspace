;;
;; The matching solution, and the bind link are in the middle 
;; of this file, so that the pattern matcher doesn't accidentally
;; start searching with the correct solution first, just because
;; we loaded it into the atomspace first. Its down in the middle
;; of this file ...

;; this should not match.
(InheritanceLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(AtTimeLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(PrepositionalRelationshipNode "Big Red Button")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(FeatureNode "here kitty kitty")
	(EvaluationLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(ConceptNode "big idea")
	(SetLink
		(FeatureNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; Should match to this, and only this.
;; Put this in the middle of the file, so that its kind-of
;; randomized in the atomspace -- i.e. not at the beginning (lowest
;; handle uuid's) nor at the end (highest handle numbers) of the
;; atomspace
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(FeatureLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(WordNode "bird is the word")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(SemeNode "ActivationModulatorUpdater")
	(ConceptNode "big idea")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(HebbianLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

(define (bind_ou)
	(CollectionOf
	(QueryLink
		;; variable decls
		(VariableList
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
		;; body
		(MemberLink
			(VariableNode "$var_schema")
			(VariableNode "$var_number")
			(SetLink
				(VariableNode "$var_schema")
				(ListLink
					(LemmaNode "thing1")
					(LemmaNode "thing2")
					(LemmaNode "thing3")
				)
			)
		)
		;; implicand -- result
		(ListLink
			(VariableNode "$var_number")
			(VariableNode "$var_schema")
		)
	)
	)
)

;; this should not match.
(SubsetLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(WordNode "bird is the word")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(ParseLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(GroundedSchemaNode "ActivationModulatorUpdater")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing ohh noo")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

;; this should not match.
(MemberLink
	(PrepositionalRelationshipNode "Big Red Button")
	(NumberNode "0.24")
	(SetLink
		(GroundedSchemaNode "ActivationModulatorUpdater")
		(ListLink
			(LemmaNode "thing1")
			(LemmaNode "thing2")
			(LemmaNode "thing3")
		)
	)
)

