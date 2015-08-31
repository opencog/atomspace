;
; Simple goal solving using redex's
;
;; XXX under construction and broken.
;
(use-modules (opencog))
(use-modules (opencog query))

;;; Assert basic fact
;;;  |- likes(Tom, baseball) 
(EvaluationLink
	(PredicateNode "likes")
	(ListLink
		(ConceptNode "Tom")
		(ConceptNode "baseball")
	)
)

;;; Assert implication
;;;   |- likes(Tom,$X) -> likes(Bill, $X) 
;;; The ImplicationLink is a declarative form of the above.
(ImplicationLink
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Tom")
			(VariableNode "$X")))
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Bill")
			(VariableNode "$X"))))

;;; The equivalent imperative form of the above.
(BindLink
	(VariableNode "$X")
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Tom")
			(VariableNode "$X")))
	(EvaluationLink
		(PredicateNode "likes")
		(ListLink
			(ConceptNode "Bill")
			(VariableNode "$X"))))

;;; Same as above, but in imperative form. it uses the GetLink
;;; to search the atomspace to find everything Tom likes, and then
;;; uses the PutLink to perform a beta-reduction, to plug in those
;;; answers into a template for the things that Bill likes.
;;; Note the use of two distinct variables; $X is bound to GetLink;
;;; basically, $X is the return value from GetLink. The $Y variable
;;; is bound to PutLink, and functions as a classical lambda-calculus
;;; lambda, defining the arguments that PutLink accepts.
(define implication
	(PutLink
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Bill")
				(VariableNode "$Y")))
		(GetLink
			(EvaluationLink
				(PredicateNode "likes")
				(ListLink
					(ConceptNode "Tom")
					(VariableNode "$X"))))))

;; This causes the implication to be performed.
(cog-execute! implication)

;;; Question to be answered: is it true that likes(Bill, baseball)?
;;; i.e. can we show that |- likes(Bill, baseball)

;;;
;;; A named satisfiability query: Does Bill like $X?
;;; The EvaluationLink just asserts that "Bill does like X" (as a fact).
;;; The SatisfactionLink turns it into a question: the SatisfactionLink
;;; can evaluate to true or false, depending on what X is.
;;; Note that the SatisfactionLink is in the form of a lambda; that is,
;;; it has the form  Lx.(Bill likes X)
;;;
;;; Finally, we want to give the above a name, so that we can refer to it
;;; in other places. We use the DefineLink to do this. Given a lambda,
;;; for example, Lx.(stuff) which is just an anonymous function taking x,
;;; the DefineLink turns it into a named function: so that 
;;;    Define name Lx.(stuff)
;;; is the same as 
;;;    (name x).(stuff)
;;;
;;; So, below, the DefineLink merely gives the question a name: It just
;;; says that there is a particular question, which is given the name 
;;; "Does Bill like X?", and that this question takes a single variable
;;; i.e $X.  We infer this variable from the SatisfactionLink.  That is,
;;; the DefineLink binds exactly the same variables that the lambda under
;;; it does (with SatisfactionLink being the lambda).
(DefineLink
	(DefinedPredicateNode "Does Bill like X?")
	(SatisfactionLink
		(VariableNode "$X")
		(EvaluationLink
			(PredicateNode "likes")
			(ListLink
				(ConceptNode "Bill")
				(VariableNode "$X")))))

;;; A satisfiability question: Does Bill like X where X is baseball?
(MemberLink
	(ConceptNode "baseball")
	(DefinedPredicateNode "Does Bill like X?")
)

;; solution:
;; do plain member link, get false,
;; look for  sat link body as second half of implication
;; pattern match first half of implication, if found
;; try to check member again.

(cog-evaluate! (DefinedPredicateNode "Does Bill like X?"))
(cog-satisfy (DefinedPredicateNode "Does Bill like X?"))

;; A quasi-generic rule implicator.
;; Searches for all implication links (of a very specific form)
;; and converts them into GetPut imperatives.
(PutLink
	(VariableList
		(VariableNode "$tp")
		(VariableNode "$fp")
		(VariableNode "$aaa")
		(VariableNode "$bbb")
		(VariableNode "$vvv")
	)
	(PutLink
		(EvaluationLink
			(VariableNode "$tp")
			(ListLink
				(VariableNode "$bbb")
				(VariableNode "$vvv")))
		(GetLink
			(EvaluationLink
				(VariableNode "$fp")
				(ListLink
					(VariableNode "$aaa")
					(VariableNode "$vvv")))))

	;; Search for ImplicationLinks, and disect them.
	(GetLink
		(VariableList
			(TypedVariableLink (VariableNode "$fpred") (TypeNode "PredicateNode"))
			(TypedVariableLink (VariableNode "$tpred") (TypeNode "PredicateNode"))
			(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
			(TypedVariableLink (VariableNode "$B") (TypeNode "ConceptNode"))
			(TypedVariableLink (VariableNode "$V") (TypeNode "VariableNode"))
		)
		(ImplicationLink
			(EvaluationLink
				(VariableNode "$fpred")
				(ListLink
					(VariableNode "$A")
					(VariableNode "$V")))
			(EvaluationLink
				(VariableNode "$tpred")
				(ListLink
					(VariableNode "$B")
					(VariableNode "$V"))))))

;; Same as above, but using BindLink, so order is reversed.
(BindLink
	;; Search for ImplicationLinks, and disect them.
	(VariableList
		(TypedVariableLink (VariableNode "$fpred") (TypeNode "PredicateNode"))
		(TypedVariableLink (VariableNode "$tpred") (TypeNode "PredicateNode"))
		(TypedVariableLink (VariableNode "$A") (TypeNode "ConceptNode"))
		(TypedVariableLink (VariableNode "$B") (TypeNode "ConceptNode"))
		(TypedVariableLink (VariableNode "$V") (TypeNode "VariableNode"))
	)
	(ImplicationLink
		(EvaluationLink
			(VariableNode "$fpred")
			(ListLink
				(VariableNode "$A")
				(VariableNode "$V")))
		(EvaluationLink
			(VariableNode "$tpred")
			(ListLink
				(VariableNode "$B")
				(VariableNode "$V"))))

	; If an ImplicationLink was found, create a matching BindLink
	(BindLink
		(VariableNode "$V")
		(EvaluationLink
			(VariableNode "$fpred")
			(ListLink
				(VariableNode "$A")
				(VariableNode "$V")))
		(EvaluationLink
			(VariableNode "$tpred")
			(ListLink
				(VariableNode "$B")
				(VariableNode "$V")))))

(cog-bind (gar (cog-bind x)))
