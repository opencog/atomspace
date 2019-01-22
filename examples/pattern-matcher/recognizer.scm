;
; recognizer.scm -- pattern recognition
;
; Pattern recognition is dual to pattern matching!
; AKA the "dynamic Rete algorithm".
;
; When designing a rule engine, one is (eventually) faced with the task
; of determining which rules can be applied at some given point of the
; calculations.  One can blindly try all of the rules, and see which
; ones fire.  When there are more than a few dozen rules, this becomes
; impractical.  This issue was observed, and resolved in the 1970's and
; 1980's, with the Rete algorithm: one organizes the set of rules into
; a Trie, which is then very easily and quickly walked, to determine
; which ones can fire.
;
; OpenCog and the AtomSpace do NOT (explicitly) implement Tries or Rete!
; However, the general (hyper-)graph structure of OpenCog Atoms already
; contains enough connectivity information to accomplish more or less
; the same thing: a kind-of "dynamic Rete", where rule-sets can be
; searched for, on-demand, at runtime.
;
; The core idea is that pattern recognition is dual to pattern matching.
; If we define "pattern matching" as the idea of finding all data that
; matches a pattern, then "pattern recognition" is the act of finding
; all patterns that match the data.  In pattern matching, where a
; pattern has variables in it, one finds all groundings (concrete values
; for the variables) that cause the pattern expression to evaluate to
; "true". In pattern recognition, one has a single "grounding", and asks
; for all pattern expressions that evaluate to "true" when applied to
; this grounding.
;
; The Atomese construct for accomplishing this is the DualLink. It is
; roughly the opposite of the GetLink, "opposite" in the cat-theory
; sense of reversing arrows.  Its "rough", in that the type constraints
; that are possible in GetLink are ignored by the DualLink.
;
; The example below is based on an AIML-like search, simply because
; this is easy to explain and demonstrate. Note that all AIML chatbots
; maintain a Trie of AIML rules, and so AIML is a "natural" example of
; pattern recognition.  The Atomese DualLink is, however, a general
; pattern recognizer: it can be used in a general setting, not just
; for AIML-like structures.
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog exec))

; Two different pseudo-AIML rules:
;    I * you   --> I * you too
;    I love *  --> I like * a lot!
;
(BindLink
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you"))
	(ListLink
		(Concept "I") (Glob "$star") (Concept "you") (Concept "too")))

(BindLink
	(ListLink
		(Concept "I")
		(Concept "love")
		(Glob "$star"))
	(ListLink
		(Concept "I")
		(Concept "like")
		(Glob "$star")
		(Concept "a")
		(Concept "lot!")))

; ---------------------------------------------------------------------
;; A pretend "sentence" that is the "input".
(define sent
	(ListLink (Concept "I") (Concept "love") (Concept "you")))

;; Search for patterns that match the sentence. Both of the above
;; should match.
(cog-execute! (DualLink sent))

;; The above should return the below.  Note how fragments of the above
;; BindLink's are matched.
(SetLink
	(ListLink
		(Concept "I")
		(Glob "$star")
		(Concept "you")
	)
	(ListLink
		(Concept "I")
		(Concept "love")
		(Glob "$star")
	)
)

; ---------------------------------------------------------------------
;; Another sentence, but with adverbs.  It will match one of the
;; patterns, but not the other.
(define adv-sent
	(ListLink
		(Concept "I")
		(Concept "really")
		(Concept "truly")
		(Concept "love")
		(Concept "you")))

;; Perform the search.
(cog-execute! (DualLink adv-sent))

; ---------------------------------------------------------------------
; At this point, one will typically want to know the full rule to which
; the dual is just a fragment of.  That is, the DualLink just returns
; the antecedent of a rule, not the rule itself; one wants the full rule,
; so that it can be applied.
;
; This can be done in several ways: one could write pure scheme code
; (or C++ or python) to trace the incoming set of the antecedent and
; find every rule its embedded in. This isn't hard to do: just use the
; `cog-chase-link` utility.  Alternately, one can write the entire
; thing in pure Atomese. This second option is explored below.  There's
; no particularly strong reason to do it this way.  It will seem a bit
; complicated, and will be slower than a pure-scheme solution. But its
; illustrative of what one can do with Atomese, so we do that here.
;
; To work up to the final, working example, several simpler expressions
; are constructed first.

; ------------------

(define (get-consequents ANTECEDENT)
"
  get-consequents ANTECEDENT -- given the ANTECEDENT, return a set of
  all of the consequents of a rule.

  Example usage:
     (get-consequents (List (Concept \"I\") (Glob \"$star\") (Concept \"you\")))
"

	; Accept only consequents that are ListLink's
	(cog-execute!
		(GetLink
			(TypedVariable (Variable "$consequent") (Type "ListLink"))
			(Quote (BindLink ANTECEDENT (Unquote (Variable "$consequent"))))
		)
	)
)

; Try it!
; (get-consequents (List (Concept "I") (Glob "$star") (Concept "you")))

; ------------------
; The below takes the above GetLink, and uses it to generate values
; that are then installed in place by a PutLink. That is, the PutLink
; re-assembles the rule from the antecedent and the consequent.

(define (get-rules-for-ante ANTECEDENT)
"
  get-rules-for-ante ANTECEDENT -- given the ANTECEDENT, return a set
  of all rules that can be applied to it.

  Example usage:
     (get-rules-for-ante (List (Concept \"I\") (Glob \"$star\") (Concept \"you\")))
"
	; The GetLink below returns all of the consequents.
	; The TypedVariable filters out and rejects all consequents that
	;   are not ListLinks.
	; The PutLink reconstructs the rule, out of the antecedent and
	;   the consequent.
	; The Quotes are used to avoid accidentally running the BindLink
	;   that is being assembled.
	(cog-execute!
		(Put
			(TypedVariable (Variable "$list") (Type "ListLink"))
			(Quote (Bind ANTECEDENT (Unquote (Variable "$list"))))
			(Get
				(Variable "$consequent")
				(Quote (BindLink ANTECEDENT (Unquote (Variable "$consequent"))))
			)
		)
	)
)

; Try it!
; (get-rules-for-ante (List (Concept "I") (Glob "$star") (Concept "you")))

; ------------------
; The below takes the above rule generator, and turns it into a
; bona-fide rule recognizer.  It does this by running the DualLink to
; find the antecedents, and then using a PutLink to plug these into
; the rule generator.

(define (get-untyped-rules DATA)
"
  get-untyped-rules DATA -- given the graph DATA, return a set of all
  rules that can be applied to it.

  Example usage:
     (get-untyped-rules (List (Concept \"I\") (Concept \"love\") (Concept \"you\")))
"
	(cog-execute!
		(Put
			(TypedVariable (Variable "$ante") (Type "ListLink"))
			(Put
				(TypedVariable (Variable "$list") (Type "ListLink"))
				(Quote (BindLink
					(Unquote (Variable "$ante"))
					(Unquote (Variable "$list"))
				))
				(GetLink
					(Variable "$consequent")
					(Quote (BindLink
						(Unquote (Variable "$ante"))
						(Unquote (Variable "$consequent"))))
				))
			(Dual DATA)
		))
)

; Try it!
; (get-untyped-rules (List (Concept "I") (Concept "love") (Concept "you")))

; ---------------------------------------------------------------------
; To complete the example, one typically needs to run the rules after
; finding them. We do this below, in an ad-hoc fashion.  The primary
; impediment is that each rule is wrapped with a SetLink, and needs to be
; unwrapped. You can see this wrapping in the "Try it!" result, above.
;

; ruleset is the unwrapped rules.  Use cog-outgoing-set to unwrap them.
(define (unwrap-rules RULES)
   (fold
		(lambda (s li) (cons (car (cog-outgoing-set s)) li))
		'()
		(cog-outgoing-set RULES)))

(define ruleset
	(unwrap-rules
		(get-untyped-rules (List (Concept "I") (Concept "love") (Concept "you")))))

(map cog-execute! ruleset)

; When the above code is executed, you will get back a bunch of
; sentences: that's because the AtomSpace already contains a bunch of
; sentences in it, and the rule-set will find ALL of them, and get
; applied to all of them.  To avoid this behavior, you need to tag
; the one sentence you are interested in in some way,  e.g. by using
; a link to connect it to `(AnchorNode "the current sentence")` and
; then including the AnchorNode in each AIML rule.
;
;-------------------------------------------------------

; A pattern with two globs in it.
; The types of the globs are constrained, because, if not constrained,
; the globs can sometimes pick up on parts of the various patterns
; created above.  We really want them to only pick up on the "sentences"
; (strings of Concepts).
(define a-love-b
	(BindLink
		(VariableList
			(TypedVariable (Glob "$A") (Type "ConceptNode"))
			(TypedVariable (Glob "$B") (Type "ConceptNode")))
		(ListLink
			(Glob "$A")
			(Concept "loves")
			(Glob "$B"))
		(ListLink
			(Concept "I'm")
			(Concept "sure")
			(Concept "that")
			(Glob "$A")
			(Concept "loves")
			(Glob "$B"))))

(define mary-n-joe
	(List (Concept "Mary") (Concept "loves") (Concept "Joe")))

; Try it!  Not much different, here:
(cog-execute! (Dual mary-n-joe))

; Because this rule includes a variable typing section, the above above
; (naively-constructed) mechanisms will choke: the typing restrictions
; cause a mis-match.  Thus, we repeat the above tools, this time, with
; a typing section.

; ------------------
; The below is just like `get-consequents` above, but for typed rules.

; Utility, from which the other parts are made. Similar to
; `get-consequents`, above, except that the `cog-execute!` step
; is skipped. So, this only defines a GetLink.
(define (pattern-getter ANTECEDENT)
	(GetLink
		(VariableList
			(TypedVariable (Variable "$vardecl")
				(TypeChoice  ; three different kinds of typedecls are possible.
					(Type "VariableNode")
					(Type "TypedVariableLink")
					(Type "VariableList")))
			(TypedVariable (Variable "$consequent") (Type "ListLink")))
		(Quote (BindLink
				(Unquote (Variable "$vardecl"))
				ANTECEDENT
				(Unquote (Variable "$consequent"))))
	)
)

(define (get-conseq-typed ANTECEDENT)
"
  get-conseq-typed ANTECEDENT -- given the ANTECEDENT, return a set of
  all of the consequents of a rule.

  Example usage:
     (get-conseq-typed (List (Glob \"$A\") (Concept \"loves\") (Glob \"$B\")))
"

	(cog-execute! (pattern-getter ANTECEDENT))
)

; Try it!
; (get-conseq-typed (List (Glob "$A") (Concept "loves") (Glob "$B")))

; ------------------
; The below is just like `get-rules-for-ante`, except that its for typed rules.

(define (rule-getter ANTECEDENT)
	; The GetLink returns all of the vardecls and consequents.
	; The TypedVariable filters out and rejects all consequents that
	;   are not ListLinks.
	; The PutLink reconstructs the rule, out of the antecedent and
	;   the consequent.
	; The Quotes are used to avoid accidentally running the BindLink
	;   that is being assembled.
	(Put
		(VariableList
			(Variable "$decls")
			(Variable "$sequent"))
		(Quote (Bind
				(Unquote (Variable "$decls"))
				ANTECEDENT
				(Unquote (Variable "$sequent"))))
		(pattern-getter ANTECEDENT)
	)
)

(define (get-typed-rules-for-ante ANTECEDENT)
"
  get-typed-rules-for-ante ANTECEDENT -- given the ANTECEDENT, return a set
  of all rules (with type declarations in them) that can be applied to it.

  Example usage:
     (get-typed-rules-for-ante (List (Glob \"$A\") (Concept \"loves\") (Glob \"$B\")))
"
	(cog-execute! (rule-getter ANTECEDENT))
)

; Try it!
; (get-typed-rules-for-ante (List (Glob "$A") (Concept "loves") (Glob "$B")))

; ---------------------------------------------------------------------
; The below is just like `get-untyped-rules` above, except its for
; typed rules.

(define (rule-recognizer DATA)
	(Put
		(TypedVariable (Variable "$ante") (Type "ListLink"))
		(rule-getter (Unquote (Variable "$ante")))
		(Dual DATA)
	)
)

(define (get-typed-rules DATA)
"
  get-typed-rules DATA -- given the graph DATA, return a set of all
  rules that (that have type declarations) that can be applied to it.

  Example usage:
     (get-typed-rules (List (Concept \"I\") (Concept \"love\") (Concept \"you\")))
"
	(cog-execute! (rule-recognizer DATA))
)

; Try it!
; (get-typed-rules (List (Concept "Mary") (Concept "loves") (Concept "Joe")))
;
; Just as before, unwrap the rules from the multiple nested SetLinks
; that get wedged in.  Finally, execute the resulting rules!
(define ruleset
	(unwrap-rules
		(get-typed-rules (List (Concept "Mary") (Concept "loves") (Concept "Joe")))))

; Try it!  Ta-DAHHH!
(map cog-execute! ruleset)

; At last, we get the expected response:
(SetLink
   (ListLink
      (ConceptNode "I'm")
      (ConceptNode "sure")
      (ConceptNode "that")
      (ConceptNode "Mary")
      (ConceptNode "loves")
      (ConceptNode "Joe")))

; ---------------------------------------------------------------------
; And now ... some simple, cheap, cheesy glue to implement a simple
; AIML-like system.  To actually use this, copy this file, and edit it
; to remove the example sentences (they'll pollute the output) and add
; more rules, in the format similar to the above.  Make sure that the
; rules are typed; this is using the typed variant of the above
; examples.

; Give a string sentence SENT, generate a rule-driven reply.
; Example: (aiml-reply "Anne loves Richard")
(define (aiml-reply SENT)

	; Split a string into words.
	; Example: (split-sentence "I love you")
	(define (split-sentence SENT)
		(ListLink
			(map (lambda (word) (Concept word)) (string-split SENT #\ ))))

	; Get the result of applying rules.
	; Example: (get-aiml-response "I love you")
	(define (get-aiml-response SENT)
		(define ruleset
			(unwrap-rules (get-typed-rules (split-sentence SENT))))
		(map cog-execute! ruleset)
	)

	; Convert a ListLink of Nodes into a scheme list of strings
	; Add a padding blank to the end.
	(define (atoms-to-strings NODELIST)
		(fold-right
			(lambda (s li) (cons (string-append (cog-name s) " ") li))
			'()
			(cog-outgoing-set NODELIST)))

	; Concatenate a list of strings into one string.
	(define (make-sent NODELIST)
		(string-concatenate (atoms-to-strings NODELIST)))

	; Create a list of reply sentences.
	; Example: (make-reply "I love you")
	(define (make-reply SENT)
		(map make-sent
			(cog-outgoing-set (car (get-aiml-response SENT)))))

	; Now, actually do stuff.  Push and pop the AtomSpace, to avoid
	; polluting it with sentences.  The push and pop here is a hack;
	; a more elegant solution is to use anchors to point at the
	; current sentence, and have the rules look for it there.
	(define reply "")
	(begin
		(cog-push-atomspace)
		(set! reply (make-reply SENT))
		(cog-pop-atomspace)
		reply)
)

; ---------------------------------------------------------------------
*unspecified*
