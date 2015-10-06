;
; state.scm
;
; This provides a very simple demo of using the StateLink to maintain
; unique state. A Statelink is a kind of link of which only one can ever
; exist in the atomspace, for a given "state" atom.  Whenever a new
; StateLink is added to the atomspace, the old one is automatically
; removed.

; The current state of "fruit" is "apple".
(StateLink (AnchorNode "fruit") (ConceptNode "apple"))

; Lets make sure of that:
(cog-incoming-set (AnchorNode "fruit"))

; Change the state to bananna:
(StateLink (AnchorNode "fruit") (ConceptNode "bananna"))

; Lets make sure the state changed:
(cog-incoming-set (AnchorNode "fruit"))

; Change it back to apple:
(StateLink (AnchorNode "fruit") (ConceptNode "apple"))

; Lets make sure the state changed:
(cog-incoming-set (AnchorNode "fruit"))
