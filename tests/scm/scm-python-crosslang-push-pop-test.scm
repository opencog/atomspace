#! /usr/bin/env guile
-s
!#

(use-modules (opencog))
(use-modules (opencog python))
(use-modules (opencog test-runner))

(opencog-test-runner)

; Test 1d: Cross-language push/pop visibility
;
; These tests verify that when one language pushes an AtomSpace,
; the other language can see atoms created in that pushed space,
; and when popped, those atoms are no longer visible.

(define tname "crosslang-push-pop-visibility-test")
(test-begin tname)

; Define a Python function that creates an atom with a value
(python-eval "
from opencog.atomspace import *
from opencog.atomspace import get_thread_atomspace

def scm_test_create_marker():
    '''Create a marker atom with a value in the current atomspace.'''
    marker = ConceptNode('py-marker-from-scheme-push')
    key = PredicateNode('py-key')
    asp = get_thread_atomspace()
    asp.set_value(marker, key, StringValue('hello-from-python-in-pushed'))
    return marker
")

; -----------------------------------------------------------------------------
; Test: Scheme pushes, Python creates atom, Scheme sees it
; -----------------------------------------------------------------------------

; Push a new temporary atomspace
(define base-as (cog-push-atomspace))

; Call Python function that creates an atom with a value
(define py-result
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:scm_test_create_marker")
            (List))))

; Verify we got a result back
(test-assert "Python returned a marker atom"
    (not (eq? #f py-result)))

(test-assert "Python returned correct atom name"
    (equal? (cog-name py-result) "py-marker-from-scheme-push"))

; Verify Scheme can see the atom in the pushed atomspace
(define marker (cog-node 'ConceptNode "py-marker-from-scheme-push"))
(test-assert "Scheme can find the atom Python created"
    (not (eq? #f marker)))

; Verify Scheme can see the value that Python set
(define key (Predicate "py-key"))
(define val (cog-value marker key))
(test-assert "Scheme can see the value Python set"
    (not (eq? #f val)))

(test-assert "Value has correct content"
    (equal? (cog-value-ref val 0) "hello-from-python-in-pushed"))

; Pop the atomspace
(cog-pop-atomspace)

; Verify the atom is gone from the base atomspace
(define marker-after-pop (cog-node 'ConceptNode "py-marker-from-scheme-push"))
(test-assert "Atom is gone after pop"
    (not marker-after-pop))

; -----------------------------------------------------------------------------
; Test: Scheme pushes, Scheme creates atom, Python sees and modifies it
; -----------------------------------------------------------------------------

; Define Python function that reads and modifies an atom's value
(python-eval "
def scm_test_read_and_modify():
    '''Read a value set by Scheme, modify it, return confirmation.'''
    marker = ConceptNode('scm-marker-for-py')
    key = PredicateNode('scm-key')

    # Read the value Scheme set
    val = marker.get_value(key)
    if val is None:
        return ConceptNode('ERROR-no-value')

    original = val.to_list()[0]
    if original != 'original-from-scheme':
        return ConceptNode('ERROR-wrong-value')

    # Modify the value
    asp = get_thread_atomspace()
    asp.set_value(marker, key, StringValue('modified-by-python'))

    return ConceptNode('SUCCESS')
")

; Push a new atomspace
(cog-push-atomspace)

; Create an atom with a value in the pushed space
(define scm-marker (Concept "scm-marker-for-py"))
(define scm-key (Predicate "scm-key"))
(cog-set-value! scm-marker scm-key (StringValue "original-from-scheme"))

; Call Python to read and modify
(define modify-result
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:scm_test_read_and_modify")
            (List))))

(test-assert "Python successfully read and modified the value"
    (equal? (cog-name modify-result) "SUCCESS"))

; Verify Scheme can see Python's modification
(define modified-val (cog-value scm-marker scm-key))
(test-assert "Scheme sees Python's modification"
    (equal? (cog-value-ref modified-val 0) "modified-by-python"))

; Pop and verify cleanup
(cog-pop-atomspace)

(define scm-marker-after-pop (cog-node 'ConceptNode "scm-marker-for-py"))
(test-assert "Scheme marker is gone after pop"
    (not scm-marker-after-pop))

(test-end tname)

(opencog-test-end)
