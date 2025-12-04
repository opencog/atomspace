;
; scm-python-threading-test.scm
;
; Cross-Language AtomSpace Threading Tests (Scheme version)
;
; These tests are the inverse of the Python tests in
; tests/cython/multithread/test_crosslang_atomspace.py
;
; Test 1a-inv: Scheme code runs threaded Python code that calls Scheme
; Test 1b-inv: Scheme code runs threaded Scheme code that calls Python
; Test 1c-inv: Cross-lang atomspace visibility tests
;
; Uses ExecuteThreadedLink instead of (ice-9 threads) for threading.
;

(use-modules (opencog) (opencog exec) (opencog python))
(use-modules (opencog test-runner))
(use-modules (srfi srfi-1))  ; For 'every' function

(opencog-test-runner)

; ============================================================================
; Setup: Define Python functions to be called from Scheme
; ============================================================================

(python-eval "
from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.type_ctors import get_thread_atomspace

# Simple Python function
def py_simple():
    return Concept('py_result')

# Python function with arguments
def py_with_args(arg1, arg2):
    return Concept(f'py_result_{arg1.name}_{arg2.name}')

# Python function that creates a marker and returns it
def py_create_marker(idx):
    n = int(float(idx.name))  # NumberNode gives string like '3.0'
    marker = Concept(f'py_marker_{n}')
    return marker

# Python function that checks atomspace size
def py_check_atomspace():
    asp = get_thread_atomspace()
    if asp is None:
        return Concept('py_no_atomspace')
    atoms = asp.get_atoms_by_type(0)  # types.Atom
    return Concept(f'py_as_size_{len(atoms)}')

# Python function that returns indexed result for py->scm test
def py_call_scm_with_idx(idx):
    n = int(float(idx.name))
    return Concept(f'py_to_scm_{n}')
")


; ============================================================================
; Test 1b-inv: Scheme runs threaded Scheme code that calls Python
; (Inverse of Python Test 1b)
; ============================================================================

(define tname-1b "scm-threaded-calls-python")
(test-begin tname-1b)

; Basic test: Single ExecutionOutput calling Python
(test-assert "basic-scm-to-python"
    (let ((result (cog-execute!
            (ExecutionOutput
                (GroundedSchema "py:py_simple")
                (List)))))
        (equal? (cog-name result) "py_result")))

; Basic test with arguments
(test-assert "scm-to-python-with-args"
    (let ((result (cog-execute!
            (ExecutionOutput
                (GroundedSchema "py:py_with_args")
                (List (Concept "alpha") (Concept "beta"))))))
        (equal? (cog-name result) "py_result_alpha_beta")))

; Threaded test: Multiple threads calling Python
; Create 10 different ExecutionOutputLinks, each calling Python
(define threaded-py-calls
    (ExecuteThreaded
        (Number 5)  ; Use 5 threads
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_create_marker")
                        (List (Number n))))
                (iota 10)))))

(define threaded-py-results (cog-execute! threaded-py-calls))

; Use cog-value->list which blocks until queue is closed
(define py-call-results (cog-value->list threaded-py-results))

(test-assert "threaded-scm-to-python-count"
    (= 10 (length py-call-results)))

(test-assert "threaded-scm-to-python-results"
    (let ((names (map cog-name py-call-results)))
        ; All results should be py_marker_N for some N
        (every (lambda (name) (string-prefix? "py_marker_" name)) names)))

(test-end tname-1b)


; ============================================================================
; Test 1a-inv: Scheme runs threaded Python code that returns results
; (Inverse of Python Test 1a)
; ============================================================================

(define tname-1a "scm-threaded-python-results")
(test-begin tname-1a)

; Threaded: Multiple Python calls returning results
(define threaded-py-to-scm
    (ExecuteThreaded
        (Number 5)
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_call_scm_with_idx")
                        (List (Number n))))
                (iota 10)))))

(define py-to-scm-results (cog-execute! threaded-py-to-scm))
(define py-to-scm-collected (cog-value->list py-to-scm-results))

(test-assert "threaded-python-results-count"
    (= 10 (length py-to-scm-collected)))

(test-assert "threaded-python-results-valid"
    (let ((names (map cog-name py-to-scm-collected)))
        (every (lambda (name) (string-prefix? "py_to_scm_" name)) names)))

(test-end tname-1a)


; ============================================================================
; Test 1c-inv: Cross-language atomspace visibility
; (Inverse of Python Test 1c)
; ============================================================================

(define tname-1c "scm-crosslang-atomspace-visibility")
(test-begin tname-1c)

; Add a marker atom in current Scheme atomspace
(Concept "scm_marker_atom")

; Check what atomspace Python sees when called from Scheme
(define py-atomspace-check
    (cog-execute!
        (ExecutionOutput
            (GroundedSchema "py:py_check_atomspace")
            (List))))

; The result tells us what Python sees
(format #t "Scheme->Python: Python sees atomspace: ~a\n" (cog-name py-atomspace-check))

(test-assert "python-sees-atomspace"
    ; Python should see some atomspace (not "py_no_atomspace")
    (not (string=? "py_no_atomspace" (cog-name py-atomspace-check))))

; Test threaded atomspace visibility
; Each thread should see the same atomspace
(define threaded-as-check
    (ExecuteThreaded
        (Number 3)
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_check_atomspace")
                        (List)))
                (iota 6)))))

(define as-check-results (cog-execute! threaded-as-check))
(define as-check-collected (cog-value->list as-check-results))

(test-assert "threaded-atomspace-visibility-count"
    (= 6 (length as-check-collected)))

; All threads should see some atomspace
(test-assert "threaded-all-see-atomspace"
    (every
        (lambda (result)
            (not (string=? "py_no_atomspace" (cog-name result))))
        as-check-collected))

(test-end tname-1c)


; ============================================================================
; Intensive threading test
; ============================================================================

(define tname-intensive "scm-intensive-threading")
(test-begin tname-intensive)

; Run many threads with many operations
(define intensive-test
    (ExecuteThreaded
        (Number 10)  ; 10 threads
        (Set
            (map
                (lambda (n)
                    (ExecutionOutput
                        (GroundedSchema "py:py_with_args")
                        (List (Concept (format #f "arg_~a_1" n))
                              (Concept (format #f "arg_~a_2" n)))))
                (iota 30)))))  ; 30 tasks

(define intensive-results (cog-execute! intensive-test))
(define intensive-collected (cog-value->list intensive-results))

(test-assert "intensive-threading-count"
    (= 30 (length intensive-collected)))

(test-assert "intensive-threading-all-valid"
    (every
        (lambda (result)
            (string-prefix? "py_result_arg_" (cog-name result)))
        intensive-collected))

(test-end tname-intensive)


; ============================================================================
; Final summary
; ============================================================================

(opencog-test-end)
