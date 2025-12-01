# Constraint Propagation for Pattern Matching

Written by Claude Code, LLM, 30 Nov 2025, edited by Linas.

Below follows a proposed enhancement to the pattern matcher to
incorporate constraint propagation techniques from CSP (Constraint
Satisfaction Problem) solvers. The goal is to reduce the search
space for patterns involving UnorderedLinks (usually SetLinks)
containing shared variables.

## The Problem

The current pattern matcher performs exhaustive backtracking search
over permutations when matching UnorderedLinks. For a SetLink with N
elements, there are N! possible ways to match variables to ground
atoms. When multiple SetLinks share variables, the search space
explodes combinatorially.

For example, consider a naive approach to Sudoku solving: if a pattern
has 9+9+9=27 row+column+box SetLinks, each with 9 VariableNodes, and
roughly 6 elements unbound, the naive search space is approximately
on the order of 6-factorial^27 = 720^27 ≈ 10^77 combinations.

## Key Insight: Constraint Propagation

CSP solvers avoid this explosion using:

1. **Domain Tracking**: Each variable maintains a set of possible values
2. **Arc Consistency**: When a variable is bound, eliminate that value
   from the domains of related variables
3. **Unit Propagation**: If a domain has only one value, bind immediately
4. **Early Failure**: If any domain becomes empty, backtrack immediately
5. **Variable Ordering**: Bind the most constrained variable first (MRV)

These techniques can prune the search space from 10^77 to thousands.

## Proposed Architecture

### 1. Domain Tracking in PatternTerm

Extend PatternTerm or create a parallel structure to track domains:

```cpp
// Possible values for each unbound variable
class VariableDomain {
    Handle _variable;
    HandleSet _possible_groundings;

    // Variables that share constraints with this one
    std::set<VariableDomain*> _neighbors;

public:
    bool is_bound() const { return _possible_groundings.size() == 1; }
    bool is_empty() const { return _possible_groundings.empty(); }
    Handle get_binding() const;  // valid only if is_bound()

    // Remove a value; returns false if domain becomes empty
    bool eliminate(const Handle& value);
};
```

### 2. Constraint Network

Build a constraint graph from the pattern during initialization:

```cpp
class ConstraintNetwork {
    std::map<Handle, VariableDomain> _domains;

    // Which variables appear together in which clauses
    std::vector<std::set<Handle>> _constraints;

public:
    // Initialize domains from the AtomSpace
    void initialize(const HandleSet& variables, AtomSpace* as);

    // Propagate: when var=value, update all neighbor domains
    // Returns false if any domain becomes empty (conflict)
    bool propagate(const Handle& var, const Handle& value);

    // Find variable with smallest domain (MRV heuristic)
    Handle most_constrained_variable() const;

    // Save/restore for backtracking
    void push_state();
    void pop_state();
};
```

### 3. Integration with unorder_compare

Modify `PatternMatchEngine::unorder_compare` to use constraint
propagation instead of blind permutation enumeration:

```cpp
bool smart_unorder_compare(const HandleSeq& pattern_elements,
                           const HandleSet& ground_elements)
{
    // Initialize domains: each pattern var can be any ground element
    // (minus those already bound elsewhere)

    // Apply existing groundings and propagate
    for (auto& pvar : pattern_elements) {
        Handle grnd = get_grounding(pvar);
        if (grnd) {
            if (!_constraints.propagate(pvar, grnd))
                return false;  // Conflict detected
        }
    }

    // Check for unit propagation opportunities
    while (auto unit = find_unit_domain()) {
        if (!_constraints.propagate(unit.var, unit.value))
            return false;
    }

    // If all bound, we're done
    if (all_bound()) return true;

    // Choose most constrained variable (MRV)
    Handle next_var = _constraints.most_constrained_variable();

    // Try each value in its domain
    for (Handle value : _constraints.domain(next_var)) {
        _constraints.push_state();

        if (_constraints.propagate(next_var, value)) {
            if (smart_unorder_compare_recurse(...))
                return true;
        }

        _constraints.pop_state();
    }

    return false;
}
```

### 4. Integration with Clause Ordering

The existing `clause_connect.cc` orders clauses by connectivity.
This could be enhanced to prefer clauses with:
- More shared variables (higher constraint density)
- Smaller initial domains
- More connections to already-processed clauses

### 5. State Management

The pattern matcher already has `perm_push()`/`perm_pop()` for
managing permutation state during backtracking. The constraint
network state can be managed similarly:

```cpp
struct ConstraintState {
    std::map<Handle, HandleSet> saved_domains;
};

std::stack<ConstraintState> _constraint_state_stack;
```

## Implementation Plan

### Phase 1: Domain Infrastructure
- Add VariableDomain class
- Add ConstraintNetwork class
- Integrate with existing PatternTerm infrastructure
- Unit tests for domain operations

### Phase 2: Basic Propagation
- Implement arc consistency (AC-3 algorithm)
- Integrate into unorder_compare
- Maintain existing behavior as fallback
- Benchmark on test cases

### Phase 3: Variable Ordering
- Implement MRV (Minimum Remaining Values) heuristic
- Optionally implement LCV (Least Constraining Value)
- Compare performance with default ordering

### Phase 4: Advanced Techniques (Future)
- Conflict-driven clause learning (CDCL)
- Watched literals for efficient propagation
- Constraint-specific propagators

## Risks and Mitigations

1. **Correctness**: The pattern matcher has complex semantics.
   Mitigation: Extensive testing, fallback to existing behavior.

2. **Overhead**: Domain tracking adds overhead for simple patterns.
   Mitigation: Only enable for patterns with UnorderedLinks and
   shared variables.

3. **Interaction with existing machinery**: The permutation state,
   choice links, and grounding management are already complex.
   Mitigation: Clean interface, incremental integration.

4. **GroundedPredicates**: Opaque code blocks constraint propagation.
   Mitigation: Treat GroundedPredicates as constraints that must be
   checked but cannot propagate.

## Files Likely to Be Modified

- `PatternMatchEngine.cc` - Core matching logic
- `PatternMatchEngine.h` - New data structures
- `PatternTerm.cc/h` - Domain tracking (possibly)
- `clause_connect.cc` - Enhanced ordering (later phase)

## References

- AC-3 Algorithm: Mackworth, "Consistency in Networks of Relations" (1977)
- CSP Solving: Russell & Norvig, "Artificial Intelligence: A Modern Approach"
- CDCL: Marques-Silva & Sakallah, "GRASP: A Search Algorithm for
  Propositional Satisfiability" (1999)
