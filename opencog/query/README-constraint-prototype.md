# Constraint Propagation Prototype

Detailed implementation plan for ExclusiveLink constraint propagation.

## Overview

This prototype adds constraint propagation for ExclusiveLink to prune the
permutation search space when matching UnorderedLinks (SetLinks). The design
splits into two stages:

1. **Analysis Stage** (pattern compilation): Detect and categorize ExclusiveLinks
2. **Runtime Stage** (pattern matching): Propagate constraints during search

## Data Structures

### PatternTerm Extensions

Add to `PatternTerm.h`:

```cpp
// New flag
bool _is_exclusive = false;

// New methods
void markExclusive();
bool isExclusive() const noexcept { return _is_exclusive; }
```

### Pattern Extensions

Add to `Pattern.h`:

```cpp
// ExclusiveLink terms that operate within a single component.
// These can use constraint propagation during permutation enumeration.
PatternTermSeq exclusives;

// ExclusiveLink terms that bridge multiple components.
// These must be treated as virtual links (filter Cartesian product).
PatternTermSeq exclusive_virtuals;

// Map from variable to ExclusiveLinks it participates in.
// Used at runtime for quick lookup.
std::map<Handle, PatternTermSeq> var_exclusives;
```

### ConstraintDomain (Already Created)

Located in `opencog/query/ConstraintDomain.h`:
- `DomainMap _domains` - variable → set of possible values
- `_neighbors` - constraint graph (which vars share ExclusiveLinks)
- `bind()` - bind variable, propagate to neighbors
- `eliminate()` - remove value from domain
- `find_unit()` - find forced bindings
- `most_constrained()` - MRV heuristic
- `push_state()`/`pop_state()` - backtracking support

## Analysis Stage

### File: `opencog/atoms/pattern/PatternTerm.cc`

Add `markExclusive()` implementation (similar to `markIdentical()`):

```cpp
void PatternTerm::markExclusive()
{
    if (_is_quoted) return;  // Quoted terms cannot be evaluated
    _is_exclusive = true;
    _has_any_eval = true;    // ExclusiveLink is evaluatable
}
```

### File: `opencog/atoms/pattern/PatternLink.cc`

**Location**: Around line 1260, in the evaluatable detection loop.

Add detection for EXCLUSIVE_LINK:

```cpp
else if (EXCLUSIVE_LINK == t)
    ptm->markExclusive();
```

**Location**: New method `categorize_exclusives()` called from `common_init()`.

```cpp
void PatternLink::categorize_exclusives()
{
    // For each exclusive term, check if all its variables are in
    // the same connected component, or if it bridges components.

    for (const PatternTermPtr& ptm : /* all marked exclusive terms */)
    {
        HandleSet vars_in_exclusive;
        // Extract variables from the ExclusiveLink

        // Check which components these variables belong to
        std::set<size_t> components_touched;
        for (const Handle& var : vars_in_exclusive)
        {
            for (size_t i = 0; i < _num_comps; i++)
            {
                if (_component_vars[i].count(var))
                    components_touched.insert(i);
            }
        }

        if (components_touched.size() == 1)
        {
            // Single component - can use for constraint propagation
            _pat.exclusives.push_back(ptm);
        }
        else
        {
            // Bridges components - must treat as virtual
            _pat.exclusive_virtuals.push_back(ptm);
        }

        // Build var_exclusives map for runtime lookup
        for (const Handle& var : vars_in_exclusive)
            _pat.var_exclusives[var].push_back(ptm);
    }
}
```

## Runtime Stage

### File: `opencog/query/PatternMatchEngine.h`

Add member:

```cpp
ConstraintDomain _constraint_domain;
```

### File: `opencog/query/PatternMatchEngine.cc`

**Integration Point 1**: `InitiateSearchMixin` search methods

Constraint domains are initialized at search time via `init_constraint_domains()`:

```cpp
// Called from search methods in InitiateSearchMixin.cc
pme.init_constraint_domains();
```

**Method**: `init_constraint_domains()`

This method determines variable domains at SEARCH time (not pattern analysis time)
by examining OTHER terms in the pattern that constrain each variable. For each
variable in an ExclusiveLink, it uses the `connectivity_map` to find terms
containing that variable, then uses the incoming sets of ground atoms in those
terms to determine possible bindings.

**Integration Point 2**: `do_perm_recurse()` (around line 1900)

When binding a variable during permutation enumeration:

```cpp
bool PatternMatchEngine::do_perm_recurse(...)
{
    // NEW: If using constraint propagation, check and propagate
    if (_constraint_domain.has_domain(pattern_var))
    {
        _constraint_domain.push_state();

        if (not _constraint_domain.bind(pattern_var, proposed_ground))
        {
            // Conflict detected - skip this branch
            _constraint_domain.pop_state();
            return false;
        }

        // Check for unit propagation (forced bindings)
        while (true)
        {
            Handle unit = _constraint_domain.find_unit();
            if (unit == Handle::UNDEFINED) break;

            Handle forced_value = _constraint_domain.get_binding(unit);
            // Record the forced grounding
            var_grounding[unit] = forced_value;
        }
    }

    // ... existing recursion ...

    // On backtrack:
    if (_constraint_domain.has_domain(pattern_var))
        _constraint_domain.pop_state();
}
```

**Integration Point 3**: Variable ordering (MRV heuristic)

In the permutation enumeration, prefer variables with smallest domains:

```cpp
// Instead of iterating pattern_elements in order,
// use _constraint_domain.most_constrained() to pick next variable
```

## Files to Modify

### opencog/atoms/pattern/

| File | Changes |
|------|---------|
| `PatternTerm.h` | Add `_is_exclusive`, `markExclusive()`, `isExclusive()` |
| `PatternTerm.cc` | Implement `markExclusive()` |
| `Pattern.h` | Add `exclusives`, `exclusive_virtuals`, `var_exclusives` |
| `PatternLink.cc` | Detect EXCLUSIVE_LINK (~line 1260), add `categorize_exclusives()` |

### opencog/query/

| File | Changes |
|------|---------|
| `PatternMatchEngine.h` | Add `_constraint_domain` member |
| `PatternMatchEngine.cc` | Modify `unorder_compare()` (~line 1800), `do_perm_recurse()` (~line 1900) |
| `ConstraintDomain.h/cc` | Already created - may need minor adjustments |

## Testing Strategy

1. **Unit tests for ConstraintDomain**: Test domain operations, propagation, backtracking
2. **Integration test**: Simple ExclusiveLink pattern with SetLink
3. **Sudoku test**: The motivating use case - should see dramatic speedup

## Edge Cases

1. **ExclusiveLink with non-variable terms**: Should be handled at evaluation time, not propagation
2. **Nested ExclusiveLinks**: Not currently supported
3. **ExclusiveLink in ChoiceLink**: Needs careful handling
4. **Cross-component ExclusiveLinks**: Fall back to virtual link treatment

## Performance Considerations

1. **Overhead**: Domain tracking adds overhead for simple patterns
2. **Threshold**: Only enable when ExclusiveLinks present AND SetLinks have >3 elements
3. **Memory**: State stack for backtracking - bounded by search depth

---

## IMPLEMENTATION STATUS (Dec 2024)

The current implementation:

1. **Initialization**: `init_constraint_domains()` is called at search time from
   `InitiateSearchMixin` methods, ONCE per pattern search (not per clause).

2. **Domain determination**: Domains are determined at search time by examining
   OTHER terms in the pattern via `connectivity_map`. For each variable in an
   ExclusiveLink, the code finds terms containing that variable and uses the
   incoming sets of ground atoms to determine possible bindings.

3. **Propagation**: When `variable_compare()` binds a variable, `propagate_exclusive()`
   is called to eliminate that value from the domains of all variables that share
   an ExclusiveLink constraint.

4. **Backtracking**: Constraint state is pushed/popped in `solution_push()`/`solution_pop()`
   to properly handle backtracking.

All Sudoku tests (2x2, 3x3, 9x9) pass with the current implementation
