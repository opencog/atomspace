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

**Integration Point 1**: `unorder_compare()` (around line 1800)

Before starting permutation enumeration, initialize constraint domains:

```cpp
bool PatternMatchEngine::unorder_compare(...)
{
    // NEW: Initialize constraint propagation if exclusives exist
    if (not _pat->exclusives.empty())
    {
        init_exclusive_constraints(pattern_elements, ground_elements);
    }

    // ... existing permutation code ...
}
```

**New Method**: `init_exclusive_constraints()`

```cpp
void PatternMatchEngine::init_exclusive_constraints(
    const PatternTermSeq& pattern_elements,
    const HandleSet& ground_elements)
{
    _constraint_domain.clear();

    // Initialize domain for each variable in pattern to all ground elements
    for (const PatternTermPtr& ptm : pattern_elements)
    {
        if (ptm->isBoundVariable())
        {
            Handle var = ptm->getHandle();

            // Check if this var participates in any ExclusiveLink
            auto it = _pat->var_exclusives.find(var);
            if (it != _pat->var_exclusives.end())
            {
                _constraint_domain.init_domain(var, ground_elements);
            }
        }
    }

    // Register constraint relationships from ExclusiveLinks
    for (const PatternTermPtr& excl : _pat->exclusives)
    {
        HandleSeq vars_in_link;
        // Extract variables from excl
        for (const Handle& h : excl->getHandle()->getOutgoingSet())
        {
            if (h->is_type(VARIABLE_NODE))
                vars_in_link.push_back(h);
        }
        _constraint_domain.add_constraint(vars_in_link);
    }

    // Apply any already-known groundings
    for (const auto& kv : var_grounding)
    {
        if (_constraint_domain.has_domain(kv.first))
        {
            if (not _constraint_domain.bind(kv.first, kv.second))
            {
                // Conflict! This branch is dead.
                return;
            }
        }
    }
}
```

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

## KNOWN ISSUES WITH CURRENT IMPLEMENTATION (Dec 2024)

The current implementation has been verified to compile and pass all 75 query tests,
but does NOT provide the expected speedup for 9x9 Sudoku. The following issues need
to be fixed:

### Issue 1: Re-initialization in unorder_compare()

**Problem**: `init_exclusive_constraints()` is called inside `unorder_compare()`,
which means the constraint domain gets reset for each UnorderedLink/SetLink clause.
For a 9x9 Sudoku with 27 clauses (9 rows + 9 columns + 9 boxes), all propagation
work is thrown away every time a new clause is entered.

**Fix**: Initialize the constraint domain ONCE at the pattern level (in `set_pattern()`
or when pattern matching begins), not inside `unorder_compare()`.

### Issue 2: Wrong Domain Source

**Problem**: Domains are initialized from the current UnorderedLink's ground set
(`osg` in `unorder_compare()`). This is wrong - the domains should come from the
problem setup. For Sudoku, that's {1,2,3,4,5,6,7,8,9} shared across all 81 variables.

**Fix**: Domains should be initialized from the ExclusiveLink's ground values or
from the pattern's global variable type constraints, not from individual clause
ground sets.

### Issue 3: Intra-clause Only (No Inter-clause Propagation)

**Problem**: The current implementation only propagates within a single UnorderedLink's
permutation search. The real power should come from INTER-clause propagation:
- When $X1 is bound to 1 in row 1, that should immediately eliminate 1 from all
  variables in $X1's column and box
- This requires constraints to persist across clause matching

**Fix**: Constraints must span clause boundaries. When `variable_compare()` binds
a variable, propagation should affect domains of ALL variables that share an
ExclusiveLink with it, regardless of which clause they appear in.

### Issue 4: Clause-level vs Pattern-level Scope

**Problem**: The constraint state is reset for each clause. It should persist
throughout the entire pattern matching process.

**Fix**:
1. Initialize constraint domain in `set_pattern()` or `explore_neighborhood()`
2. Build the full constraint graph from ALL ExclusiveLinks in the pattern
3. Use `solution_push()`/`solution_pop()` ONLY for backtracking, not for clause transitions
4. Let propagation persist as variables are bound across multiple clauses

### Required Code Changes

1. **Move initialization**: From `unorder_compare()` to `set_pattern()` or
   `explore_neighborhood()` entry point

2. **Domain initialization**: Get domains from ExclusiveLink ground sets or
   variable type constraints, not from individual clause ground sets

3. **Persistent propagation**: Ensure `variable_compare()` propagation affects
   the global constraint state, not a per-clause state

4. **State management**: Only push/pop constraint state on backtracking (in
   `solution_push()`/`solution_pop()`), not on clause transitions
