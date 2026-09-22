# A Recurrent Enumeration of Free Hypermultisets (corrected source)

This directory contains a syntax-correct MATLAB implementation of the
free-hypermultiset enumeration described in *A Recurrent Enumeration of Free
Hypermultisets* by Vincenzo Manca.

Free hypermultisets are equivalent to unlabeled unordered rooted trees. If
`T(n)` counts rooted trees with `n` nodes, the number with `m` non-skin
membranes is `M(m) = T(m + 1)`.

## MATLAB implementation

- `free_hypermultiset_counts(maxMembranes)` returns `M(0)` through
  `M(maxMembranes)`.
- `free_hypermultiset_count(membranes)` returns one count.

The implementation uses Otter's recurrence:

```text
n*T(n) = sum(k = 1..n-1, T(n-k) * sum(d divides k, d*T(d)))
```

The first values are:

```text
M(0..12) = 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486
```

## Corrections to the supplied MATLAB listing

The supplied listing uses spaces in function names (`compute M`, `compute C`,
etc.), non-MATLAB ceiling/floor glyphs, Unicode multiplication and minus
signs, and semicolons in conditional expressions. Those have been replaced by
valid MATLAB syntax in the implementation here.

Its final recurrence also contains `compute M(n)` on the right-hand side while
computing `compute M(n)`, which causes infinite recursion. The implementation
uses the equivalent Otter recurrence instead, avoiding that transcription
error while producing the same published sequence.
