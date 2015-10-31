
Reduct-inspired term reduction for atoms
----------------------------------------

This is an experimental implementation of reduct-type functionality
in the atomspace. Its experimental because its not really designed for
performance nor for extensibility, nor anything else. Its more of a
proof-of-concept. It was insipred by combo, which does reduction in
C++ code...

After doing this, I decided that this is a bad idea. If you want to
do reduction, do it using the unified rule engine (URE) ad write some
rules for it. Don't do it in C++ code, the way its done here.
Basically, much or most of the code here should be eliminated, and
polynomial algebra should be implemented using URE rules.


Caution:
Some of the examples below used to work, but got broken during a
reorganization.  The core problem is that there is a ciruclar
dependency between all the various shared libraries that is
relly hard to disentangle; untangling it cause breakage which is not
fixed.

Anyway... try this at the guile prompt:
```
(cog-reduce! (PlusLink (NumberNode 2) (NumberNode 2)))
```

you should see `(NumberNode 4)` as the output.

A more challenging example:
```
(cog-reduce! (PlusLink (NumberNode 0) (VariableNode "$x")))
```

should yeild `(VariableNode "$x")` -- that is, adding zero to something
has no effect!

Some more interesting examples:
```
(cog-reduce! (PlusLink (NumberNode 2) (VariableNode "$x") (NumberNode -2)))
```

should yeild `(VariableNode "$x")` -- the +2 and -2 cancel.

```
(cog-reduce! (PlusLink (NumberNode 2) (VariableNode "$x") (NumberNode 2)))
```

should yeild `(PlusLink (VariableNode "$x") (NumberNode 4))` --
the +2 and +2 sum.

Using the same ideas:
```
(cog-reduce! (TimesLink (NumberNode 1) (VariableNode "$x")))

(cog-reduce!
   (TimesLink
      (VariableNode "$y")
      (NumberNode 1)
      (VariableNode "$x")))

(cog-reduce!
   (TimesLink
      (VariableNode "$y")
         (NumberNode 0.5)
         (VariableNode "$x")
         (PlusLink (NumberNode 1) (NumberNode 1))))
```

Not sure, but reduction of x+2x to 3x and x+(-x) to 0 should work.
