Haskell Examples:
================

To run these examples you have to previously
[build](https://github.com/opencog/atomspace#building-atomspace) and
[install](https://github.com/opencog/atomspace#install) the AtomSpace.

Set the stack environment with:
```
export STACK_YAML=<ATOMSPACE_ROOT>/opencog/haskell/stack.yaml
```
Then you can just compile them with:
```
stack ghc example.hs
```

To use GHCi:

```
export STACK_YAML=<ATOMSPACE_ROOT>/opencog/haskell/stack.yaml
stack ghci --ghc-options -lhaskell-atomspace
```

If when running an example you get an error: "...cannont open shared object
file: No such file or directory ...":
  - Remember to add: "/usr/local/lib/opencog" to your */etc/ld.so.conf* file.
  - Check if the file: */usr/local/lib/opencog/libhaskell-atomspace.so* exists.
  - To update loader's cache, run:
    ```
       sudo ldconfig /usr/local/lib/opencog/
    ```

