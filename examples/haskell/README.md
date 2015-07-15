Haskell Examples:
================

To run these examples you have to previously
[build](https://github.com/opencog/atomspace#building-atomspace) and
[install](https://github.com/opencog/atomspace#install) the AtomSpace.

Then you can just compile them with:
```
ghc example.hs
```

If when running an example you get an error: "...cannont open shared object
file: No such file or directory ...":
  - Remember to add: "/usr/local/lib/opencog" to your */etc/ld.so.conf* file.
  - Check if the file: */usr/local/lib/opencog/libhaskell-atomspace.so* exists.
  - To update loader's cache, run:
    ```
       ldconfig /usr/local/lib/opencog/
    ```

