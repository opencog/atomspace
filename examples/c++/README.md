
C++ Coding Examples
===================

These examples can be built by saying, in the `build` directory,
```
make examples
```

The resulting binaries will be placed in `build/examples/c++`.

The examples can also be compiled by hand. The `basic.cc` example can be
compiled as:
```
g++ -std=c++17 -c basic.cc
g++ -o basic basic.o -L/usr/local/lib/opencog -latom_types -latombase -latomspace
```
