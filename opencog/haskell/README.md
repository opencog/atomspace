Haskell Bindings for OpenCog:
============================

This directory contains the implementation of Haskell bindings for the AtomSpace.

GSoC 2015 - Haskell Bindings.

**Student:** Marcos Pividori

**Mentor:** Nil Geisweiller

For general information on haskell bindings you can visit the wiki page:
[Haskell](http://wiki.opencog.org/w/Haskell)

For in depth information on the implementation and project details:
[Haskell Bindings - GSoC 2015](http://wiki.opencog.org/w/Haskell_Bindings_-_GSoC_2015)

### Requirements

To use the Haskell bindings, it is necessary to have installed:

* (Stack https://github.com/commercialhaskell/stack/wiki) (Haskell development tool)

To install Stack, you should follow (Download instructions
https://github.com/commercialhaskell/stack/wiki/Downloads)

### Installation

Go through the normal process of
[building](https://github.com/opencog/atomspace#building-atomspace) and
[installing](https://github.com/opencog/atomspace#install) the AtomSpace.

This will automatically build the haskell library.

If you want to use this library in some project you should include it to the
package list on the project stack.yaml config file. For example:

```yaml
...
packages:
- ...RelativePathToThisDirectory...
...
```

On the other hand, if you simply want to compile some code using this
library, you should compile with:

```
export STACK_YAML=...RelativePathToThisDirectory/stack.yaml
stack ghc example.hs
```

Or, to avoid this flag, you can include this library to your local environment,
adding this library path to your package list in your local config file:
~/.stack/global/stack.yaml ,and then running:

```
stack install opencog-atomspace
```

Then you can compile simple .hs files with:
```
stack ghc example.hs
```

(It is necessary to previously build and install the AtomSpace, because the
opencog-atomspace haskell library
depends on the haskell-atomspace C wrapper library)

### Documentation
To generate proper (Haddock https://www.haskell.org/haddock/) documentation,
you should go to the build directory and execute:
```
make doxygen
```
Then you can open the OpenCog documentation (build/doc/html/index.html),
go to "OpenCog source code documentation - Libraries - Haskell bindings
library" and click on the "opencog-atomspace" link.

### Usage

To use the opencog-atomspace haskell library, we just import the modules like:
```haskell
import OpenCog.AtomSpace
...
```

You can find many examples on the [examples/haskell](../../examples/haskell)
directory.

### AtomSpace Environment

The main idea is to build programs that work on an AtomSpace on the
Monad 'AtomSpace'.
Then, we can run this programs with the function runOnNewAtomSpace:

```haskell
runOnNewAtomSpace :: AtomSpace a -> IO a
```

It creates a new C++ atomspace behind, does all the computation, and finally
deletes it.

The AtomSpace data type is defined as:

```haskell
type AtomSpace = ReaderT AtomSpaceRef IO
```

ReaderT is a monad transformer, so in fact:

```haskell
AtomSpace a = ReaderT { runReaderT :: AtomSpaceRef -> IO a }
```

The interpretation of runReaderT in this case is:  "given an atomspace in
memory, it reduces to performing IO actions with a result of type a"

Because of the use of the monad IO, we can lift IO actions inside the
monad AtomSpace, through the use of liftIO. For example:

```haskell
import OpenCog.AtomSpace.Api
import Control.Monad.IO.Class

prog :: AtomSpace ()
prog = do
        liftIO $ putStrLn "hello world"
        ...

main :: IO ()
main = runOnNewAtomSpace prog

```

###Main functions:

####insert:
Function to insert atoms to the atomspace. To create new atoms or just to
update the mutable information of a specific atom.
```haskell
insert :: Atom a -> AtomSpace ()
```
####get:
Function to get an atom back from the atomspace.
```haskell
get :: Atom a -> AtomSpace (Maybe (Atom a))
```
####remove:
Function to remove atoms from the atomspace.
```haskell
remove :: Atom a -> AtomSpace Bool
```
####printAtom
Function to show the given atom in opencog notation.
```haskell
printAtom :: Atom a -> IO ()
```
####debug:
Debug function to print the state of the atomspace on stderr.
```haskell
debug :: AtomSpace ()
```

