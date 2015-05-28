Haskell Bindings for OpenCog:
============================

### Requirements

To use the Haskell bindings, it is necessary to have installed:

* GHC (Glasgow Haskell Compiler https://www.haskell.org/ghc/)
* Cabal (Haskell package manager https://www.haskell.org/cabal/)

For new users, the simplest option is to install the "Haskell Platform" (https://www.haskell.org/platform/)
which includes the most important tools for Haskell environment.

### Installation

Go through the normal process of [building](https://github.com/opencog/atomspace#building-atomspace) and
[installing](https://github.com/opencog/atomspace#install) the AtomSpace.

Then move to this directory (/opencog/haskell) and build and install the opencog-atomspace haskell library:

```
 cabal configure
 cabal build
 cabal install
```

(It is necessary to previously build and install the AtomSpace, because the opencog-atomspace haskell library
depends on the atomspace_wrapper library)

### Usage

To use the opencog-atomspace haskell library, we just import the modules like:
```haskell
import OpenCog.AtomSpace
...
```

### AtomSpace API

The main idea is to build programs that work on an AtomSpace on the Monad 'AtomSpace'.
Then, we can run this programs with the function runOnNewAtomSpace:

```haskell
runOnNewAtomSpace :: AtomSpace a -> IO a
```

It creates a new C++ atomspace behind, does all the computation, and finally deletes it.

The AtomSpace data type is defined as:

```haskell
type AtomSpace = ReaderT AtomSpaceRef IO
```

ReaderT is a monad transformer, so in fact:

```haskell
AtomSpace a = ReaderT { runReaderT :: AtomSpaceRef -> IO a }	 
```

The interpretation of runReaderT in this case is:  "given an atomspace in memory, it reduces to performing IO actions with a result of type a"

Because of the use of the monad IO, we can lift IO actions inside de monad AtomSpace, through the use of liftIO. For example:

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

