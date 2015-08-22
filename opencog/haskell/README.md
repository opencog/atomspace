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

* [Stack](https://github.com/commercialhaskell/stack/wiki) (Haskell development tool)

To install Stack, you should follow
[Download instructions](https://github.com/commercialhaskell/stack/wiki/Downloads)

### Installation

To check if you have proper ghc version installed. Run:
```
cd <ATOMSPACE_ROOT>/opencog/haskell
stack setup
```

Then, go through the normal process of
[building](https://github.com/opencog/atomspace#building-atomspace) and
[installing](https://github.com/opencog/atomspace#install) the AtomSpace.

This will automatically build the haskell library.

If you want to use this library in some project you should include it to the
package list on the project
[stack.yaml](https://github.com/commercialhaskell/stack/wiki/stack.yaml)
config file. For example:

```yaml
...
packages:
- <ATOMSPACE_ROOT>/opencog/haskell
...
```

On the other hand, if you simply want to compile some code using this
library, you should compile with:

```
export STACK_YAML=<ATOMSPACE_ROOT>/opencog/haskell/stack.yaml
stack ghc example.hs
```

For using ghci:

```
export STACK_YAML=<ATOMSPACE_ROOT>/opencog/haskell/stack.yaml
stack ghci
```

Or, to avoid defining STACK_YAML every time, you can include this library to your
local environment, adding this library path to your package
list in your local config file:
~/.stack/global/stack.yaml ,and then running:

```
stack install opencog-atomspace
```

Then you can compile simple .hs files with:
```
stack ghc example.hs
```

### Possible errors
* It is necessary to previously build and install the AtomSpace, because the opencog-atomspace haskell library depends on the haskell-atomspace C wrapper library.
So, if you have a problem like "haskell-atomspace library not found" , first of all, you should ensure it was properly installed when installing the AtomSpace.

* If when running "stack ghc" or "stack build" you get an error about "Missing C library: haskell-atomspace", you should add the flag:
 
  --extra-lib-dir=/usr/local/lib/opencog (with proper library location).

* If when running "stack ghci" you get an error about "lhaskell-atomspace not found", you should add the flags: 

  --ghc-options -lhaskell-atomspace

* If when using "stack ghci" you see that ghci is interpreting the code: "[..] Compiling ... (..., interpreted )"
Then, a better option is to compile the package to object code, so it is loaded, which runs faster. All you have to do is to execute only one time:

 stack ghci --ghc-options -fobject-code

 On future uses, ghci will automatically load the compiled package.

* If you find problems when building haskell libraries. One good option is to remove the directory:

 <ATOMSPACE_ROOT>/opencog/haskell/.stack-work

 It is automatically created when building, and sometimes becomes inconsistent.


### Documentation
To generate proper [Haddock](https://www.haskell.org/haddock/) documentation,
you should go to the build directory and execute:

```
make doxygen
```

Then you can open the OpenCog documentation (build/doc/html/index.html),
go to:

 "OpenCog source code documentation - Libraries - Haskell bindings library" 
 and click on the "opencog-atomspace" link.

**NOTE:** Documentation can be REALLY useful. There you can see the complete
definition of Haskell bindings, including data types that are automatically generated
by template haskell, and are not available in the code.

### Modifying the code

If you are a developer working on this code, 
when you only modify the Haskell code, you can build the library
(working on: <ATOMSPACE_ROOT>/opencog/haskell/) with:
```
 stack build --extra-lib-dirs=/usr/local/lib/opencog
```

### Adding new Atom Types

Because Haskell bindings does type checking on atoms, each atom type must be defined in the code.

Right now, in actual implementation of Haskell bindings, we get some information from the
[atom_types.script](../atomspace/atom_types.script)
file using Template Haskell (this provides the hierarchical information), but you should update some part of
the code providing information of the outgoing set of the new atom type
(which is not provided in the file [atom_types.script](../atomspace/atom_types.script)).

So, the steps to add a new atom type are:

 - Ensure that it is properly included in the file:
   [opencog/atomspace/atom_types.script](../atomspace/atom_types.script)

 - Update the file:
   [opencog/haskell/OpenCog/AtomSpace/Types.hs](./OpenCog/AtomSpace/Types.hs)

   Add a new data type constructor to the GADT "Atom". There, you can find comments with proper information in how to do that.

 - Update the file:
   [opencog/haskell/OpenCog/AtomSpace/Internal.hs](./OpenCog/AtomSpace/Internal.hs)

   Add proper case clauses for this new atom type to the functions "toRaw" and "fromRaw".

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
Then, we can run this programs on a new AtomSpace with the function runOnNewAtomSpace:

```haskell
runOnNewAtomSpace :: AtomSpace a -> IO a
```

It creates a new C++ atomspace behind, does all the computation, and finally
deletes it.

Note that AtomSpace is an instance of the type class: MonadIO.
So, we can lift IO actions inside the monad AtomSpace,
through the use of liftIO. For example:

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

### Multiple AtomSpaces
The simplest option to work in an AtomSpace is to use "runOnNewAtomSpace" as mentioned before. By doing that, you can work freely in the monad AtomSpace and you don't have to specify the atomspace instance everytime, because it is implicit.

But, sometimes, you want to work with multiples atomspaces, or maybe
you want to do interactive evaluation using GHCi.

For that, you can create new AtomSpaces (from a optionally given parent atomspace) with the function:
```haskell
newAtomSpace :: Maybe AtomSpaceObj -> IO AtomSpaceObj
```

To work over an atomspace, you can use the infix operator "<:"
```haskell
(<:) :: AtomSpaceObj -> AtomSpace a -> IO a
```

For example:
```haskell
main :: IO ()
main = do
    parentAS <- newAtomSpace Nothing
    childAS  <- newAtomSpace (Just parentAS)
    
    parentAS <: insert (ConceptNode "GenConcept" noTv)
    childAS  <: do
        insert (ConceptNode "PrivateConcept1" (stv 1 1))
        insert (ConceptNode "PrivateConcept2" (stv 0.5 1))
    parentAS <: program
    childAS  <: debug
    parentAS <: remove (ConceptNode "GenConcept" noTv)

program :: AtomSpace ()
program = do
    s <- get (ConceptNode "GenConcept" noTv)
    ...
```

### Main functions

#### insert
Function to insert atoms to the atomspace. To create new atoms or just to
update the mutable information of a specific atom.
```haskell
insert :: Atom a -> AtomSpace ()
```
#### get
Function to get an atom back from the atomspace (With updated mutable information).
```haskell
get :: Atom a -> AtomSpace (Maybe (Atom a))
```
#### remove
Function to remove atoms from the atomspace.
```haskell
remove :: Atom a -> AtomSpace Bool
```
#### printAtom
Function to show the given atom in opencog notation.
```haskell
printAtom :: Atom a -> IO ()
```
#### debug
Debug function to print the state of the atomspace on stderr.
```haskell
debug :: AtomSpace ()
```
#### cogBind
Function to use the Pattern Matcher.
(Note: Before using it, you should insert the bind link to the atomspace).
```haskell
cogBind :: Atom BindT -> AtomSpace (Maybe AtomGen)
```
