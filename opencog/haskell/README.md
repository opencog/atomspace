
Haskell Bindings for OpenCog:
============================

Unfortunately, there isn't any appropiate tool for calling C++ functions, 
and creating/deleting objects from Haskell([1][2][3]). So, the general approach
is to wrap the C++ code to C, and use the FFI (Foreign Function Interface)[4]
to call the resulting C functions from Haskell.

Going that way, I developed a simple initial example interface
for the atomspace with some functions to create/delete a new atomspace 
and add new nodes.

The main idea is to build programs that work on an AtomSpace on the Monad 'AtomSpace'.
Then, we can run this programs with the function runOnNewAtomSpace, that crates a new
C++ atomspace behind, does all the computation, and finally deletes it.

In the future, when developing a Haskell shell plugin for CogServer (using GHCI), I plan to offer a similar
function runOnCogServerAtomSpace that will enable us to run programs on the specific
instance of the AtomSpace running on the CogServer. This seems to be a simple task, something like

    runOnCogServerAtomSpace :: AtomSpace a -> IO a
    runOnCogServerAtomSpace m = run m cogServerAtomSpaceRef

Where cogServerAtomSpaceRef is a reference to the actual instance of the AtomSpace in the CogServer,
that I will provide to GHCi. (something like: AtomSpaceRef pointerToAtomSpaceObjectInMemory)

We have to use the IO monad because of the use of FFI for calling c functions for working
on a mutable instance of the atomspace, so we have side effects.

I read old discussions on this topic suggesting the use of the monad State.

"
The Haskell type State describes functions that consume a state
and produce a tuple that contains a result along with the
new state after the result has been extracted.

    newtype State s a = State { runState :: s -> (a, s) }

Here, s is the type of the state, and a the type of the produced result.
" from [5]

As we are working with a mutable object outside Haskell pure environment,
our state is represented by the monad IO, so we will use
a "run" function a little different from "runState".

    data AtomSpace a = AtomSpace {run :: AtomSpaceRef -> IO a}

(maybe I should call it "AtomSpaceState" to be more evident)

Our run function will return the state wrapped in the monad IO,
I mean the state of the atomspace object in memory outside.

We can not get rid of the monad IO because we can not completely
represent the state of the atomspace in Haskell.

On the other hand, if we want to work purely, without maintaining a state
outside the Haskell pure environment, we should re implement the AtomSpace
completely in Haskell. Then, we could use a pure monad State without IO.

To try example.hs, I assume a "build" directory exists, and that the library  
libatomspace.so was compiled on the folder: build/opencog/atomspace/
So, to compile, and run the example:

    make all
    export LD_LIBRARY_PATH=.
    ./example

So, I think it won't be so difficult to complete a global binding including
all the functionalities of Opencog.
Also, I think directly accessing c code will be faster than translating into Scheme
and then being interpreted.

[1] http://www.reddit.com/r/haskell/comments/nkb74/question_state_of_the_art_in_c_bindings/

[2] https://wiki.haskell.org/CPlusPlus_from_Haskell

[3] https://codeflow.wordpress.com/2011/01/03/wrapping-c-libraries-for-haskell/

[4] https://wiki.haskell.org/FFI_Introduction

[5] http://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
