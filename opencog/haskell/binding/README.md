
Haskell Bindings for OpenCog:
============================

Unfortunately, there isn't any appropiate tool for calling C++ functions, 
and creating/deleting objects from Haskell([1][2][3]). So, the general approach
is to wrap the C++ code to C, and use the FFI (Foreign Function Interface)[4]
to call the resulting C functions from Haskell.

Going that way, I developed a simple initial example interface
for the atomspace with some functions to create/delete a new atomspace 
and add new nodes.

To try example.hs, I assume a "build" directory exists, and that the library  
libatomspace.so was compiled on the folder: build/opencog/atomspace/
So, to compile, and run the example:

    make all
    export LD_LIBRARY_PATH=../../../build/opencog/atomspace/
    ./example

So, I think it won't be so difficult to complete a global binding including
all the functionalities of Opencog.
Also, I think directly accessing c code will be faster than translating into Scheme
and then being interpreted.

[1] http://www.reddit.com/r/haskell/comments/nkb74/question_state_of_the_art_in_c_bindings/
[2] https://wiki.haskell.org/CPlusPlus_from_Haskell
[3] https://codeflow.wordpress.com/2011/01/03/wrapping-c-libraries-for-haskell/
[4] https://wiki.haskell.org/FFI_Introduction

