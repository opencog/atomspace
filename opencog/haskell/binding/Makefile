
PATH_TO_OPENCOG=../../../

gen_lib:
	rm AtomSpace_CWrapper.o libatoms.a -f
	g++ -c -std=c++11 AtomSpace_CWrapper.cpp -o AtomSpace_CWrapper.o -I $(PATH_TO_OPENCOG) -I $(PATH_TO_OPENCOG)/build/
	ar q libatoms.a AtomSpace_CWrapper.o

all: gen_lib
	rm example -f 
	ghc --make -o example -L. -latoms -L$(PATH_TO_OPENCOG)/build/opencog/atomspace -latomspace -lstdc++ example.hs

