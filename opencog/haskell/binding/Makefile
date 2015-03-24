
PATH_TO_OPENCOG=../../../

gen_lib:
	rm AtomSpace_CWrapper.o libatoms_wrapper.so -f
	g++ -c -std=c++11 -fPIC AtomSpace_CWrapper.cpp -o AtomSpace_CWrapper.o -I $(PATH_TO_OPENCOG) -I $(PATH_TO_OPENCOG)/build/
	g++ -shared -o libatoms_wrapper.so AtomSpace_CWrapper.o -lc

all: gen_lib
	rm example -f 
	ghc --make -o example -L. -latoms_wrapper -L$(PATH_TO_OPENCOG)/build/opencog/atomspace -latomspace -lstdc++ example.hs

