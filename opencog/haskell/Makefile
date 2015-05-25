
gen_lib:
	rm AtomSpace_CWrapper.o libatoms_wrapper.so -f
	g++ -c -std=c++11 -fPIC AtomSpace_CWrapper.cpp -o AtomSpace_CWrapper.o
	g++ -shared -o libatoms_wrapper.so AtomSpace_CWrapper.o -lc

all: gen_lib
	rm example -f 
	ghc --make -o example -L. -latoms_wrapper -L/usr/local/lib/opencog -latomspace -lstdc++ example.hs

