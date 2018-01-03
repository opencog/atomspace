# OpenCog Benchmarker #

This tool was developed by Joel Pitt, in order to assess the impact of
forthcoming changes to the AtomSpace API (running the AtomSpace as a separate
event loop that fulfils requests). We primarily want to be aware of any
effects in terms of latency, but the benchmarker also has some ability to track
the resident set size (memory) of the benchmark process.

A simple example:

```bash
$ cd <BUILD_DIR>/opencog/benchmark
$ ./atomspace_bm -m "addLink" -k -S 100 -n 10000 -f
```

This will run the addLink benchmark (-m "addLink"). It will also build
a test AtomSpace before running the benchmark which consists of random
graph whose size and link density can be changed on the command line.

It will run and measure the speed of the addLink method 10,000 times
(-n 10000), but between each it will add 100 more random atoms to the
AtomSpace (-S 100) so that you can assess how performance scales with
AtomSpace size. -k calculates some statistics on the run times (min,
max, mean, etc) of the specified method, and -f dumps the records to a
file called addLink_benchmark.csv (the filename used is always the
method name appended by "_benchmark.csv").

You should see output like:

```
OpenCog Atomspace Benchmark - Version 1.0.1

Random generator: MT19937
Random seed: 1454704931

Ingnore this: 32619
Benchmarking AtomSpace's addLink method 10000 times .....
0.226272 seconds elapsed (110486.44 per second)
Sum clock() time for all requests: 184614 (0.184614 seconds, 135418 requests per second)
Per operation stats, in CPU clock ticks:
  N: 5
  mean: 36922
  min: 34017
  max: 42678
  std: 3570
------------------------------
```

Use the -l option to list methods that are implemented for testing, or -A to
run ALL methods sequentially. You can also specify multiple methods by using -m
repeatedly.

The option -? will get more detail.

## A note about memory measurement ##

We just measure changes in the max RSS (resident stack size). This means that
when memory is freed the benchmarker doesn't detect it. For this reason, if you
want (semi)meaningful memory measurements, you should:

- only benchmark a single method at a time
- not enable statistic calculation (-k), as this will be storing all the time
records (you can always output to file with -f and do stats calculations later)

## Graphs ##

There is a script scripts/make_benchmark_graphs.py which will create graphs
from the csv files. You must have matplotlib (Python graphing library)
installed for this to work. If you run the script from a directory with
files ending in "_benchmark.csv" in it, it will create a .png file for
each.

## TODO ##

The memory estimation should be made more informative and reliable. One way to
do this may be to use dmalloc or wrap the process in valgrind and create
a script to process memory calls. Python also has the subprocess module which
could spawn instances of the benchmarker for each method (since the current
memory measurement via max RSS isn't useful when running multiple methods).

A better way might be to record the number of mallocs:
http://www.gnu.org/s/libc/manual/html_node/Hooks-for-Malloc.html



## Python benchmark.py ##

This tool was developed by Cosmo Harrigan and updated by Curtis Faith to
test the performance of the Python bindings and to compare new bindings
against the Scheme scheme_eval and scheme_eval_h which were previously the
only way to access certain OpenCog functionality which has been replaced by
new Cython bindings.

To use the python benchmark, make sure you have your PYTHONPATH set to include
the cython directory:

```bash
$ export PYTHONPATH="${PYTHONPATH}:${HOME}/atomspace/build/opencog/cython"
```

and then run (from the atomspace directory, `/home/opencog/atomspace` on Docker):

```bash
$ python ./opencog/benchmark/benchmark.py
```

or execute directly with:

```bash
$ ./opencog/benchmark/benchmark.py
```

The test will run and produce its output automatically if no options are
specified. The options are available with -h or --help.

```bash
$ python benchmark.py -h
usage: benchmark.py [-h] [-v | -c]
                    [-a | -t {spread,node,bindlink,traverse,scheme}] [-i N]

OpenCog Python Benchmark

optional arguments:
  -h, --help            show this help message and exit
  -v, --verbose         verbose output
  -c, --columns         columnar output (default)
  -a, --all             run all tests
  -t {spread,node,bindlink,traverse,scheme}, --test {spread,node,bindlink,traverse,scheme}
                        Test to benchmark, where:
                          spread    - a spread of tests across areas (default)
                          node      - atomspace node operations
                          bindlink  - all the bindlink forms
                          traverse  - traversal of atomspace lists
                          scheme    - scheme evaluation functions
  -i N, --iterations N  iterations to average (default=10)
```

## Profiling ##

The benchmark directory now includes a sample profiling for the
bindlink function in `profile_bindlink.cc` This file can be used as a
template for profiling other atomspace functions.

### Using perf_events ###
Install:
```
apt-get install linux-tools
```

oprofile


### Using valgrind ###
Build as normal, then use

```
valgrind --tool=callgrind ./opencog/benchmark/profile_bindlink
```
after this completes:
```
kcachegrind callgrind.out.<pid>
```

### Using gprof ###
To use the `gprof` profiler, you will need to build a profile build
of AtomSpace.

The following commands will create a new profile directory and build
the atomspace with the compiler options so that gprof will output the
appropriate profiling function hooks. It will also make the libraries
static and not shared which is required for gprof to profile functions
in the libraries.

From the top level atomspace directory, i.e. the one that already
contains the build directory, execute:

```
mkdir profile
cd profile
cmake -D CMAKE_BUILD_TYPE=Profile ..
make
```

This will build atomspace using the profile options. After the build
finishes there will be a program `profile_bindlink` in the directory:

```
opencog/benchmark/
```

within the profile build directory. Running:

```
./profile_bindlink
```

from that directory will run the executable and after few seconds it
will exit and generate a file:

```
gmon.out
```

Now execute the following command:

```
gprof profile_bindlink gmon.out > analysis.txt
```

This will take the binary profiling information in gmon.out and format
it so that you can read it in a file called `analysis.txt`. Open
`analysis.txt` in a text viewer and you will see the results of the
profiling.

# Experimental #

## atom_bench ##
The atom_bench tool tests some prototype methods for potential Atom implementation that uses RAM pages of a fixed size to store Atoms. It is intended to show potential performance gains which might be useful for an enterprise-class distributed OpenCog. It is not designed for real-world use but rather to model the performance characteristics of a potential new method for storing atoms.

The atom_bench tool can be compiled and run with two modes, if USE_ATOMSPACE is defined true, the code will test the existing atomspace using the same data and  benchmark algorithm. When USE_ATOMSPACE is false, the benchmark implements and benchmarks the speed of a paged-memory implementation.

The atom_bench includes a test which creates 1 million atoms, and one that searches all 1 million atoms for a particular value that is in the data, and one that searches for a value which will not be found to benchmark the speed of traversing links and outgoing sets.

### Motivation ###
Consider the following Atom:

```
EvaluationLink
    PredicateNode "breathe"
    ListLink
        ConceptNode "birds"
        ConceptNode "air"
```
How many bytes do you think this currently takes per Atom?

To understand the answer, let's look at the current implementation, and in particular, the C++ objects required to instantiate an atom of the above form.

![image](http://i.imgur.com/lxy1o4D.png)

This requires:

- 5 Atoms
-- 2 - links at 144 bytes
-- 3 - nodes at 128 bytes

Breaking this down further:

- 2 Links
-- 2 - outgoing set vectors at 16 bytes

- 3 Nodes
-- 3 - strings for names

Inbound Links require:
- 5 - maps of type to sets at 48 bytes
- 5 - type-based sets of inbound links at 48 bytes
- 5 - RB Tree nodes for each at 24 bytes plus size of the key string itself

So we have 25 C++ heap-based objects to represent one simple predicate of knowledge: "birds breathe air." Total memory used for this atom is about 1,500 bytes. This for a very simple "birds breathe air".

How does this impact insert performance? In the above example, if one assumes the PredicateNode and ConceptNodes were already in the atomspace, then the addition would add:

- 2 - Links at 144 bytes

and for Inbound Links

- 2 - maps of type to sets at 48 bytes
- 2 - type-based sets of inbound links at 48 bytes
- 5 - RB Tree nodes for each at 24 bytes plus size of the key string itself

so 11 heap allocations per atom insert.


NOTE: The above does not even include any of the AtomTable index entries for each atom. These entries require more memory.

How about the speed of pattern matching? Consider how many different heap structures need to be traversed to just iterate through the various parts of the atom? Maps of Sets of Handles at every inbound level. Vectors of Handles for outbound. Every level requires a new function call to create the iterators. For the typical small number of elements the overhead of the data structures and various levels of container objects becomes significant.

The predicate statement "birds breathe air" can be represented by a simpler serial encoding like the way that SQL databases represent table rows in B+ or B* trees. The main atom could be stored at the leaves of a B+ tree while there are separate inbound B+ tree indices for inbound links. If done well, inserts would only result in memory management calls when pages overflow. Inbound link indices will be very fast and compact compared to the current implementation.

Consider the following diagram which shows the minimum data required to support efficient inserts, loads and lookup for inbound sets:

![image](http://i.imgur.com/8o023sj.png)

Since, at its core, this predicate example is simply an edge linking the "breathe" predicate with the "birds" and "air" predicate, there should be three entries in the inbound table to facility a search for where "breathe" is used, "birds" are referenced, and "air" is referenced. If we used a mechanism like this, we'd end up with much smaller atoms, and we would have less than one memory allocation per atom insert. It might be as low as an allocation per 100 atom inserts. This compares with a much higher number for our current implementation. 

In the above example, the nodes will already have been created, the B+ tree pages containing the inbound references will likely not exceed a page, and the same for the insert of the statement itself. If one used 4K pages, for example, one might expect to get 100 or more atoms of this size per page. And one would get perhaps 400 to 500 inbound link entries per page, resulting in an average 1 heap allocation per 80 to 100 or so atoms.

Now consider the highest performance serialization suitable for high-speed pattern matching since all of the relevant information is contained in one serial set of entries which will likely be in the same cache line for very high-speed memory access:

![image](http://i.imgur.com/ViQgayz.png)

By flattening out the references to the nodes, a super-atom like this could be easily iterated and compared without having to follow pointers to areas of memory likely to result in cache misses because of poor locality of reference.

Given the above, and the need for thinking about how to serialize atoms into forms suitable for high-speed transmission between server nodes in a distributed AtomSpace, we should at least consider reworking the internals for AtomSpace to fit the performance needs for expected usage patterns.

### Page Based Storage Protopype Experiments ###

Curtis's Notes:

In order to determine how much the Atom's scattered memory data structures is impacting performance to get a feel for the actual improvements we might expect if the format for atoms was changed to use a page-based storage system, I implemented a prototype and benchmark tests which implements some of the above features. These tests provide concrete data that can be used to guide work and prioritize in the future. The tests indicate that gains from these ideas are sufficient to warrant additional work and exploration.

The test code in `atom_bench.cc` and `PagedAtoms.h` and `PagedAtoms.cc` models page based storage. It puts atoms into a serial page form. It adds new pages when pages are full. It does some memory copying to simulate the work required if sorting was done per page, it implements a page-level mutex and locks it while changing, it records edge connections between links and items in the outgoing set in separate edge pages, it keeps several vectors of these pages, as well as a set of index pages which keep references to all the other pages. It models a couple of the access patterns and work that would be performed in a real implementation to study the relative performance when compared with the current implementation: inserts and search / traversals of link and node graphs.

The tests randomly generate atoms for insertions only, the random number and text generation is implemented in the test prep code which is not timed. The tests first create 100,000 ConceptNodes (untimed), and then the timed test creates 1 million EvaluationLink subgraphs which, in turn contain, 1 million references to a PredicateNode and 1 million ListLinks which contains two randomly selected ConceptNodes. So the 1 million atoms below includes the creation of 2 million separate atoms: 1M EvaluationLinks and 1M ListLinks.

For a test of 1 million atoms, time per insert of an atom was:

Current AtomSpace          25.0 μs
Page-based                  1.3 μs

Total resident memory at the end of the test:

Current AtomSpace                     1.397 G
Page-based                            0.235 G

The new approach is about 20 times faster with 1/6 the memory footprint. To account for changes that will likely come with a real implementation, an addition 50% should be added to the memory footprint to account for non-full pages. Additional work might also double the time per operation to conservatively account for work missed and some non-trivial work like performing binary searches on each page, and referencing the pages in the index entries which is not implemented in this prototype. 

These tests indicate we'd likely be able to get insert performance of 2.5 μs and a memory footprint of 350 bytes per Atom versus the current 25 μs and 1400 bytes per atom if we changed the AtomSpace implementation to a paged model.

### Greater concurrency ###
One further factor which could be model tested in a few days more work, is that having locks on each page should allow for much greater concurrency for multi-threaded access. The data structure itself can be modified to allow high-concurrency multi-threading. The granularity of a page is much finer than an entire index. So only the page where the new atom would reside needs to be locked for an insert. Once an atom is created, edge index entries can be added one by one, and again each of these inserts need only lock the affected pages. Other threads can read and update other pages without affecting a particular insert, and vice versa.

### Issues with this approach ###
The paged-based atom code does not use shared_ptrs. An enterprise-class distributed AtomSpace can still wrap the internal Atoms allocated within pages and return shared_ptrs through the API. However, an approach which does not delete Atoms unless they are explicitly removed is likely a better approach.

