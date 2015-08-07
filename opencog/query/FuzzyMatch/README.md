### Fuzzy Pattern Matcher
The fuzzy pattern matcher can be used to search patterns in the atomspace that are similar to the input pattern. Because of this, the input pattern does not necessarily contain any variables since "variable grounding" is not obvious for hypergraphs that may be very different in terms of their contents and structures.

#### Algorithm
The algorithm starts by creating a `SatisfactionLink` for the input pattern and feeds it to the Pattern Matcher to find potential solutions.

The Pattern Matcher will then perform a node-neighbor search, which is implemented in `FuzzyPatternMatchCB::initiate_search()`. It begins with a method, `FuzzyPatternMatchCB::find_starters()`, that aims to find starters from the input pattern. Each of the starters has to be a Node but not a VariableNode nor an instance node. The neighbors (incoming sets) of each of the starters found will then be explored in the neighbor search and see if there are any potential solutions. The "thinnest" starter, which has the smallest incoming set size will be selected first for performance consideration.

In order to find patterns in the atomspace that are similar but not identical to the input pattern, callbacks in the `FuzzyPatternMatchCB` (i.e. `link_match()`, `node_match()`, `fuzzy_match()`, and `grounding()`) always return true by default in order to accept the matches and keep the maching process going. `FuzzyPatternMatchCB::check_if_accept()` will be called on the way to decide whether or not to accept the potential solution when one is found by the Pattern Matcher.

Once all the neighbors of that starter has been explored, another neighbor search will be initiated by using a different starter. It ends until either the neighbors of all the starters has been explored. If no solution is found after all, `FuzzyPatternMatchCB::initiate_search()` will return false and the Pattern Matcher will then use different approaches (e.g. `link_type_search()`, `variable_search()` etc. as defined in DefaultPatternMatchCB) in order to find solutions.

The final solutions will be returned as a list when all searches are completed.

#### Usage
The fuzzy pattern matcher can be called in these ways:
- In C++, call `find_approximate_match()`, as implemented in FuzzyPatternMatch.
- In Scheme, call `(cog-fuzzy-match ...)`, which will call the `find_approximate_match()` in FuzzyPatternMatch.

One possible application of this is to find similar sentences in the atomspace, which might potentially be useful for natural language question-answering if the system couldn't yield a decent answer in some situation. Detail implementation of sentence matching can be found:
https://github.com/opencog/atomspace/blob/master/opencog/query/sentence-matching.scm
