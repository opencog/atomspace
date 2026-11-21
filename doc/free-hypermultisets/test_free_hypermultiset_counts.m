function test_free_hypermultiset_counts()
%TEST_FREE_HYPERMULTISET_COUNTS Check exact counts and bound rejection.
expected = [1 1 2 4 9 20 48 115 286 719 1842 4766 12486];
assert(isequal(free_hypermultiset_counts(12), expected));
assert(free_hypermultiset_count(30) == 997171512998);
assert(isequal(free_hypermultiset_counts(uint64(12)), expected));

for bound = [31 38 Inf NaN -1 0.5]
    rejected = false;
    try
        free_hypermultiset_count(bound);
    catch err
        rejected = strcmp(err.identifier, 'free_hypermultiset_count:InvalidBound') || ...
            strcmp(err.identifier, 'free_hypermultiset_counts:InexactBound');
    end
    assert(rejected);
end
end
