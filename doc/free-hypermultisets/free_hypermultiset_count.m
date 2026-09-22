function count = free_hypermultiset_count(membranes)
%FREE_HYPERMULTISET_COUNT Count free hypermultisets for one membrane bound.
if nargin ~= 1 || ~isscalar(membranes) || membranes < 0 || ...
        membranes ~= floor(membranes)
    error('free_hypermultiset_count:InvalidBound', ...
        'membranes must be a non-negative integer scalar.');
end
counts = free_hypermultiset_counts(membranes);
count = counts(end);
end
