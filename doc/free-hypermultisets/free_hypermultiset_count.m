function count = free_hypermultiset_count(membranes)
%FREE_HYPERMULTISET_COUNT Count free hypermultisets for one membrane bound.
%   Accepts bounds from 0 through 30; larger bounds are rejected.
if nargin ~= 1 || ~isnumeric(membranes) || ~isreal(membranes) || ...
        ~isscalar(membranes) || ~isfinite(membranes) || membranes < 0 || ...
        membranes ~= floor(membranes)
    error('free_hypermultiset_count:InvalidBound', ...
        'membranes must be a non-negative integer scalar.');
end
counts = free_hypermultiset_counts(membranes);
count = counts(end);
end
