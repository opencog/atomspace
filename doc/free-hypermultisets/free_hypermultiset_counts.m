function counts = free_hypermultiset_counts(maxMembranes)
%FREE_HYPERMULTISET_COUNTS Count finite free hypermultisets.
%   COUNTS(m+1) is the number of free hypermultisets with m non-skin
%   membranes. Free hypermultisets are equivalent to unlabeled unordered
%   rooted trees, so this returns OEIS A000081 shifted by one node.
%
%   The implementation uses Otter's recurrence:
%     n*T(n) = sum_{k=1}^{n-1} T(n-k) * sum_{d|k} d*T(d)
%   where COUNTS(m+1) = T(m+1).

if nargin ~= 1 || ~isscalar(maxMembranes) || maxMembranes < 0 || ...
        maxMembranes ~= floor(maxMembranes)
    error('free_hypermultiset_counts:InvalidBound', ...
        'maxMembranes must be a non-negative integer scalar.');
end

maxNodes = maxMembranes + 1;
treeCounts = zeros(1, maxNodes + 1);
divisorSums = zeros(1, maxNodes + 1);
treeCounts(2) = 1; % T(1)
divisorSums(2:end) = 1;

for nodes = 2:maxNodes
    total = 0;
    for part = 1:(nodes - 1)
        total = total + treeCounts(nodes - part + 1) * divisorSums(part + 1);
    end
    treeCounts(nodes + 1) = total / (nodes - 1);

    for multiple = nodes:nodes:maxNodes
        divisorSums(multiple + 1) = divisorSums(multiple + 1) + ...
            nodes * treeCounts(nodes + 1);
    end
end

counts = treeCounts(2:end);
end
