### Optimization opportunities

1. Avoid caching of elements for non-generic types (segmentElement)

2. Avoid creating Fold nodes for CostedOption.get,isDefined (remove CostedFoldExtractors.IsGet)

3. Use Slicing to optimize dataSize computations out of cost: (Int, Long) => Int function.
   This make sense when sizes are not used in cost computation.