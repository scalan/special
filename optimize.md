### Optimization opportunities

1. Avoid caching of elements for non-generic types (segmentElement)

2. Avoid creating Fold nodes for CostedOption.get,isDefined (remove CostedFoldExtractors.IsGet)